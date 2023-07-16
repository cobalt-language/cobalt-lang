#![allow(unused_imports, dead_code, unused_variables)] // TODO: remove before merge
use cobalt_errors::*;
use cobalt_ast::{*, ast::*};
use unicode_ident::*;
use chumsky::prelude::*;
use cobalt_errors::miette::{MietteDiagnostic, LabeledSpan, SourceSpan};
mod utils;
pub mod prelude {
    pub use super::parse_expr;
    pub use super::parse_tl;
    pub use super::cvt_err;
    pub use chumsky::Parser as _;
}
/// make chumsky errors pretty
pub fn cvt_err(err: Rich<char>) -> MietteDiagnostic {
    let mut msg = err.to_string();
    if let Some(idx) = msg.find(" expected") {msg.insert(idx, ',')} // the lack of the comma was bothering me
    MietteDiagnostic::new(msg)
        .with_labels(
            std::iter::once(LabeledSpan::underline(err.span().into_range()))
            .chain(err.contexts().map(|(&label, span)| LabeledSpan::at(span.into_range(), label))))
}
/// for use with map_with_span
#[inline(always)]
fn add_loc<T>(val: T, loc: SimpleSpan) -> (T, SourceSpan) {(val, loc.into_range().into())}
/// wrapper around Box::new that makes the output dyn
#[inline(always)]
fn box_ast<T: AST + 'static>(val: T) -> Box<dyn AST> {Box::new(val)}
// useful type definitions
type Extras<'a> = chumsky::extra::Full<Rich<'a, char>, (), ()>;
type BoxedParser<'a, 'b, T> = Boxed<'a, 'b, &'a str, T, Extras<'a>>;
type BoxedASTParser<'a, 'b> = BoxedParser<'a, 'b, Box<dyn AST>>;
static KEYWORDS: &[&str] = &[
    "let", "mut", "const", "fn",
    "if", "while", "for",
    "null", "type"
];
/// parse an identifier. unicode-aware
fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, Extras<'a>> + Copy {
    any()
        .filter(|&c| is_xid_start(c))
        .then(any().filter(|&c| is_xid_continue(c)).repeated())
        .slice()
        .try_map(|val, span| if KEYWORDS.contains(&val) {Err(Rich::custom(span, format!("`{val}` is a keyword and cannot be used as an identifier")))} else {Ok(val)})
        .labelled("an identifier")
}
fn local_id<'a>() -> impl Parser<'a, &'a str, DottedName, Extras<'a>> + Copy {ident().map_with_span(|id, loc| DottedName::local((id.to_string(), loc.into_range().into()))).labelled("a local identifier")}
fn global_id<'a>() -> impl Parser<'a, &'a str, DottedName, Extras<'a>> + Copy {
    just('.').or_not().map(|o| o.is_some())
        .then(ident().map_with_span(|id, loc| (id.to_string(), loc.into_range().into())).separated_by(just('.')).at_least(1).collect())
        .map(|(global, ids)| DottedName::new(ids, global))
        .labelled("a global identifier")
}
fn ignored<'a>() -> impl Parser<'a, &'a str, (), Extras<'a>> + Copy {
    choice((
        // whitespace
        any().filter(|c: &char| c.is_whitespace()).ignored(),
        // multiline comment
        just('#').ignore_then(just('=').repeated().count().then_ignore(any().repeated()).then_with_ctx(just('=').repeated().configure(|cfg, &ctx| cfg.exactly(ctx))).then_ignore(just('#'))).ignored(),
        // single-line comment
        just('#').then_ignore(any().repeated()).ignore_then(end().or(text::newline()))
    )).repeated().ignored().labelled("whitespace")
}
fn padding<'a>() -> impl Parser<'a, &'a str, (), Extras<'a>> + Copy {
    choice((
        // whitespace
        any().filter(|c: &char| c.is_whitespace()).ignored(),
        // multiline comment
        just('#').ignore_then(just('=').repeated().count().then_ignore(any().repeated()).then_with_ctx(just('=').repeated().configure(|cfg, &ctx| cfg.exactly(ctx))).then_ignore(just('#'))).ignored(),
        // single-line comment
        just('#').then_ignore(any().repeated()).ignore_then(end().or(text::newline()))
    )).repeated().at_least(1).ignored().labelled("whitespace")
}
/// where a declaration is being parsed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclLoc {
    /// local scope- inside a block
    Local,
    /// method scope- in the method block of a type definition
    Method,
    /// global scope- do I need to explain this?
    Global
}
fn annotation<'a>() -> impl Parser<'a, &'a str, (String, Option<String>, SourceSpan), Extras<'a>> + Copy {
    just('@')
        .ignore_then(ident())
        .then(none_of(')').repeated().collect().delimited_by(just('('), just(')')).or_not())
        .map_with_span(|(name, arg), loc| (name.to_string(), arg, loc.into_range().into()))
        .labelled("an annotation")
}
/// parse declarations. these are:
/// - functions
/// - variables
/// - type definitions
fn declarations<'a>(loc: DeclLoc, metd: Option<BoxedASTParser<'a, 'a>>, expr: &(impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone + 'a)) -> impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone + 'a {
    let expr_clone = expr.clone();
    let metd = metd.unwrap_or_else(|| recursive(move |m| declarations(DeclLoc::Method, Some(m.boxed()), &expr_clone)).boxed());
    let id = if loc == DeclLoc::Global {global_id().boxed()} else {local_id().boxed()}; // TODO: remove allocation
    let anns = annotation().padded_by(ignored()).repeated().collect();
    choice((
        anns.then(text::keyword("let").map_with_span(|_, loc: SimpleSpan| loc.into_range().into())).then_ignore(ignored())
            .then(text::keyword("mut").ignore_then(ignored()).or_not().map(|o| o.is_some()))
            .then(
                id.clone()
                .recover_with(skip_then_retry_until(any().filter(|&c| is_xid_continue(c)).repeated().at_least(1).ignored().or(any().ignored()), one_of(":=;").ignored()))
                .recover_with(skip_until(any().ignored(), one_of(":=;").rewind().ignored().or(end()), || DottedName::local(("<error>".to_string(), unreachable_span()))))
                .then_ignore(ignored()))
            .then(just(':').ignore_then(ignored()).ignore_then(expr.clone()).or_not())
            .then(just('=').ignore_then(ignored()).ignore_then(expr.clone()).or_not().map_with_span(|expr, loc| expr.unwrap_or(box_ast(NullAST::new(loc.into_range().into())))))
            .map(move |(((((anns, l), is_mut), name), ty), val)| box_ast(VarDefAST::new(l, name, val, ty, anns, loc == DeclLoc::Global, is_mut))),
        anns.then(text::keyword("const").map_with_span(|_, loc: SimpleSpan| loc.into_range().into())).then_ignore(ignored())
            .then(
                id.clone()
                .recover_with(skip_then_retry_until(any().filter(|&c| is_xid_continue(c)).repeated().at_least(1).ignored().or(any().ignored()), one_of(":=;").ignored()))
                .recover_with(skip_until(any().ignored(), one_of(":=;").rewind().ignored().or(end()), || DottedName::local(("<error>".to_string(), unreachable_span()))))
                .then_ignore(ignored()))
            .then(just(':').ignore_then(ignored()).ignore_then(expr.clone()).or_not())
            .then(just('=').ignore_then(ignored()).ignore_then(expr.clone()).or_not().map_with_span(|expr, loc| expr.unwrap_or(box_ast(NullAST::new(loc.into_range().into())))))
            .map(move |((((anns, l), name), ty), val)| box_ast(ConstDefAST::new(l, name, val, ty, anns))),
    )).labelled("a declaration")
}
/// create a parser for a statement.
/// it requires an expression parser to passed, otherwise it would require infinite recursion
fn def_stmt<'a>(expr: impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone + 'a) -> impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone + 'a {
    declarations(DeclLoc::Local, None, &expr).or(expr)
}
/// create a parser for expressions
pub fn parse_expr<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    // currently only identifiers
    ident().map_with_span(|v, loc| box_ast(VarGetAST::new(loc.into_range().into(), v.to_string(), false))).boxed()
}
/// create a parser for statements
pub fn parse_stmt<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    def_stmt(parse_expr()).boxed()
}
/// create a parser for the top-level scope
pub fn parse_tl<'a: 'b, 'b>() -> BoxedParser<'a, 'b, TopLevelAST> {
    ignored()
        .ignore_then(declarations(DeclLoc::Global, None, &parse_expr()))
        .then_ignore(just(';').padded_by(ignored()))
    .repeated().collect().map(TopLevelAST::new).then_ignore(end()).boxed()
}