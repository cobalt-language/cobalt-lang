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
    MietteDiagnostic {
        labels: Some(vec![LabeledSpan::underline(err.span().into_range())]),
        ..MietteDiagnostic::new(err.to_string())
    }
}
fn add_loc<T>(val: T, loc: SimpleSpan) -> (T, SourceSpan) {(val, loc.into_range().into())}
// useful type definitions
type Extras<'a> = chumsky::extra::Full<Rich<'a, char>, (), ()>;
type BoxedParser<'a, 'b, T> = Boxed<'a, 'b, &'a str, T, Extras<'a>>;
type BoxedASTParser<'a, 'b> = BoxedParser<'a, 'b, Box<dyn AST>>;
/// parse an identifier. unicode-aware
fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, Extras<'a>> + Copy {any().filter(|&c| is_xid_start(c)).then(any().filter(|&c| is_xid_continue(c)).repeated()).slice().labelled("identifier")}
fn local_id<'a>() -> impl Parser<'a, &'a str, DottedName, Extras<'a>> + Copy {ident().map_with_span(|id, loc| DottedName::local((id.to_string(), loc.into_range().into()))).labelled("local identifier")}
fn global_id<'a>() -> impl Parser<'a, &'a str, DottedName, Extras<'a>> + Copy {
    just('.').or_not().map(|o| o.is_some())
        .then(ident().map_with_span(|id, loc| (id.to_string(), loc.into_range().into())).separated_by(just('.')).collect())
        .map(|(global, ids)| DottedName::new(ids, global))
        .labelled("global identifier")
}
fn ignored<'a>() -> impl Parser<'a, &'a str, (), Extras<'a>> + Copy {
    choice((
        // whitespace
        any().filter(|c: &char| c.is_whitespace()).ignored(),
        // multiline comment
        just('#').ignore_then(just('=').repeated().count().then_ignore(any().repeated()).then_with_ctx(just('=').repeated().configure(|cfg, &ctx| cfg.exactly(ctx))).then_ignore(just('#'))).ignored(),
        // single-line comment
        just('#').then_ignore(any().repeated()).ignore_then(end().or(text::newline()))
    )).repeated().ignored()
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
/// parse declarations. these are:
/// - functions
/// - variables
/// - type definitions
fn declarations<'a>(loc: DeclLoc, metd: Option<BoxedASTParser<'a, '_>>) -> impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone {
    let metd = metd.unwrap_or_else(|| recursive(|m| declarations(DeclLoc::Method, Some(m.boxed()))).boxed());
    todo()
}
/// create a parser for a statement.
/// it requires an expression parser to passed, otherwise it would require infinite recursion
fn def_stmt<'a>(expr: impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone) -> impl Parser<'a, &'a str, Box<dyn AST>, Extras<'a>> + Clone {
    expr
}
/// create a parser for expressions
pub fn parse_expr<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    todo().boxed()
}
/// create a parser for statements
pub fn parse_stmt<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    def_stmt(parse_expr()).boxed()
}
/// create a parser for the top-level scope
pub fn parse_tl<'a: 'b, 'b>() -> BoxedParser<'a, 'b, TopLevelAST> {
    todo().boxed()
}