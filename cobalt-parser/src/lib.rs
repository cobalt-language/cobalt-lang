pub mod lexer;
pub mod parser;

use chumsky::{error::RichReason, prelude::*};
use cobalt_ast::{ast::*, *};
use cobalt_errors::miette::{LabeledSpan, MietteDiagnostic, SourceSpan};
use cobalt_errors::*;
use std::borrow::Cow;
use std::collections::HashMap;
use unicode_ident::*;
mod utils;
pub mod prelude {
    pub use super::cvt_err;
    pub use super::parse_expr;
    pub use super::parse_tl;
    pub use chumsky::Parser as _;
}
fn cvt_reason(span: SimpleSpan, err: RichReason<'_, char, &'static str>) -> Vec<MietteDiagnostic> {
    if let RichReason::Many(mut errs) = err {
        errs.sort();
        errs.dedup();
        errs.into_iter().flat_map(|e| cvt_reason(span, e)).collect()
    } else {
        let mut msg = err.to_string();
        if let Some(idx) = msg.find(" expected") {
            msg.insert(idx, ',')
        } // the lack of the comma was bothering me
        vec![MietteDiagnostic::new(msg).with_label(LabeledSpan::underline(span.into_range()))]
    }
}
/// make chumsky errors pretty
pub fn cvt_err(err: Rich<char>) -> Vec<MietteDiagnostic> {
    let span = *err.span();
    cvt_reason(span, err.into_reason())
}
/// for use with map_with_span
#[inline(always)]
fn add_loc<T>(val: T, loc: SimpleSpan) -> (T, SourceSpan) {
    (val, loc.into_range().into())
}
/// wrapper around Box::new that makes the output dyn
#[inline(always)]
fn box_ast<'src, T: AST<'src> + 'src>(val: T) -> BoxedAST<'src> {
    Box::new(val)
}
// useful type definitions
type Extras<'a> = chumsky::extra::Full<Rich<'a, char>, Vec<HashMap<&'a str, SimpleSpan>>, ()>;
type BoxedParser<'a, 'b, T> = Boxed<'a, 'b, &'a str, T, Extras<'a>>;
type BoxedASTParser<'a, 'b> = BoxedParser<'a, 'b, BoxedAST<'a>>;
static KEYWORDS: &[&str] = &[
    "let", "mut", "const", "fn", "if", "else", "while", "for", "null", "type",
];
/// operators that can be interned
static OPS: &[&str] = &[
    "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "+", "-", "*", "/", "%",
    "&", "|", "^", "<<", ">>", "<", ">", "<=", ">=", "==", "!=", "mut", "++", "--", "!", "&?",
    "|?",
];
/// take op and make it 'static
/// assumes op is an operator
fn intern_op(op: &str) -> &'static str {
    if let Some(op) = OPS.iter().find(|&&o| o == op) {
        op
    } else {
        unreachable!("expected an operator, found {op:?}")
    }
}
/// same as intern_op, but take a char instead of a &str
fn intern_char_op(op: char) -> &'static str {
    if let Some(op) = OPS
        .iter()
        .find(|o| o.len() == 1 && o.chars().next().unwrap() == op)
    {
        op
    } else {
        unreachable!("expected an operator, found {op:?}")
    }
}

/// parse an identifier. unicode-aware
fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, Extras<'a>> + Copy {
    // First parse it, then check if it's a keyword.
    //
    // - 1: Accept any character which is an '_' or xid start.
    // - 2: Then accept any number of xid_continue, storing them in a `Vec`.
    // - 3: We want the output of this parser to be a slice.
    any()
        .filter(|&c| c == '_' || is_xid_start(c)) // 1
        .then(any().filter(|&c| is_xid_continue(c)).repeated()) // 2
        .slice() // 3
        .try_map(|val, span| {
            if KEYWORDS.contains(&val) {
                Err(Rich::custom(
                    span,
                    format!("`{val}` is a keyword and cannot be used as an identifier"),
                ))
            } else {
                Ok(val)
            }
        })
        .labelled("an identifier")
}

fn local_id<'a>() -> impl Parser<'a, &'a str, DottedName<'a>, Extras<'a>> + Copy {
    ident()
        .map_with_span(|id, loc| DottedName::local((id.into(), loc.into_range().into())))
        .labelled("a local identifier")
}
fn global_id<'a>() -> impl Parser<'a, &'a str, DottedName<'a>, Extras<'a>> + Copy {
    just('.')
        .or_not()
        .map(|o| o.is_some())
        .then(
            ident()
                .map_with_span(|id, loc| (id.into(), loc.into_range().into()))
                .separated_by(just('.'))
                .at_least(1)
                .collect(),
        )
        .map(|(global, ids)| DottedName::new(ids, global))
        .labelled("a global identifier")
}

/// Parses (well, ignores) patterns that Cobalt should ignore, such as whitespace and comments.
fn ignored<'a>() -> impl Parser<'a, &'a str, (), Extras<'a>> + Copy {
    // Consume any number of either
    // - whitespace
    // - multiline comments
    // - single-line comments
    choice((
        // whitespace
        any().filter(|c: &char| c.is_whitespace()).ignored(),
        // multiline comment
        just('#').ignore_then(just('=').repeated().at_least(1).count().then_with_ctx({
            let term = just('=')
                .repeated()
                .configure(|cfg, &ctx| cfg.exactly(ctx))
                .then_ignore(just('#'));
            any().and_is(term.not()).repeated().then_ignore(term)
        })),
        // single-line comment
        just('#').ignore_then(any().and_is(text::newline().not()).repeated()),
    ))
    .repeated()
    .ignored()
    .labelled("whitespace")
}

/// where a declaration is being parsed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclLoc {
    /// local scope- inside a block
    Local,
    /// method scope- in the method block of a type definition
    Method,
    /// global scope- do I need to explain this?
    Global,
}

/// Parser for an annotation, e.g. `@C(extern)`.
///
/// The output is (annotation name, annotation argument, source span), e.g. for
/// `@C(extern)` the annotation name is "C" and the annotation argument is "extern".
fn annotation<'a>(
) -> impl Parser<'a, &'a str, (Cow<'a, str>, Option<Cow<'a, str>>, SourceSpan), Extras<'a>> + Copy {
    // - 1: Accept an `@` (but don't store it), then an identifier (so no keywords).
    // - 2: The argument can be any sequence of characters that does not include ')' and is
    // surrounded by parentheses.
    // - 3: Since not every annotation has an argument, don't fail if there isn't one.
    just('@')
        .ignore_then(ident()) // 1
        .then(
            none_of(')')
                .repeated()
                .map_slice(Cow::Borrowed)
                .delimited_by(just('('), just(')')) // 2
                .or_not(), // 3
        )
        .map_with_span(|(name, arg), loc| (name.into(), arg, loc.into_range().into()))
        .labelled("an annotation")
}

fn cdn<'a>() -> impl Parser<'a, &'a str, CompoundDottedName<'a>, Extras<'a>> + Clone {
    use CompoundDottedNameSegment::*;
    let cdns = recursive(|cdns| {
        choice((
            ident().map_with_span(|id, loc| Identifier(id.into(), loc.into_range().into())),
            just('*').map_with_span(|_, loc: SimpleSpan| Glob(loc.into_range().into())),
            cdns.separated_by(just('.').padded_by(ignored()).ignored().recover_with(
                skip_then_retry_until(none_of(".,};").ignored(), one_of(".,};").ignored()),
            ))
            .collect()
            .separated_by(just(',').padded_by(ignored()).ignored().recover_with(
                skip_then_retry_until(none_of(",};").ignored(), one_of(",};").ignored()),
            ))
            .collect()
            .delimited_by(just('{'), just('}'))
            .map(Group),
        ))
    })
    .labelled("an import segment");
    just('.')
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(ignored())
        .then(
            cdns.separated_by(just('.').padded_by(ignored()).ignored().recover_with(
                skip_then_retry_until(none_of(".,};").ignored(), one_of(".,};").ignored()),
            ))
            .at_least(1)
            .collect(),
        )
        .map(|(global, ids)| CompoundDottedName::new(ids, global))
        .labelled("an import path")
}
fn import<'a>() -> impl Parser<'a, &'a str, ImportAST<'a>, Extras<'a>> + Clone {
    let anns = annotation().padded_by(ignored()).repeated().collect();
    anns.then(text::keyword("import").to_span())
        .then_ignore(ignored())
        .then(cdn())
        .map(|((anns, loc), cdn)| ImportAST::new(loc.into_range().into(), cdn, anns))
        .labelled("an import")
}

/// parse declarations. these are:
/// - functions
/// - variables
/// - type definitions
fn declarations<'a>(
    loc: DeclLoc,
    metd: Option<BoxedASTParser<'a, 'a>>,
    part_expr: &BoxedASTParser<'a, 'a>,
) -> impl Parser<'a, &'a str, BoxedAST<'a>, Extras<'a>> + Clone + 'a {
    let full_expr = add_assigns(part_expr.clone());
    let expr_clone = part_expr.clone();
    let metd = metd.unwrap_or_else(|| {
        recursive(move |m| declarations(DeclLoc::Method, Some(m.boxed()), &expr_clone)).boxed()
    });
    let id = if loc == DeclLoc::Global {
        global_id().boxed()
    } else {
        local_id().boxed()
    }; // TODO: remove allocation
    let anns = annotation().padded_by(ignored()).repeated().collect();
    let param = choice((
        text::keyword("mut").to(ParamType::Mutable),
        text::keyword("const").to(ParamType::Constant),
        empty().to(ParamType::Normal),
    ))
    .then_ignore(ignored())
    .then(
        ident()
            .or_not()
            .map_with_span(|v, l| (v.map_or_else(Cow::default, Cow::Borrowed), l))
            .labelled("parameter name"),
    )
    .then_ignore(ignored())
    .then(
        just(':')
            .recover_with(skip_then_retry_until(
                any()
                    .filter(|&c| is_xid_continue(c))
                    .repeated()
                    .at_least(1)
                    .ignored()
                    .or(any().ignored()),
                one_of(":=)").ignored(),
            ))
            .recover_with(skip_until(
                any().ignored(),
                one_of(":,;=)").ignored().or(end()),
                || '-',
            ))
            .ignore_then(ignored())
            .ignore_then(
                part_expr.clone().recover_with(via_parser(
                    none_of(";,=)")
                        .repeated()
                        .map_with_span(|_, loc: SimpleSpan| {
                            box_ast(ErrorTypeAST::new(loc.into_range().into()))
                        }),
                )),
            )
            .labelled("parameter type"),
    )
    .then_ignore(ignored())
    .then(
        just('=')
            .ignore_then(ignored())
            .ignore_then(
                full_expr.clone().recover_with(via_parser(
                    none_of(",)")
                        .repeated()
                        .map_with_span(|_, span: SimpleSpan| {
                            box_ast(ErrorAST::new(span.end.into()))
                        }),
                )),
            )
            .or_not()
            .labelled("default value"),
    )
    .map(|(((pty, (name, loc)), ty), default)| (loc.into_range().into(), name, pty, ty, default))
    .and_is(none_of("),").rewind())
    .boxed();
    choice([
        anns.then(text::keyword("let").map_with_span(|_, loc: SimpleSpan| loc.into_range().into()))
            .then_ignore(ignored())
            .then(
                text::keyword("mut")
                    .ignore_then(ignored())
                    .or_not()
                    .map(|o| o.is_some()),
            )
            .then(
                id.clone()
                    .recover_with(skip_then_retry_until(
                        any()
                            .filter(|&c| is_xid_continue(c))
                            .repeated()
                            .at_least(1)
                            .ignored()
                            .or(any().ignored()),
                        one_of(":=;").ignored(),
                    ))
                    .recover_with(skip_until(
                        any().ignored(),
                        one_of(":=;").rewind().ignored().or(end()),
                        || DottedName::local(("<error>".into(), unreachable_span())),
                    )),
            )
            .then_ignore(ignored())
            .then(
                just(':')
                    .labelled("variable type")
                    .ignore_then(ignored())
                    .ignore_then(part_expr.clone().recover_with(
                        via_parser(none_of(",=;").repeated().map_with_span(
                            |_, span: SimpleSpan| box_ast(ErrorTypeAST::new(span.end.into())),
                        )),
                    ))
                    .or_not(),
            )
            .then_ignore(ignored())
            .then(
                just('=')
                    .labelled("variable value")
                    .ignore_then(ignored())
                    .ignore_then(full_expr.clone().recover_with(
                        via_parser(none_of(",;").repeated().map_with_span(
                            |_, span: SimpleSpan| box_ast(ErrorAST::new(span.end.into())),
                        )),
                    ))
                    .or_not()
                    .map_with_span(|expr, loc| {
                        expr.unwrap_or_else(|| box_ast(NullAST::new(loc.into_range().into())))
                    }),
            )
            .map(move |(((((anns, l), is_mut), name), ty), val)| {
                box_ast(VarDefAST::new(
                    l,
                    name,
                    val,
                    ty,
                    anns,
                    loc == DeclLoc::Global,
                    is_mut,
                ))
            })
            .boxed(),
        anns.then(
            text::keyword("const").map_with_span(|_, loc: SimpleSpan| loc.into_range().into()),
        )
        .then_ignore(ignored())
        .then(
            id.clone()
                .recover_with(skip_then_retry_until(
                    any()
                        .filter(|&c| is_xid_continue(c))
                        .repeated()
                        .at_least(1)
                        .ignored()
                        .or(any().ignored()),
                    one_of(":=;").ignored(),
                ))
                .recover_with(skip_until(
                    any().ignored(),
                    one_of(":=;").rewind().ignored().or(end()),
                    || DottedName::local(("<error>".into(), unreachable_span())),
                )),
        )
        .then_ignore(ignored())
        .then(
            just(':')
                .labelled("variable type")
                .ignore_then(ignored())
                .ignore_then(
                    part_expr.clone().recover_with(via_parser(
                        none_of(",=;")
                            .repeated()
                            .map_with_span(|_, span: SimpleSpan| {
                                box_ast(ErrorTypeAST::new(span.end.into()))
                            }),
                    )),
                )
                .or_not(),
        )
        .then_ignore(ignored())
        .then(
            just('=')
                .labelled("variable value")
                .ignore_then(ignored())
                .ignore_then(
                    full_expr.clone().recover_with(via_parser(
                        none_of(",;")
                            .repeated()
                            .map_with_span(|_, span: SimpleSpan| {
                                box_ast(ErrorAST::new(span.end.into()))
                            }),
                    )),
                )
                .or_not()
                .map_with_span(|expr, loc| {
                    expr.unwrap_or_else(|| box_ast(NullAST::new(loc.into_range().into())))
                }),
        )
        .map(move |((((anns, l), name), ty), val)| {
            box_ast(ConstDefAST::new(l, name, val, ty, anns))
        })
        .boxed(),
        anns.then(
            text::keyword("type").map_with_span(|_, loc: SimpleSpan| loc.into_range().into()),
        )
        .then_ignore(ignored())
        .then(
            id.clone()
                .recover_with(skip_then_retry_until(
                    any()
                        .filter(|&c| is_xid_continue(c))
                        .repeated()
                        .at_least(1)
                        .ignored()
                        .or(any().ignored()),
                    one_of(":=;").ignored(),
                ))
                .recover_with(skip_until(
                    any().ignored(),
                    one_of(":=;").rewind().ignored().or(end()),
                    || DottedName::local(("<error>".into(), unreachable_span())),
                )),
        )
        .then_ignore(ignored())
        .then(
            just('=')
                .ignore_then(ignored())
                .ignore_then(full_expr.clone())
                .labelled("type body")
                .recover_with(via_parser(
                    any()
                        .and_is(just("::").or(just(";")).not())
                        .repeated()
                        .ignore_then(
                            empty()
                                .to_span()
                                .map(|loc: SimpleSpan| box_ast(ErrorTypeAST::new(loc.end.into()))),
                        ),
                )),
        )
        .then_ignore(ignored())
        .then(
            just("::")
                .then_ignore(ignored())
                .ignore_then(
                    metd.padded_by(ignored())
                        .then_ignore(
                            just(';')
                                .recover_with(skip_then_retry_until(none_of(";}").ignored(), end()))
                                .ignore_then(ignored())
                                .ignore_then(ignored()),
                        )
                        .repeated()
                        .collect()
                        .delimited_by(just('{'), just('}')),
                )
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .map(|((((anns, loc), name), val), metds)| {
            box_ast(TypeDefAST::new(loc, name, val, anns, metds))
        })
        .boxed(),
        anns.then(text::keyword("fn").map_with_span(|_, loc: SimpleSpan| loc.into_range().into()))
            .then_ignore(ignored())
            .then(
                id.clone()
                    .recover_with(skip_then_retry_until(
                        any()
                            .filter(|&c| is_xid_continue(c))
                            .repeated()
                            .at_least(1)
                            .ignored()
                            .or(any().ignored()),
                        one_of(":=;").ignored(),
                    ))
                    .recover_with(skip_until(
                        any().ignored(),
                        one_of(":=;").rewind().ignored().or(end()),
                        || DottedName::local(("<error>".into(), unreachable_span())),
                    )),
            )
            .then_ignore(ignored())
            .then(
                param
                    .separated_by(just(',').padded_by(ignored()).recover_with(
                        skip_then_retry_until(none_of(":=;").ignored(), one_of(":=;").ignored()),
                    ))
                    .allow_trailing()
                    .collect()
                    .delimited_by(
                        just('('),
                        just(')').recover_with(skip_then_retry_until(
                            none_of(":=;").ignored(),
                            one_of(":=;").ignored(),
                        )),
                    ),
            )
            .then_ignore(ignored())
            .then(
                just(':')
                    .ignore_then(ignored())
                    .ignore_then(part_expr.clone().recover_with(
                        via_parser(none_of(",=;").repeated().map_with_span(
                            |_, span: SimpleSpan| box_ast(ErrorTypeAST::new(span.end.into())),
                        )),
                    ))
                    .or_not()
                    .map_with_span(|expr, loc| {
                        expr.unwrap_or_else(|| box_ast(NullAST::new(loc.into_range().into())))
                    }),
            )
            .then_ignore(ignored())
            .then(
                just('=')
                    .ignore_then(ignored())
                    .ignore_then(full_expr.clone().recover_with(
                        via_parser(none_of(",;").repeated().map_with_span(
                            |_, span: SimpleSpan| box_ast(ErrorAST::new(span.end.into())),
                        )),
                    ))
                    .or_not()
                    .map_with_span(|expr, loc| {
                        expr.unwrap_or_else(|| box_ast(NullAST::new(loc.into_range().into())))
                    }),
            )
            .map(move |(((((anns, l), name), params), ret), body)| {
                box_ast(FnDefAST::new(
                    l,
                    name,
                    ret,
                    params,
                    body,
                    anns,
                    loc == DeclLoc::Method,
                ))
            })
            .boxed(),
    ])
    .labelled("a declaration")
}

/// create a parser for a statement.
/// it requires an expression parser to passed, otherwise it would require infinite recursion
fn def_stmt<'a>(expr: BoxedASTParser<'a, 'a>) -> BoxedASTParser<'a, 'a> {
    choice((
        declarations(DeclLoc::Local, None, &expr),
        import().map(box_ast),
        add_assigns(expr),
    ))
    .boxed()
}
fn top_level<'a>() -> impl Parser<'a, &'a str, BoxedAST<'a>, Extras<'a>> + Clone {
    let anns = annotation().padded_by(ignored()).repeated().collect();
    recursive(|tl| {
        choice((
            declarations(DeclLoc::Global, None, &expr_impl())
                .padded_by(ignored())
                .then_ignore(
                    just(';')
                        .recover_with(skip_then_retry_until(none_of(';').ignored(), end()))
                        .ignore_then(ignored()),
                ),
            import()
                .padded_by(ignored())
                .then_ignore(just(';').recover_with(skip_then_retry_until(
                    none_of(';').ignored(),
                    just(";").ignored(),
                )))
                .map(box_ast),
            anns.then(
                text::keyword("module").map_with_span(|_, loc: SimpleSpan| loc.into_range().into()),
            )
            .then_ignore(ignored())
            .then(
                global_id()
                    .recover_with(skip_then_retry_until(
                        any()
                            .filter(|&c| is_xid_continue(c))
                            .repeated()
                            .at_least(1)
                            .ignored()
                            .or(any().ignored()),
                        one_of("{=;").ignored(),
                    ))
                    .recover_with(skip_until(
                        any().ignored(),
                        one_of("{=;").rewind().ignored().or(end()),
                        || DottedName::local(("<error>".into(), unreachable_span())),
                    )),
            )
            .then_ignore(ignored())
            .then(choice((
                tl.repeated()
                    .collect()
                    .delimited_by(just('{'), just('}'))
                    .map(|v| (v, false)),
                just('=')
                    .then_ignore(ignored())
                    .ignore_then(cdn())
                    .map_with_span(|cdn, loc| {
                        (
                            vec![box_ast(ImportAST::new(
                                loc.into_range().into(),
                                cdn,
                                vec![],
                            ))],
                            false,
                        )
                    })
                    .then_ignore(just(';')),
                just(';').to((vec![], true)),
            )))
            .validate(|(((anns, loc), name), (body, err)), span, e| {
                if err {
                    e.emit(Rich::custom(
                        span,
                        "file-level module declaration cannot go here",
                    ))
                }
                box_ast(ModuleAST::new(loc, name, body, anns))
            }),
            just(';')
                .to_span()
                .map(|l: SimpleSpan| box_ast(NullAST::new(l.into_range().into()))),
        ))
    })
    .labelled("a top-level declaration")
}

/// add assignments and control flow to parser
fn add_assigns<'a: 'b, 'b>(
    expr: impl Parser<'a, &'a str, BoxedAST<'a>, Extras<'a>> + Clone + 'a,
) -> BoxedASTParser<'a, 'b> {
    // - 1: After the initial parser is done, the input can be followed up by any of these options.
    // The order in which they are listed indicates their precedence. After this `then()`, the
    // output will be (output of `expr`, operator).
    //
    // - 2: Right associative folding. Consider this input:
    // ```
    // x = y+=z<<=a
    // ```
    // This will parse as (x, =, (y, +=, (z, <<=, a))). Note this is the opposite direction of
    // associativity to math operations.
    let prev = expr
        .clone()
        .then(
            // 1
            choice(
                [
                    "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=",
                ]
                .map(just),
            )
            .map_with_span(|op, span: SimpleSpan| (intern_op(op), span.into_range().into()))
            .labelled("an operator")
            .padded_by(ignored()),
        )
        .repeated()
        .foldr(expr, |(lhs, (op, loc)), rhs| {
            // 2
            box_ast(BinOpAST::new(loc, op, lhs, rhs))
        })
        .labelled("an expression")
        .boxed();

    // - 1: Parse an if/else section, e.g.
    // ```
    // if (x == 3i32) {
    //   x += 1;
    // };
    // ```
    // -- 1a: The conditional will begin with `(` or `{`...
    // -- 1b: ...but don't immediately fail if not. In any case we have were expecting an
    // expression (something accepted by `expr`) and just consumed one character of it.
    // Rewind one character and parse it using `expr`.
    // -- 1c: The reason for having (1a) and (1b) in the first place is just for better error
    // messaging; we want to know if the user just forgot to wrap their condition in
    // parentheses/braces.
    // -- 1d: This parses the body of the section, e.g. `{ x += 1; }`.
    // -- 1e: There may or may not be an else clause.
    // -- 1f: If there was an "else" clause, then unwrap it, or just put a dummy ast node in
    // it's place.
    //
    // - 2: Parse a while section, e.g.
    // ```
    // while (x > 0i32) {
    //  x -= 1;
    // };
    // ```
    // This is functionally very similar to the if/else parsing.
    //
    // - 3: Error recovery strategy: maybe we're just seeing an else clause for some reason.
    // If that's the consume it. In any case, an error is thrown.
    recursive(|expr| {
        choice([
            text::keyword("if") // 1
                .ignore_then(ignored())
                .ignore_then(
                    one_of("({")
                        .or_not() // 1a
                        .rewind() // 1b
                        .then(expr.clone())
                        .validate(|(o, ast), loc, e| {
                            // 1c
                            if o.is_none() {
                                e.emit(Rich::custom(
                                    loc,
                                    "condition must be wrapped in either parentheses or braces",
                                ))
                            }
                            ast
                        })
                        .labelled("a condition"),
                )
                .then(expr.clone().padded_by(ignored())) // 1d
                .then(
                    text::keyword("else")
                        .ignore_then(expr.clone().padded_by(ignored()))
                        .or_not() // 1e
                        .map_with_span(|expr, loc| {
                            // 1f
                            expr.unwrap_or_else(|| box_ast(NullAST::new(loc.into_range().into())))
                        }),
                )
                .map_with_span(|((cond, if_true), if_false), span| {
                    box_ast(IfAST::new(
                        span.into_range().into(),
                        cond,
                        if_true,
                        if_false,
                    ))
                })
                .boxed(),
            text::keyword("while") // 2
                .ignore_then(ignored())
                .ignore_then(
                    one_of("({")
                        .or_not()
                        .rewind() // 2a
                        .then(expr.clone())
                        .validate(|(o, ast), loc, e| {
                            if o.is_none() {
                                e.emit(Rich::custom(
                                    loc,
                                    "condition must be wrapped in either parentheses or braces",
                                ))
                            }
                            ast
                        })
                        .labelled("a condition"),
                )
                .then(expr.clone().padded_by(ignored()))
                .map_with_span(|(cond, body), span| {
                    box_ast(WhileAST::new(span.into_range().into(), cond, body))
                })
                .boxed(),
            prev,
        ])
        .recover_with(via_parser(text::keyword("else").ignore_then(expr))) // 3
    })
    .labelled("an expression")
    .boxed()
}

/// create a parser for expressions, without assignment
fn expr_impl<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    type Cbi = utils::CharBytesIterator;
    let int_literal = choice((
        just("0x")
            .ignore_then(text::digits(16).at_least(1))
            .slice()
            .map(|v| i128::from_str_radix(v, 16).unwrap()),
        just("0o")
            .ignore_then(text::digits(8).at_least(1))
            .slice()
            .map(|v| i128::from_str_radix(v, 8).unwrap()),
        just("0b")
            .ignore_then(text::digits(2).at_least(1))
            .slice()
            .map(|v| i128::from_str_radix(v, 2).unwrap()),
        text::digits(10).slice().from_str().unwrapped(),
    ))
    .map_with_span(add_loc)
    .then_ignore(ignored())
    .then(
        ident()
            .map_with_span(|suf, span| (suf.into(), span.into_range().into()))
            .or_not(),
    )
    .map(|((val, loc), suf)| box_ast(IntLiteralAST::new(loc, val, suf)));
    let float_literal = text::digits(10)
        .then(just('.'))
        .then(text::digits(10).at_least(1))
        .or(text::digits(10)
            .at_least(1)
            .then(just('.'))
            .then(ident().not().rewind()))
        .slice()
        .from_str()
        .unwrapped()
        .map_with_span(add_loc)
        .then_ignore(ignored())
        .then(
            ident()
                .map_with_span(|suf, span| (suf.into(), span.into_range().into()))
                .or_not(),
        )
        .map(|((val, loc), suf)| box_ast(FloatLiteralAST::new(loc, val, suf)));
    let char_literal = just('\'')
        .ignore_then(choice((
            none_of("\\'").map(u32::from),
            just("\\0").to(0x00u32),
            just("\\n").to(0x0au32),
            just("\\r").to(0x0du32),
            just("\\t").to(0x09u32),
            just("\\v").to(0x0au32),
            just("\\b").to(0x08u32),
            just("\\e").to(0x1bu32),
            just("\\a").to(0x07u32),
            just("\\c").ignore_then(
                text::digits(16)
                    .exactly(2)
                    .slice()
                    .map(|v| u32::from_str_radix(v, 16).unwrap())
                    .recover_with(via_parser(empty().to(0u32))),
            ),
            just("\\u").ignore_then(
                text::digits(16)
                    .at_least(2)
                    .at_most(6)
                    .slice()
                    .map(|v| u32::from_str_radix(v, 16).unwrap())
                    .recover_with(skip_until(
                        none_of("}'").ignored(),
                        one_of("}'").ignored(),
                        || 0u32,
                    ))
                    .delimited_by(just('{'), just('}'))
                    .recover_with(skip_until(
                        none_of("}'").ignored(),
                        one_of("}'").ignored(),
                        || 0u32,
                    )),
            ),
            just('\\').ignore_then(any()).map(u32::from),
        )))
        .then_ignore(just('\''))
        .map_with_span(add_loc)
        .then_ignore(ignored())
        .then(
            ident()
                .map_with_span(|suf, span| (suf.into(), span.into_range().into()))
                .or_not(),
        )
        .map(|((val, loc), suf)| box_ast(CharLiteralAST::new(loc, val, suf)));
    let str_literal = just('"')
        .ignore_then(
            choice((
                none_of("\\\"").map(Cbi::from_char),
                just("\\0").to(Cbi::from_u8(0x00)),
                just("\\n").to(Cbi::from_u8(0x0a)),
                just("\\r").to(Cbi::from_u8(0x0d)),
                just("\\t").to(Cbi::from_u8(0x09)),
                just("\\v").to(Cbi::from_u8(0x0a)),
                just("\\b").to(Cbi::from_u8(0x08)),
                just("\\e").to(Cbi::from_u8(0x1b)),
                just("\\a").to(Cbi::from_u8(0x07)),
                just("\\c").ignore_then(
                    text::digits(16)
                        .exactly(2)
                        .slice()
                        .map(|v| Cbi::from_u8(u8::from_str_radix(v, 16).unwrap()))
                        .recover_with(via_parser(empty().to(Cbi::from_u8(0)))),
                ),
                just("\\x").ignore_then(
                    text::digits(16)
                        .exactly(2)
                        .slice()
                        .map(|v| Cbi::raw(u8::from_str_radix(v, 16).unwrap()))
                        .recover_with(via_parser(empty().to(Cbi::from_u8(0)))),
                ),
                just("\\u").ignore_then(
                    text::digits(16)
                        .at_least(2)
                        .at_most(6)
                        .slice()
                        .validate(|v, span, e| {
                            let v = u32::from_str_radix(v, 16).unwrap();
                            Cbi::from_u32(v).unwrap_or_else(|| {
                                e.emit(Rich::custom(
                                    span,
                                    format!("{v:0>4X} is not a valid Unicode codepoint"),
                                ));
                                Cbi::from_u8(0)
                            })
                        })
                        .recover_with(skip_until(
                            none_of("}'").ignored(),
                            one_of("}'").ignored(),
                            || Cbi::from_u8(0),
                        ))
                        .delimited_by(just('{'), just('}'))
                        .recover_with(skip_until(
                            none_of("}'").ignored(),
                            one_of("}'").ignored(),
                            || Cbi::from_u8(0),
                        )),
                ),
                just('\\').ignore_then(any()).map(Cbi::from_char),
            ))
            .repeated()
            .collect::<Vec<Cbi>>(),
        )
        .then_ignore(just('"'))
        .map_with_span(add_loc)
        .then_ignore(ignored())
        .then(
            ident()
                .map_with_span(|suf, span| (suf.into(), span.into_range().into()))
                .or_not(),
        )
        .map(|((val, loc), suf)| {
            box_ast(StringLiteralAST::new(
                loc,
                val.into_iter().flatten().collect(),
                suf,
            ))
        });
    let literal =
        choice((int_literal, float_literal, char_literal, str_literal)).labelled("a literal");
    let varget = just('.')
        .or_not()
        .map(|o| o.is_some())
        .then(ident())
        .map_with_span(|(global, name), loc| {
            box_ast(VarGetAST::new(loc.into_range().into(), name.into(), global))
        });
    let special = choice((
        text::keyword("null")
            .to_span()
            .map(|span: SimpleSpan| box_ast(NullAST::new(span.into_range().into()))),
        text::keyword("type")
            .to_span()
            .map(|span: SimpleSpan| box_ast(TypeLiteralAST::new(span.into_range().into()))),
    ))
    .labelled("a literal");
    let intrinsic = just('@')
        .ignore_then(ident())
        .map_with_span(|name, span| {
            box_ast(IntrinsicAST::new(span.into_range().into(), name.into()))
        })
        .labelled("an intrinsic");
    recursive(move |raw_expr| {
        let expr = add_assigns(raw_expr.clone());
        let maybe_expr = expr
            .clone()
            .or_not()
            .map_with_span(|ast, span: SimpleSpan| {
                ast.unwrap_or_else(|| box_ast(NullAST::new(span.into_iter().into())))
            });
        let atom = choice((
            literal,
            special,
            varget,
            intrinsic,
            choice((
                maybe_expr
                    .clone()
                    .padded_by(ignored())
                    .then_ignore(just(')').rewind()),
                maybe_expr
                    .clone()
                    .recover_with(skip_then_retry_until(
                        none_of(",;)}").ignored(),
                        one_of(",;)}").ignored(),
                    ))
                    .padded_by(ignored())
                    .separated_by(just(';').recover_with(skip_then_retry_until(
                        none_of(";)").ignored(),
                        one_of(";)").ignored(),
                    )))
                    .at_least(2)
                    .collect()
                    .map(|vals| box_ast(GroupAST::new(vals))),
                expr.clone()
                    .recover_with(skip_then_retry_until(
                        none_of(",;)}").ignored(),
                        one_of(",;)}").ignored(),
                    ))
                    .padded_by(ignored())
                    .separated_by(just(',').recover_with(skip_then_retry_until(
                        none_of(",)").ignored(),
                        one_of(",)").ignored(),
                    )))
                    .allow_trailing()
                    .at_least(1)
                    .collect()
                    .map(|vals| box_ast(TupleLiteralAST::new(vals))),
            ))
            .delimited_by(just('('), just(')'))
            .map_with_span(|ast, span| box_ast(ParenAST::new(span.into_range().into(), ast)))
            .recover_with(via_parser(nested_delimiters(
                '(',
                ')',
                [('[', ']'), ('{', '}')],
                |span: SimpleSpan| box_ast(ErrorAST::new(span.into_range().into())),
            ))),
            // array
            expr.clone()
                .recover_with(skip_then_retry_until(none_of(",;)}").ignored(), end()))
                .padded_by(ignored())
                .separated_by(just(',').recover_with(skip_then_retry_until(
                    none_of(",]").ignored(),
                    one_of(",]").ignored(),
                )))
                .allow_trailing()
                .collect()
                .delimited_by(just('['), just(']'))
                .map_with_span(|vals, span| {
                    box_ast(ArrayLiteralAST::new(
                        (span.start, 1).into(),
                        (span.end - 1, 1).into(),
                        vals,
                    ))
                })
                .recover_with(via_parser(nested_delimiters(
                    '[',
                    ']',
                    [('(', ')'), ('{', '}')],
                    |span: SimpleSpan| box_ast(ErrorAST::new(span.into_range().into())),
                ))),
            // block
            choice([
                empty()
                    .map_with_state(|_, _, state: &mut Vec<HashMap<&'a str, SimpleSpan>>| {
                        state.push(Default::default())
                    }) // push new field set onto stack
                    .then(
                        ident()
                            .map_with_span(|i, s| (i, s))
                            .try_map_with_state(|(name, span), _, state| {
                                if let Some(_prev) = state.last_mut().unwrap().insert(name, span) {
                                    Err(Rich::custom(span, format!("redefinition of {name}")))
                                } else {
                                    Ok(name.into())
                                }
                            })
                            .then_ignore(just(':').padded_by(ignored()))
                            .then(expr.clone()) // parse `name: type`
                            .padded_by(ignored())
                            .separated_by(just(',').padded_by(ignored()))
                            .allow_trailing()
                            .at_least(1)
                            .collect::<HashMap<_, _>>(),
                    )
                    .then(empty().map_with_state(
                        |_, _, state: &mut Vec<HashMap<&'a str, SimpleSpan>>| state.pop(),
                    ))
                    .map(|x| either::Right(x.0 .1))
                    .boxed(),
                def_stmt(
                    raw_expr
                        .or_not()
                        .map_with_span(|ast, span: SimpleSpan| {
                            ast.unwrap_or_else(|| box_ast(NullAST::new(span.into_iter().into())))
                        })
                        .boxed(),
                )
                .recover_with(skip_then_retry_until(none_of(";)}").ignored(), end()))
                .padded_by(ignored())
                .separated_by(just(';').recover_with(skip_then_retry_until(
                    none_of(";}").ignored(),
                    one_of(";}").ignored(),
                )))
                .collect()
                .map(either::Left)
                .boxed(),
            ])
            .delimited_by(just('{'), just('}'))
            .map_with_span(|vals, span| match vals {
                either::Left(vals) => box_ast(BlockAST::new(span.into_range().into(), vals)),
                either::Right(vals) => {
                    box_ast(StructLiteralAST::new(span.into_range().into(), vals))
                }
            })
            .recover_with(via_parser(nested_delimiters(
                '{',
                '}',
                [('[', ']'), ('(', ')')],
                |span: SimpleSpan| box_ast(ErrorAST::new(span.into_range().into())),
            ))),
        ))
        .labelled("an atom")
        .boxed();
        #[derive(Debug, Clone)]
        enum PostfixType<'src> {
            Op(char, SourceSpan),
            Attr(&'src str, SourceSpan),
            Sub(BoxedAST<'src>, SourceSpan),
            Call(Vec<BoxedAST<'src>>, SourceSpan),
        }
        let postfix = atom
            .foldl(
                choice((
                    // postfix operator
                    one_of("?!").map_with_span(|c: char, span: SimpleSpan| {
                        PostfixType::Op(c, span.into_range().into())
                    }),
                    // attribute
                    just('.')
                        .ignore_then(ignored())
                        .ignore_then(ident())
                        .map_with_span(|attr, span| {
                            PostfixType::Attr(attr, span.into_range().into())
                        }),
                    // subscript
                    maybe_expr
                        .padded_by(ignored())
                        .delimited_by(just('['), just(']'))
                        .map_with_span(|ast, loc| PostfixType::Sub(ast, loc.into_range().into())),
                    expr.padded_by(ignored())
                        .separated_by(just(',').recover_with(skip_then_retry_until(
                            none_of(",)").ignored(),
                            one_of(",)").ignored(),
                        )))
                        .allow_trailing()
                        .collect()
                        .delimited_by(just('('), just(')'))
                        .map_with_span(|ast, loc| PostfixType::Call(ast, (loc.end - 1, 1).into())),
                ))
                .labelled("an operator")
                .padded_by(ignored())
                .repeated(),
                |ast, op| match op {
                    PostfixType::Op(op, loc) => {
                        box_ast(PostfixAST::new(loc, intern_char_op(op), ast))
                    }
                    PostfixType::Attr(attr, loc) => box_ast(DotAST::new(ast, (attr.into(), loc))),
                    PostfixType::Sub(idx, loc) => box_ast(SubAST::new(loc, ast, idx)),
                    PostfixType::Call(args, loc) => box_ast(CallAST::new(loc, ast, args)),
                },
            )
            .labelled("an expression")
            .boxed();
        let prefix = choice((just("++"), just("--"), text::keyword("mut")))
            .or(one_of("+-*&~!").slice())
            .labelled("an operator")
            .map_with_span(add_loc)
            .padded_by(ignored())
            .repeated()
            .foldr(postfix, |(op, loc), ast| {
                box_ast(PrefixAST::new(loc, intern_op(op), ast))
            })
            .labelled("an expression")
            .boxed();
        #[inline(always)]
        fn impl_ltr<'a, const N: usize>(
            prev: BoxedASTParser<'a, 'a>,
            ops: [&'static str; N],
        ) -> BoxedASTParser<'a, 'a> {
            prev.clone()
                .foldl(
                    choice(ops.map(just))
                        .labelled("an operator")
                        .map_with_span(|op, span: SimpleSpan| {
                            (intern_op(op), span.into_range().into())
                        })
                        .padded_by(ignored())
                        .then(prev)
                        .repeated(),
                    |lhs, ((op, loc), rhs)| box_ast(BinOpAST::new(loc, op, lhs, rhs)),
                )
                .labelled("an expression")
                .boxed()
        }
        let mul_div = impl_ltr(prefix, ["*", "/", "%"]);
        let add_sub = impl_ltr(mul_div, ["+", "-"]);
        let shifts = impl_ltr(add_sub, ["<<", ">>"]);
        let cmps = impl_ltr(shifts, ["<=", ">=", "<", ">"]);
        let eqs = impl_ltr(cmps, ["==", "!="]);
        let log_or = ["&", "^", "|", "&?", "|?"]
            .into_iter()
            .fold(eqs, |parser, op| impl_ltr(parser, [op]));
        // casts
        log_or
            .clone()
            .foldl(
                just(':')
                    .ignore_then(just('?').or_not())
                    .map(|o| o.is_some())
                    .labelled("an operator")
                    .map_with_span(add_loc)
                    .padded_by(ignored())
                    .then(log_or)
                    .repeated(),
                |lhs, ((bit, loc), rhs)| {
                    if bit {
                        box_ast(BitCastAST::new(loc, lhs, rhs))
                    } else {
                        box_ast(CastAST::new(loc, lhs, rhs))
                    }
                },
            )
            .labelled("an expression")
            .boxed()
    })
    .boxed()
}
/// create a parser for expressions
#[inline(always)]
pub fn parse_expr<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    add_assigns(expr_impl())
}
/// create a parser for statements
#[inline(always)]
pub fn parse_stmt<'a: 'b, 'b>() -> BoxedASTParser<'a, 'b> {
    def_stmt(expr_impl())
}
/// create a parser for the top-level scope
pub fn parse_tl<'a: 'b, 'b>() -> BoxedParser<'a, 'b, TopLevelAST<'a>> {
    text::keyword("module")
        .then_ignore(ignored())
        .ignore_then(global_id())
        .then_ignore(ignored())
        .then_ignore(just(';'))
        .padded_by(ignored())
        .or_not()
        .then(top_level().repeated().collect())
        .map(|(module, vals)| TopLevelAST::new(vals, module))
        .then_ignore(ignored().then(end()))
        .boxed()
}

#[cfg(test)]
mod tests;
