#![allow(unused_imports, dead_code)] // TODO: remove before merge
use cobalt_errors::*;
use cobalt_ast::{*, ast::*};
use unicode_ident::*;
use chumsky::prelude::*;
use cobalt_errors::miette::{MietteDiagnostic, LabeledSpan};
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
// useful type definitions
type Extras<'a> = chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>;
type BoxedParser<'a, 'b, T> = Boxed<'a, 'b, &'a str, T, Extras<'a>>;
type BoxedASTParser<'a, 'b> = BoxedParser<'a, 'b, Box<dyn AST>>;
/// parse an identifier. unicode-aware
fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, Extras<'a>> + Copy {any().filter(|&c| is_xid_start(c)).then(any().filter(|&c| is_xid_continue(c)).repeated()).slice()}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclLoc {
    Local,
    Method,
    Global
}
/// parse declarations. these are:
/// - functions
/// - variables
/// - type definitions
fn declarations<'a>(loc: DeclLoc, metd: Option<BoxedASTParser<'a, '_>>) -> impl Parser<'a, &'a str, Box<dyn AST>, chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>> + Clone {
    let metd = metd.unwrap_or_else(|| recursive(|m| declarations(DeclLoc::Method, Some(m.boxed()))).boxed());
    todo()
}
/// create a parser for a statement.
/// it requires an expression parser to passed, otherwise it would require infinite recursion
fn def_stmt<'a>(expr: impl Parser<'a, &'a str, Box<dyn AST>, chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>> + Clone) -> impl Parser<'a, &'a str, Box<dyn AST>, chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>> + Clone {
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