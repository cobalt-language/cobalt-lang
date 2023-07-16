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
pub fn cvt_err(err: Rich<char>) -> MietteDiagnostic {
    MietteDiagnostic {
        labels: Some(vec![LabeledSpan::underline(err.span().into_range())]),
        ..MietteDiagnostic::new(err.to_string())
    }
}
pub fn parse_expr<'a>() -> impl Parser<'a, &'a str, Box<dyn AST>, chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>> {
    todo()
}
pub fn parse_tl<'a>() -> impl Parser<'a, &'a str, TopLevelAST, chumsky::extra::Full<Rich<'a, char>, (), Vec<usize>>> {
    todo()
}