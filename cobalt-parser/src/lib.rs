#![allow(unused_imports, dead_code)] // TODO: remove before merge
use cobalt_errors::*;
use cobalt_ast::{*, ast::*};
use unicode_ident::*;
use chumsky::prelude::*;
mod utils;
pub mod prelude {
    pub use super::parse_expr;
    pub use super::parse_tl;
    pub use super::cvt_err;
    pub use chumsky::Parser as _;
}
type Error = Simple<char>;
pub fn cvt_err(_err: Error) -> CobaltError {
    unimplemented!()
}
pub fn parse_expr() -> BoxedParser<'static, char, Box<dyn AST>, Error> {
    unimplemented!()
}
pub fn parse_tl() -> BoxedParser<'static, char, TopLevelAST, Error> {
    unimplemented!()
}