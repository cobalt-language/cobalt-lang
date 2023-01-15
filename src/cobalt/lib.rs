pub mod parser;
#[allow(unused_variables)]
pub mod ast;
pub mod context;
pub mod dottedname;
pub mod errors;
pub mod parsed_type;
pub mod misc;
pub mod types;
pub mod varmap;

pub use parser::lexer::{lex, Token, TokenData};
pub use parser::ast::parse;
pub use dottedname::*;
pub use misc::*;
pub use context::*;
pub use ast::AST;
pub use types::{Type, SizeType};
pub use varmap::*;
pub use errors::Diagnostic;
pub(crate) use ast::*;
pub(crate) use parsed_type::*;

#[cfg(test)]
mod tests;
