pub mod lexer;
pub mod ast;
mod ops;

pub use lexer::{Token, TokenData, lex};
pub use ast::parse;
use cobalt_errors::{unreachable_span, merge_spans, SourceSpan, Diagnostic};
use cobalt_ast::{*, ast::*};
use cobalt_utils::Flags;
