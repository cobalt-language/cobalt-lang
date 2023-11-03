#![allow(clippy::type_complexity)]
use cobalt_ast::ast::TopLevelAST;
use cobalt_errors::CobaltError;
pub use lexer::SourceReader;
pub use parser::Parser;

pub mod lexer;
pub mod parser;
pub mod utils;

pub fn parse_str(src: &str) -> (Option<TopLevelAST>, Vec<CobaltError>) {
    let mut reader = SourceReader::new(src);
    let (tokens, mut errors) = reader.tokenize();

    let mut parser = Parser::new(tokens);
    parser.next();
    let mut ast = parser.parse(&mut errors);

    (ast, errors)
}

#[cfg(test)]
mod tests;
