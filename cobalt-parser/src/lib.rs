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
    let mut errors = vec![];
    let mut tokenize_result = reader.tokenize();
    errors.append(&mut tokenize_result.1);

    let mut parser = Parser::new(tokenize_result.0);
    parser.next();
    let mut parse_result = parser.parse();
    errors.append(&mut parse_result.1);

    (parse_result.0, errors)
}

#[cfg(test)]
mod tests;
