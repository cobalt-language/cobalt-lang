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
    let tokenize_result = reader.tokenize();
    errors.extend(tokenize_result.1);

    let mut parser = Parser::new(src, tokenize_result.0);
    parser.next();
    let parse_result = parser.parse();
    errors.extend(parse_result.1);

    (parse_result.0, errors)
}

#[cfg(test)]
mod tests;
