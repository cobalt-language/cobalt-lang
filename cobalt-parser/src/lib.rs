use cobalt_ast::ast::TopLevelAST;
use cobalt_errors::CobaltError;
use lexer::SourceReader;
use parser::Parser;

pub mod lexer;
pub mod parser;
pub mod utils;

pub fn parse_str<'src>(src: &'src str) -> (Option<TopLevelAST<'src>>, Vec<CobaltError<'src>>) {
    let mut reader = SourceReader::new(src);
    let tokens = reader.tokenize().0;

    let mut parser = Parser::new(src, tokens);
    parser.next();
    parser.parse()
}
