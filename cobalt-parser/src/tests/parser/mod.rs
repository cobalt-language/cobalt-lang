use super::*;
use cobalt_errors::{CobaltError, Report};

mod decl;
mod expr;
mod general;

fn test_parser_fn<T, F>(src: &'static str, show_output: bool, parse_fn: F)
where
    T: std::fmt::Debug + 'static,
    F: FnOnce(&mut Parser<'static>, &mut Vec<CobaltError<'static>>) -> T,
{
    let mut reader = SourceReader::new(src);

    let (tokens, mut errors) = reader.tokenize();

    let mut parser = Parser::new(tokens);
    parser.next();
    let ast_or_similar = parse_fn(&mut parser, &mut errors);

    if show_output {
        dbg!(ast_or_similar);
        for (err_count, e) in errors.iter().enumerate() {
            if err_count > 5 {
                break;
            }
            let printable_e = Report::from(e.clone()).with_source_code(src);
            println!("{:?}", printable_e);
        }
    }

    assert!(errors.is_empty());
}
