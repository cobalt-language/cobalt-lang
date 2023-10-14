use super::*;
use cobalt_errors::{CobaltError, Report};

mod decl;
mod expr;
mod general;

fn test_parser_fn<T, F>(src: &'static str, show_output: bool, parse_fn: F)
where
    T: std::fmt::Debug + 'static,
    F: Fn(&mut Parser<'static>) -> (T, Vec<CobaltError<'static>>),
{
    let mut reader = SourceReader::new(src);
    let mut errors = vec![];

    let tokenize_result = reader.tokenize();
    errors.extend(tokenize_result.1);

    let mut parser = Parser::new(src, tokenize_result.0);
    parser.next();
    let (ast_or_similar, parser_errors) = parse_fn(&mut parser);
    errors.extend(parser_errors);

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
