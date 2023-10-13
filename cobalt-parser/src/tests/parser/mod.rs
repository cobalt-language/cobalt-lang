use super::*;
use cobalt_errors::{CobaltError, Report};

mod expr;
mod general;

fn test_parser_fn<T, F>(src: &'static str, show_output: bool, parse_fn: F)
where
    T: std::fmt::Debug + 'static,
    F: Fn(&mut Parser<'static>) -> (T, Vec<CobaltError<'static>>),
{
    let mut reader = SourceReader::new(src);
    let tokens = reader.tokenize().0;
    //dbg!(&tokens.0);

    let mut parser = Parser::new(src, tokens);
    parser.next();
    let (ast_or_similar, errors) = parse_fn(&mut parser);

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
