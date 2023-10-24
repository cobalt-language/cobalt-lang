use super::*;

#[test]
fn test_parse_num_literal() {
    test_parser_fn("1", true, |parser| parser.parse_literal());

    test_parser_fn("1i32", true, |parser| parser.parse_literal());

    test_parser_fn("1.0", true, |parser| parser.parse_literal());

    test_parser_fn("1.0f32", true, |parser| parser.parse_literal());
}

#[test]
fn test_parse_struct_literal() {
    test_parser_fn("{size: u32, offset: u16}", true, |parser| {
        parser.parse_struct_literal()
    });
}

#[test]
fn test_parse_string_literal() {
    test_parser_fn(r#""Hello, world!""#, true, |parser| {
        parser.parse_string_literal()
    });

    test_parser_fn(r#""\cff""#, true, |parser| parser.parse_string_literal());

    test_parser_fn(r#""\xff""#, true, |parser| parser.parse_string_literal());

    test_parser_fn(r#""before \u{2122}""#, true, |parser| {
        parser.parse_string_literal()
    });
}
