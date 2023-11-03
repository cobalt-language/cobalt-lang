use super::*;

#[test]
fn test_parse_num_literal() {
    test_parser_fn("1", true, |parser, errors| parser.parse_literal(errors));

    test_parser_fn("1i32", true, |parser, errors| parser.parse_literal(errors));

    test_parser_fn("1.0", true, |parser, errors| parser.parse_literal(errors));

    test_parser_fn("1.0f32", true, |parser, errors| {
        parser.parse_literal(errors)
    });
}

#[test]
fn test_parse_struct_literal() {
    test_parser_fn("{size: u32, offset: u16}", true, |parser, errors| {
        parser.parse_struct_literal(errors)
    });
}

#[test]
fn test_parse_string_literal() {
    test_parser_fn(r#""Hello, world!""#, true, |parser, errors| {
        parser.parse_string_literal(errors)
    });

    test_parser_fn(r#""\cff""#, true, |parser, errors| {
        parser.parse_string_literal(errors)
    });

    test_parser_fn(r#""\xff""#, true, |parser, errors| {
        parser.parse_string_literal(errors)
    });

    test_parser_fn(r#""before \u{2122}""#, true, |parser, errors| {
        parser.parse_string_literal(errors)
    });

    test_parser_fn(r#""c string"c"#, true, |parser, errors| {
        parser.parse_literal(errors)
    });
}

#[test]
fn test_parse_char_literal() {
    test_parser_fn(r#"'c'"#, true, |parser, errors| {
        parser.parse_char_literal(errors)
    });

    test_parser_fn(r#"'\u{2122}'"#, true, |parser, errors| {
        parser.parse_char_literal(errors)
    });

    test_parser_fn(r#"'\0'"#, true, |parser, errors| {
        parser.parse_char_literal(errors)
    });
}
