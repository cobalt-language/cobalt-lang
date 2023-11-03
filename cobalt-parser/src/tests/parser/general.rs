use super::*;

#[test]
fn test_hello_world() {
    let src = include_str!("../inputs/hello_world.co");
    test_parser_fn(src, true, |parser, errors| parser.parse(errors));
}

#[test]
fn test_parse_1() {
    let src = include_str!("../inputs/test_1.co");
    test_parser_fn(src, true, |parser, errors| parser.parse(errors));
}

#[test]
fn test_parse_2() {
    let src = include_str!("../inputs/test_2.co");
    test_parser_fn(src, true, |parser, errors| parser.parse(errors));
}
