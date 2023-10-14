use super::*;

#[test]
fn test_parse_let_decl() {
    test_parser_fn(
        "let x: i32 = 5i32;",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_let_decl()),
    );

    test_parser_fn(
        "let x: i32;",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_let_decl()),
    );
}

#[test]
fn test_parse_fn_param() {
    test_parser_fn(
        "x: i32",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_fn_param()),
    );

    test_parser_fn(
        "mut x: i32",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_fn_param()),
    );

    test_parser_fn(
        "const x: i32 = 5i32",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_fn_param()),
    );

    test_parser_fn(
        "x: *mut i32",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_fn_param()),
    );
}

#[test]
fn test_parse_type_decl() {
    test_parser_fn(
        "type Foo = i32;",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_type_decl()),
    );
}

#[test]
fn test_fn_def() {
    // test_parser_fn(
    //     "fn foo(x: i32): i32 = 5i32;",
    //     true,
    //     Box::new(|parser: &mut Parser<'static>| parser.parse_fn_def(false)),
    // );

    // test_parser_fn(
    //     "fn foo();",
    //     true,
    //     Box::new(|parser: &mut Parser<'static>| parser.parse_fn_def(false)),
    // );

    test_parser_fn(
        "@C(extern) fn puts(str: *u8);",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_fn_def(false)),
    );

    // test_parser_fn(
    //     "@C(extern) @inline fn foo();",
    //     true,
    //     Box::new(|parser: &mut Parser<'static>| parser.parse_fn_def(false)),
    // );

    // test_parser_fn(
    //     "fn foo(): i32 = { let x = 3; x};",
    //     true,
    //     Box::new(|parser: &mut Parser<'static>| parser.parse_fn_def(false)),
    // );
}

#[test]
fn test_module() {
    test_parser_fn(
        "module foo;",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_module_decl()),
    );
}

#[test]
fn test_annotation() {
    test_parser_fn(
        "@method",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_annotation()),
    );

    test_parser_fn(
        "@C(extern)",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_annotation()),
    );
}
