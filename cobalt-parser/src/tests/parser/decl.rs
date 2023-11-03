use super::*;
use crate::parser::DeclLoc;

#[test]
fn test_parse_let_decl() {
    test_parser_fn("let x: i32 = 5i32;", true, |parser, errors| {
        parser.parse_let_decl(DeclLoc::Local, errors)
    });

    test_parser_fn("let x: i32;", true, |parser, errors| {
        parser.parse_let_decl(DeclLoc::Local, errors)
    });
}

#[test]
fn test_parse_fn_param() {
    test_parser_fn("x: i32", true, |parser, errors| {
        parser.parse_fn_param(errors)
    });

    test_parser_fn("mut x: i32", true, |parser, errors| {
        parser.parse_fn_param(errors)
    });

    test_parser_fn("const x: i32 = 5i32", true, |parser, errors| {
        parser.parse_fn_param(errors)
    });

    test_parser_fn("x: *mut i32", true, |parser, errors| {
        parser.parse_fn_param(errors)
    });
}

#[test]
fn test_parse_type_decl() {
    test_parser_fn("type Foo = i32;", true, |parser, errors| {
        parser.parse_type_decl(true, errors)
    });
}

#[test]
fn test_fn_def() {
    test_parser_fn("fn foo(x: i32): i32 = 5i32;", true, |parser, errors| {
        parser.parse_fn_def(DeclLoc::Global, errors)
    });

    test_parser_fn("fn foo();", true, |parser, errors| {
        parser.parse_fn_def(DeclLoc::Global, errors)
    });

    test_parser_fn("@C(extern) fn puts(str: *u8);", true, |parser, errors| {
        parser.parse_fn_def(DeclLoc::Global, errors)
    });

    test_parser_fn("@C(extern) @inline fn foo();", true, |parser, errors| {
        parser.parse_fn_def(DeclLoc::Global, errors)
    });

    test_parser_fn(
        "fn foo(): i32 = { let x = 3; x};",
        true,
        |parser, errors| parser.parse_fn_def(DeclLoc::Global, errors),
    );
}

#[test]
fn test_module() {
    test_parser_fn("module foo;", true, |parser, errors| {
        parser.parse_file_module_decl(errors)
    });

    test_parser_fn(
        "@ann module foo.bar :: { fn baz(): i32 = { let x = 3; x}; module kay :: {}; };",
        true,
        |parser, errors| parser.parse_inline_module_decl(errors),
    );
}
