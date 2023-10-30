use super::*;

mod literals;

#[test]
fn test_binop() {
    test_parser_fn(
        "a + b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a - b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a * b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a / b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    // --- In place operators.

    test_parser_fn(
        "a += b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a -= b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a *= b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "a /= b",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );
}

#[test]
fn test_paren_add() {
    test_parser_fn(
        "a + (b + c)",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );
}

#[test]
fn test_mul_add() {
    test_parser_fn(
        "a * b + c",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );
}

#[test]
fn test_bitcast() {
    test_parser_fn(
        "a + 4 :? u8",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );
}

#[test]
fn test_mixed() {
    test_parser_fn("a * b + c / (d - (f + g * h))", true, |parser| {
        parser.parse_expr(false)
    });
}

#[test]
fn test_block_expr() {
    test_parser_fn("{ a + b;; let x = 4; x }", true, |parser| {
        parser.parse_block_expr()
    });

    test_parser_fn("{ x = 4; }", true, |parser| parser.parse_block_expr());

    test_parser_fn("{ x: u32, y: f64, }", true, |parser| {
        parser.parse_block_expr()
    });
}

#[test]
fn test_prefix_expr() {
    test_parser_fn("!a", true, |parser| parser.parse_prefix_expr());

    test_parser_fn("!&*a", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("++a", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("--a", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("+a", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("-a", true, |parser| parser.parse_primary_expr(false));
}

#[test]
fn test_flow_expr() {
    test_parser_fn("if (a) x = 4", true, |parser| parser.parse_if_expr());

    test_parser_fn("if (x == 3) x = 4 else y = 5", true, |parser| {
        parser.parse_if_expr()
    });

    test_parser_fn("while (x < 10) ++i", true, |parser| {
        parser.parse_while_expr()
    });
}

#[test]
fn test_fn_call() {
    test_parser_fn("foo()", true, |parser| parser.parse_expr(false));

    test_parser_fn("foo(x, y + z)", true, |parser| parser.parse_expr(false));

    test_parser_fn(
        "@size(T)",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "(*fn_ptr)()",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );

    test_parser_fn(
        "returns_fn()()",
        true,
        Box::new(|parser: &mut Parser<'static>| parser.parse_expr(false)),
    );
}

#[test]
fn test_dotted() {
    test_parser_fn("foo.bar.baz", true, |parser| parser.parse_ident_expr());

    test_parser_fn("foo.bar.baz()", true, |parser| {
        parser.parse_primary_expr(false)
    });

    test_parser_fn("foo().bar", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("(a + b).bar", true, |parser| {
        parser.parse_primary_expr(false)
    });
}

#[test]
fn test_index() {
    test_parser_fn("arr[i][j]", true, |parser| parser.parse_primary_expr(false));
}

#[test]
fn test_postfix() {
    test_parser_fn("a?!", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("a++", true, |parser| parser.parse_primary_expr(false));

    test_parser_fn("a--", true, |parser| parser.parse_primary_expr(false));
}

#[test]
fn test_cast() {
    test_parser_fn("ptr : *mut u8", true, |parser| parser.parse_expr(false));
}

#[test]
fn test_intrinsics() {
    test_parser_fn("@const", true, |parser| parser.parse_intrinsic());
}
