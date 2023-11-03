use super::*;

mod literals;

#[test]
fn test_binop() {
    test_parser_fn("a + b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a - b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a * b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a / b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    // --- In place operators.

    test_parser_fn("a += b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a -= b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a *= b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("a /= b", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    // --- Assignment operators should be right associative.

    test_parser_fn("a += b += c += d", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_paren_add() {
    test_parser_fn("a + (b + c)", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_mul_add() {
    test_parser_fn("a * b + c", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_bitcast() {
    test_parser_fn("a + 4 :? u8", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_mixed() {
    test_parser_fn("a * b + c / (d - (f + g * h))", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_block_expr() {
    test_parser_fn("{ a + b;; let x = 4; x }", true, |parser, errors| {
        parser.parse_block_expr(errors)
    });

    test_parser_fn("{ x = 4; }", true, |parser, errors| {
        parser.parse_block_expr(errors)
    });

    test_parser_fn("{ x: u32, y: f64, }", true, |parser, errors| {
        parser.parse_block_expr(errors)
    });
}

#[test]
fn test_prefix_expr() {
    test_parser_fn("!a", true, |parser, errors| {
        parser.parse_prefix_expr(errors)
    });

    test_parser_fn("!&*a", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("++a", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("--a", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("+a", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("-a", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });
}

#[test]
fn test_flow_expr() {
    test_parser_fn("if (a) x = 4", true, |parser, errors| {
        parser.parse_if_expr(errors)
    });

    test_parser_fn("if (x == 3) x = 4 else y = 5", true, |parser, errors| {
        parser.parse_if_expr(errors)
    });

    test_parser_fn("while (x < 10) ++i", true, |parser, errors| {
        parser.parse_while_expr(errors)
    });
}

#[test]
fn test_fn_call() {
    test_parser_fn("foo()", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("foo(x, y + z)", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("@size(T)", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("(*fn_ptr)()", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });

    test_parser_fn("returns_fn()()", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_dotted() {
    test_parser_fn("foo.bar.baz", true, |parser, errors| {
        parser.parse_ident_expr(errors)
    });

    test_parser_fn("foo.bar.baz()", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("foo().bar", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("(a + b).bar", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });
}

#[test]
fn test_index() {
    test_parser_fn("arr[i][j]", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });
}

#[test]
fn test_postfix() {
    test_parser_fn("a?!", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("a++", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });

    test_parser_fn("a--", true, |parser, errors| {
        parser.parse_primary_expr(false, errors)
    });
}

#[test]
fn test_cast() {
    test_parser_fn("ptr : *mut u8", true, |parser, errors| {
        parser.parse_expr(false, errors)
    });
}

#[test]
fn test_intrinsics() {
    test_parser_fn("@align", true, |parser, _errors| parser.parse_intrinsic());
}
