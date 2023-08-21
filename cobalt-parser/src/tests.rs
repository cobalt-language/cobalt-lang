use super::*;

#[test]
fn test_fn_decls() {
    let parser = super::parse_tl();

    let no_p_no_rt = "fn foo1();";
    assert!(!parser.parse(no_p_no_rt).has_errors());

    let one_p_no_rt = "fn foo2(a: i32);";
    assert!(!parser.parse(one_p_no_rt).has_errors());

    let two_p_no_rt = "fn foo3(a: i32, b: i32);";
    assert!(!parser.parse(two_p_no_rt).has_errors());

    let no_p_rt = "fn foo4(): i32;";
    assert!(!parser.parse(no_p_rt).has_errors());

    let one_p_rt = "fn foo5(a: i32): i32;";
    assert!(!parser.parse(one_p_rt).has_errors());

    let two_p_rt = "fn foo6(a: i32, b: i32): i32;";
    assert!(!parser.parse(two_p_rt).has_errors());
}

#[test]
fn test_fn_calls() {
    let parser = super::parse_expr();

    let no_p = "foo1()";
    assert!(!parser.parse(no_p).has_errors());

    let one_p = "foo2(a)";
    assert!(!parser.parse(one_p).has_errors());

    let two_p = "foo3(a, b)";
    assert!(!parser.parse(two_p).has_errors());
}

#[test]
fn test_var_decl() {
    let parser = super::parse_tl();

    let no_ty = "let a = 1;";
    assert!(!parser.parse(no_ty).has_errors());

    let ty = "let a: i32 = 1;";
    assert!(!parser.parse(ty).has_errors());
}

#[test]
fn test_bin_ops() {
    let parser = super::parse_expr();

    let add = "1 + 22";
    assert!(!parser.parse(add).has_errors());

    let sub = "11 - 2";
    assert!(!parser.parse(sub).has_errors());

    let mul = "1 * 2";
    assert!(!parser.parse(mul).has_errors());

    let div = "1 / 2";
    assert!(!parser.parse(div).has_errors());

    let mod_ = "1 % 2";
    assert!(!parser.parse(mod_).has_errors());

    let shl = "1 << 2";
    assert!(!parser.parse(shl).has_errors());

    let shr = "1 >> 2";
    assert!(!parser.parse(shr).has_errors());

    let lt = "1 < 2";
    assert!(!parser.parse(lt).has_errors());

    let gt = "1 > 2";
    assert!(!parser.parse(gt).has_errors());

    let le = "1 <= 2";
    assert!(!parser.parse(le).has_errors());

    let ge = "1 >= 2";
    assert!(!parser.parse(ge).has_errors());

    let eq = "1 == 2";
    assert!(!parser.parse(eq).has_errors());

    let ne = "1 != 2";
    assert!(!parser.parse(ne).has_errors());

    let and = "1 & 2";
    assert!(!parser.parse(and).has_errors());

    let or = "1 | 2";
    assert!(!parser.parse(or).has_errors());

    let xor = "1 ^ 2";
    assert!(!parser.parse(xor).has_errors());

    let oror = "1 || 2";
    assert!(parser.parse(oror).has_errors());

    let andq = "1 &? 2";
    assert!(!parser.parse(andq).has_errors());

    let orq = "1 |? 2";
    assert!(!parser.parse(orq).has_errors());
}

#[test]
fn test_add_assigns() {
    let parser = super::parse_stmt();

    let eq = "a = 1";
    assert!(!parser.parse(eq).has_errors());

    let add_eq = "a += 1";
    assert!(!parser.parse(add_eq).has_errors());

    let sub_eq = "a -= 1";
    assert!(!parser.parse(sub_eq).has_errors());

    let mul_eq = "a *= 1";
    assert!(!parser.parse(mul_eq).has_errors());

    let div_eq = "a /= 1";
    assert!(!parser.parse(div_eq).has_errors());

    let mod_eq = "a %= 1";
    assert!(!parser.parse(mod_eq).has_errors());

    let and_eq = "a &= 1";
    assert!(!parser.parse(and_eq).has_errors());

    let or_eq = "a |= 1";
    assert!(!parser.parse(or_eq).has_errors());

    let xor_eq = "a ^= 1";
    assert!(!parser.parse(xor_eq).has_errors());

    let shl_eq = "a <<= 1";
    assert!(!parser.parse(shl_eq).has_errors());

    let shr_eq = "a >>= 1";
    assert!(!parser.parse(shr_eq).has_errors());
}

#[test]
fn test_cond() {
    let parser = super::parse_stmt();

    let if_ = "if (true) {}";
    assert!(!parser.parse(if_).has_errors());

    let if_else = "if (true) {} else {}";
    assert!(!parser.parse(if_else).has_errors());

    let if_elseif = "if (true) {} else if (true) {}";
    assert!(!parser.parse(if_elseif).has_errors());

    let if_elseif_else = "if (true) {} else if (true) {} else {}";
    assert!(!parser.parse(if_elseif_else).has_errors());

    let if_elseif_else_if = "if (true) {} else if (true) {} else if (true) {}";
    assert!(!parser.parse(if_elseif_else_if).has_errors());

    let if_elseif_else_if_else = "if (true) {} else if (true) {} else if (true) {} else {}";
    assert!(!parser.parse(if_elseif_else_if_else).has_errors());

    let if_cond = "if (1 < 2) {}";
    assert!(!parser.parse(if_cond).has_errors());

    let if_else_cond = "if (1 < 2) {} else {}";
    assert!(!parser.parse(if_else_cond).has_errors());

    let if_elseif_cond = "if (1 < 2) {} else if (1 < 2) {}";
    assert!(!parser.parse(if_elseif_cond).has_errors());
}

#[test]
fn test_annotations() {
    let parser = super::parse_tl();

    let single_ann = "@C(extern) fn puts(str: *u8);";
    assert!(!parser.parse(single_ann).has_errors());

    let multi_ann = "@C(extern) @C(inline) fn puts(str: *u8);";
    assert!(!parser.parse(multi_ann).has_errors());
}

#[test]
fn test_type_decls() {
    let parser = super::parse_tl();

    let typedef = "type MyType = i32;";
    assert!(!parser.parse(typedef).has_errors());

    let tuple = "type MyTuple = (i32, i32);";
    assert!(!parser.parse(tuple).has_errors());

    let type_impls = "type MyType = i32 :: { fn foo(): i32 = {}; };";
    assert!(!parser.parse(type_impls).has_errors());

    let self_t = "type MyType = i32 :: { fn foo(self: self_t): i32 = {}; };";
    assert!(!parser.parse(self_t).has_errors());

    let base_t = "type MyType = i32 :: { fn foo(x: base_t): i32 = {}; };";
    assert!(!parser.parse(base_t).has_errors());
}
