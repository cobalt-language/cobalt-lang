use super::*;
use TokenData::*;
fn test_lex(text: &str, toks: &[Token]) {
    let flags = Flags::default();
    let (res, errs) = lex(text, Location::from_name(ANONYMOUS), &flags);
    if errs.len() > 0 {
        panic!("lexing produced an error");
    }
    let mut idx = 0;
    for (l, r) in res.iter().zip(toks.iter()) {
        if l != r {
            panic!("mismatch in element {idx}: {l} (@ {}) != {r} (@ {})", l.loc, r.loc);
        }
        idx += 1;
    }
}
#[test]
fn identifiers() {
    test_lex("a b c de fgh $ _", &[
        Token::new(Location::new(ANONYMOUS, 1, 1, 0), Identifier("a".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 3, 2), Identifier("b".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 5, 4), Identifier("c".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 7, 6), Identifier("de".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 10, 9), Identifier("fgh".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 14, 13), Identifier("$".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 16, 15), Identifier("_".to_string()))
    ]);
}
#[test]
fn numbers() {
    test_lex("0 1 0xDEADBEEF 0b10011001 0o777 5.0", &[
        Token::new(Location::new(ANONYMOUS, 1, 1, 0), Int(0)),
        Token::new(Location::new(ANONYMOUS, 1, 3, 2), Int(1)),
        Token::new(Location::new(ANONYMOUS, 1, 5, 4), Int(0xDEADBEEF)),
        Token::new(Location::new(ANONYMOUS, 1, 16, 15), Int(0b10011001)),
        Token::new(Location::new(ANONYMOUS, 1, 27, 26), Int(0o777)),
        Token::new(Location::new(ANONYMOUS, 1, 33, 32), Float(5.0))
    ]);
}
#[test]
fn strings() {
    test_lex(r#"'a' '\xe9' '\u2023' '\U0001f600' "test" "\"""#, &[
        Token::new(Location::new(ANONYMOUS, 1, 1, 0), Char('a')),
        Token::new(Location::new(ANONYMOUS, 1, 5, 4), Char('é')),
        Token::new(Location::new(ANONYMOUS, 1, 12, 11), Char('‣')),
        Token::new(Location::new(ANONYMOUS, 1, 21, 20), Char('😀')),
        Token::new(Location::new(ANONYMOUS, 1, 34, 33), Str("test".to_string())),
        Token::new(Location::new(ANONYMOUS, 1, 41, 40), Str("\"".to_string()))
    ]);
}
#[test]
fn comments() {
    test_lex("# this is commented
1
#=
this is commented
=#
2
#==
all of this is commented
=#
even after this
==#
3", &[
        Token::new(Location::new(ANONYMOUS, 2, 1, 20), Int(1)),
        Token::new(Location::new(ANONYMOUS, 6, 1, 46), Int(2)),
        Token::new(Location::new(ANONYMOUS, 12, 1, 100), Int(3))
    ]);
}
#[test]
fn macros() {
    test_lex("@a @b(test) @version(major) after", &[
        Token::new(Location::new(ANONYMOUS, 1, 1, 0), Macro("a".to_string(), None)),
        Token::new(Location::new(ANONYMOUS, 1, 4, 3), Macro("b".to_string(), Some("test".to_string()))),
        Token::new(Location::new(ANONYMOUS, 1, 13, 12), Int(0)), // NOTE: this will need to be updated in 1.0.0
        Token::new(Location::new(ANONYMOUS, 1, 29, 28), Identifier("after".to_string()))
    ]);
}
