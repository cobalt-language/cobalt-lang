use cobalt_errors::SourceSpan;

use crate::lexer::tokens::*;

use super::*;

#[test]
fn test_fn_decls() {
    let two_p_rt = "fn foo6(a: i32, b: i32): i32;";

    let mut string_reader = SourceReader::new(two_p_rt);

    let (tokens, _errors) = string_reader.tokenize();

    assert_eq!(
        tokens.0.as_ref(),
        &vec![
            Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                span: SourceSpan::from((0, 2)),
            },
            Token {
                kind: TokenKind::Ident("foo6"),
                span: SourceSpan::from((3, 7)),
            },
            Token {
                kind: TokenKind::OpenDelimiter(Delimiter::Paren),
                span: SourceSpan::from((7, 8)),
            },
            Token {
                kind: TokenKind::Ident("a"),
                span: SourceSpan::from((8, 9)),
            },
            Token {
                kind: TokenKind::Colon,
                span: SourceSpan::from((9, 10)),
            },
            Token {
                kind: TokenKind::Ident("i32"),
                span: SourceSpan::from((11, 14)),
            },
            Token {
                kind: TokenKind::Comma,
                span: SourceSpan::from((14, 15)),
            },
            Token {
                kind: TokenKind::Ident("b"),
                span: SourceSpan::from((16, 17)),
            },
            Token {
                kind: TokenKind::Colon,
                span: SourceSpan::from((17, 18)),
            },
            Token {
                kind: TokenKind::Ident("i32"),
                span: SourceSpan::from((19, 22)),
            },
            Token {
                kind: TokenKind::CloseDelimiter(Delimiter::Paren),
                span: SourceSpan::from((22, 23)),
            },
            Token {
                kind: TokenKind::Colon,
                span: SourceSpan::from((23, 24)),
            },
            Token {
                kind: TokenKind::Ident("i32"),
                span: SourceSpan::from((25, 28)),
            },
            Token {
                kind: TokenKind::Semicolon,
                span: SourceSpan::from((28, 29)),
            },
        ]
    );
}

#[test]
fn test_literals() {
    let int_lit = "let a = 123;";
    let mut string_reader = SourceReader::new(int_lit);

    let (tokens, _errors) = string_reader.tokenize();

    assert_eq!(
        tokens.0.as_ref(),
        &vec![
            Token {
                kind: TokenKind::Keyword(Keyword::Let),
                span: SourceSpan::from((0, 3)),
            },
            Token {
                kind: TokenKind::Ident("a"),
                span: SourceSpan::from((4, 1)),
            },
            Token {
                kind: TokenKind::BinOp(BinOpToken::Eq),
                span: SourceSpan::from((6, 1)),
            },
            Token {
                kind: TokenKind::Literal(LiteralToken::Int("123")),
                span: SourceSpan::from((8, 3)),
            },
            Token {
                kind: TokenKind::Semicolon,
                span: SourceSpan::from((11, 1)),
            },
        ]
    );

    let str_literal = r#""hello""#;
    let mut string_reader = SourceReader::new(str_literal);
    let (tokens, _errors) = string_reader.tokenize();
    assert_eq!(
        tokens.0.as_ref(),
        &vec![Token {
            kind: TokenKind::Literal(LiteralToken::Str("\"hello\"")),
            span: SourceSpan::from((0, 0))
        }]
    );

    let str_literal = r#""hello \"world\"""#;
    let mut string_reader = SourceReader::new(str_literal);
    let (tokens, _errors) = string_reader.tokenize();
    assert_eq!(
        tokens.0.as_ref(),
        &vec![Token {
            kind: TokenKind::Literal(LiteralToken::Str("\"hello \\\"world\\\"\"")),
            span: SourceSpan::from((0, 0))
        }]
    );
}

#[test]
fn test_comments() {
    // ---

    let src = "Something # else\nOk\n#comment \nmore";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert_eq!(
        tokens.0.as_ref(),
        &vec![
            Token {
                kind: TokenKind::Ident("Something"),
                span: SourceSpan::from((0, 8)),
            },
            Token {
                kind: TokenKind::Ident("Ok"),
                span: SourceSpan::from((9, 1)),
            },
            Token {
                kind: TokenKind::Ident("more"),
                span: SourceSpan::from((0, 1)),
            }
        ]
    );

    // ---

    let src = "#= bla bla =#";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert!(tokens.0.is_empty());

    // ---

    let src = "#=== bla bla =========#";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert!(tokens.0.is_empty());

    // ---

    let src = "#=== bla bla =# aadsf ===#";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert!(tokens.0.is_empty());

    // ---

    let src = "#=== bla bla == =# bla";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert!(tokens.0.is_empty());

    // ---

    let src = "#===\nbla bla\n===# bla";
    let mut string_reader = SourceReader::new(src);

    let (tokens, _errors) = string_reader.tokenize();

    assert_eq!(
        tokens.0.as_ref(),
        &vec![Token {
            kind: TokenKind::Ident("bla"),
            span: SourceSpan::from((0, 0)),
        },]
    );
}
