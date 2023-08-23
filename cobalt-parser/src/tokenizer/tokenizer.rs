use std::{iter::Peekable, rc::Rc, str::Chars};

use super::tokens::{BinOpToken, Delimiter, Keyword, Token, TokenKind, UnOpToken};

use cobalt_errors::SourceSpan;
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, PartialEq)]
pub enum TokenizeErrorKind {
    EmptyInput,
    BadFirstChar,
    UnexpectedCharacter(char),
}

#[derive(Debug)]
pub struct TokenizeError {
    pub kind: TokenizeErrorKind,
    pub span: SourceSpan,
}

pub fn is_ignored_char(c: char) -> bool {
    c.is_whitespace()
}

/// Consume ignored characters until we hit a non-ignored character. We
/// do not eat the first non-ignored character.
pub fn eat_ignored(input: &mut Peekable<Chars>) {
    while let Some(c) = input.peek() {
        if !is_ignored_char(*c) {
            break;
        }

        input.next();
    }
}

/// Consume characters until we hit an ignored character. We do not eat
/// the first ignored character.
pub fn eat_until_ignored(input: &mut Peekable<Chars>) {
    while let Some(c) = input.peek() {
        if is_ignored_char(*c) {
            break;
        }

        input.next();
    }
}

fn eat_ident(
    input: &mut Peekable<Chars>,
    source_span_start: usize,
) -> Result<Token, TokenizeError> {
    let mut ident = String::new();

    if let Some(c) = input.peek() {
        if !is_xid_start(*c) {
            let err = TokenizeError {
                kind: TokenizeErrorKind::BadFirstChar,
                span: SourceSpan::from((source_span_start, source_span_start + 1)),
            };

            return Err(err);
        }

        ident.push(*c);
        input.next();
    } else {
        let err = TokenizeError {
            kind: TokenizeErrorKind::EmptyInput,
            span: SourceSpan::from((source_span_start, source_span_start)),
        };

        return Err(err);
    }

    while let Some(c) = input.peek() {
        if !is_xid_continue(*c) {
            break;
        }

        ident.push(*c);
        input.next();
    }

    let source_span_end = source_span_start + ident.len();
    let to_return = Token {
        kind: TokenKind::Ident(ident),
        span: SourceSpan::from((source_span_start, source_span_end)),
    };

    Ok(to_return)
}

pub struct TokenStream(Rc<Vec<Token>>);

pub fn tokenize(input: Chars, source_span_start: usize) -> (TokenStream, Vec<TokenizeError>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut input = input.peekable();
    let mut current_source_loc = source_span_start;

    while let Some(c) = input.peek() {
        match c {
            ' ' | '\n' | '\t' => {
                input.next();
            }

            // An identifier is an xid_start followed by zero or more xid_continue.
            // Match an identifier.
            c if is_xid_start(*c) => {
                let ident_parse_res = eat_ident(&mut input, current_source_loc);
                if let Err(ident_parse_err) = ident_parse_res {
                    current_source_loc += &ident_parse_err.span.len();
                    errors.push(ident_parse_err);
                    continue;
                }

                let ident_token = ident_parse_res.unwrap();
                current_source_loc += ident_token.span.len();

                // Check if the identifier is reserved.
                if let Some(keyword) = Keyword::from_token(&ident_token) {
                    tokens.push(Token {
                        kind: TokenKind::Keyword(keyword),
                        span: ident_token.span,
                    });
                } else {
                    tokens.push(ident_token);
                }
            }

            // Delimiters.
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::OpenDelimiter(Delimiter::Paren),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::CloseDelimiter(Delimiter::Paren),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }
            '{' => {
                tokens.push(Token {
                    kind: TokenKind::OpenDelimiter(Delimiter::Brace),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }
            '}' => {
                tokens.push(Token {
                    kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            // BINARY OPERATORS
            '=' => {
                input.next();
                match input.peek() {
                    Some('=') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::EqEq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }
                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Eq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '!' => {
                input.next();
                match input.peek() {
                    Some('=') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Neq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }
                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::UnOp(UnOpToken::Not),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '<' => {
                input.next();
                match input.peek() {
                    Some('=') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Leq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    Some('<') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Shl),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Lt),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '>' => {
                input.next();
                match input.peek() {
                    Some('=') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Geq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    Some('>') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Shr),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Gt),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '&' => {
                input.next();
                match input.peek() {
                    Some('&') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::AndAnd),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    Some('?') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Andq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::And),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '|' => {
                input.next();
                match input.peek() {
                    Some('|') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::OrOr),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    Some('?') => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Orq),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 2)),
                        });
                        input.next();
                        current_source_loc += 2;
                    }

                    _ => {
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Or),
                            span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                        });
                        current_source_loc += 1;
                    }
                }
            }

            '^' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Xor),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            '+' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Add),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            '-' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Sub),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            '*' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Mul),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            '/' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Div),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            '%' => {
                tokens.push(Token {
                    kind: TokenKind::BinOp(BinOpToken::Mod),
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            // MISC.
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::Semicolon,
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            ':' => {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            ',' => {
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    span: SourceSpan::from((current_source_loc, current_source_loc + 1)),
                });
                input.next();
                current_source_loc += 1;
            }

            _ => panic!("Unexpected character: {}", c),
        }
    }

    (TokenStream(Rc::new(tokens)), errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_decls() {
        let two_p_rt = "fn foo6(a: i32, b: i32): i32;";
        let (tokens, errors) = tokenize(two_p_rt.chars(), 0);

        assert_eq!(
            tokens.0.as_ref(),
            &vec![
                Token {
                    kind: TokenKind::Keyword(Keyword::Fn),
                    span: SourceSpan::from((0, 2)),
                },
                Token {
                    kind: TokenKind::Ident("foo6".to_string()),
                    span: SourceSpan::from((3, 7)),
                },
                Token {
                    kind: TokenKind::OpenDelimiter(Delimiter::Paren),
                    span: SourceSpan::from((7, 8)),
                },
                Token {
                    kind: TokenKind::Ident("a".to_string()),
                    span: SourceSpan::from((8, 9)),
                },
                Token {
                    kind: TokenKind::Colon,
                    span: SourceSpan::from((9, 10)),
                },
                Token {
                    kind: TokenKind::Ident("i32".to_string()),
                    span: SourceSpan::from((11, 14)),
                },
                Token {
                    kind: TokenKind::Comma,
                    span: SourceSpan::from((14, 15)),
                },
                Token {
                    kind: TokenKind::Ident("b".to_string()),
                    span: SourceSpan::from((16, 17)),
                },
                Token {
                    kind: TokenKind::Colon,
                    span: SourceSpan::from((17, 18)),
                },
                Token {
                    kind: TokenKind::Ident("i32".to_string()),
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
                    kind: TokenKind::Ident("i32".to_string()),
                    span: SourceSpan::from((25, 28)),
                },
                Token {
                    kind: TokenKind::Semicolon,
                    span: SourceSpan::from((28, 29)),
                },
            ]
        );
    }
}
