use std::{iter::Peekable, rc::Rc, str::Chars};

use super::{
    tokens::{
        BinOpToken, Delimiter, Keyword, LiteralToken, Token, TokenKind, UnOpToken, UnOrBinOpToken,
    },
    SourceReader,
};

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

pub struct TokenStream<'src>(pub Rc<Vec<Token<'src>>>);

impl<'src> SourceReader<'src> {
    pub fn tokenize(&mut self) -> (TokenStream<'src>, Vec<TokenizeError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(c) = self.peek_char() {
            match c {
                ' ' | '\n' | '\t' => {
                    self.next_char();
                }

                // An identifier is an xid_start followed by zero or more xid_continue.
                // Match an identifier.
                // TODO: include check for underscore, but then there must be at least
                // one xid_continue after the underscore.
                c if is_xid_start(c) => {
                    let ident_parse_res = self.eat_ident();

                    if let Err(ident_parse_err) = ident_parse_res {
                        errors.push(ident_parse_err);
                        continue;
                    }

                    let ident_token = ident_parse_res.unwrap();

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
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Paren),
                        span: self.source_span_backward(1),
                    });
                }
                ')' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Paren),
                        span: self.source_span_backward(1),
                    });
                }
                '{' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Brace),
                        span: self.source_span_backward(1),
                    });
                }
                '}' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                        span: self.source_span_backward(1),
                    });
                }

                // BINARY OPERATORS
                '=' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::EqEq),
                                span: self.source_span_backward(2),
                            });
                        }
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Eq),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '!' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Neq),
                                span: self.source_span_backward(2),
                            });
                        }
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOp(UnOpToken::Not),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '<' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Leq),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('<') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Shl),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Lt),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '>' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Geq),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('>') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Shr),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Gt),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '&' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('?') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Andq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::And),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '|' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('?') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Orq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Or),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '^' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Xor),
                        span: self.source_span_backward(1),
                    });
                }

                '+' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Add),
                        span: self.source_span_backward(1),
                    });
                }

                '-' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Sub),
                        span: self.source_span_backward(1),
                    });
                }

                '*' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Star),
                        span: self.source_span_backward(1),
                    });
                }

                '/' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Div),
                        span: self.source_span_backward(1),
                    });
                }

                '%' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Mod),
                        span: self.source_span_backward(1),
                    });
                }

                // LITERALS.
                n if n.is_ascii_digit() => {
                    let mut num_len = 1;
                    self.next_char();

                    // Now pointing to the first digit.

                    while let Some(c) = self.peek_char() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit before the non-digit.

                    if self.peek_char() == Some('.') {
                        self.next_char();
                        num_len += 1;
                    } else {
                        // Not a float.
                        tokens.push(Token {
                            kind: TokenKind::Literal(LiteralToken::Int(
                                self.slice_backward(num_len),
                            )),
                            span: self.source_span_backward(num_len),
                        });
                        continue;
                    }

                    // Now pointing to the decimal point.

                    while let Some(c) = self.peek_char() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit of the float.

                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Float(self.slice_backward(num_len))),
                        span: self.source_span_backward(num_len),
                    });
                }

                // MISC.
                ';' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Semicolon,
                        span: self.source_span_backward(1),
                    });
                }

                ':' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Colon,
                        span: self.source_span_backward(1),
                    });
                }

                ',' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Comma,
                        span: self.source_span_backward(1),
                    });
                }

                _ => panic!("Unexpected character: {}", c),
            }
        }

        (TokenStream(Rc::new(tokens)), errors)
    }

    /// Parse an identifier.
    ///
    /// When calling this function, the reader should be pointing to what is
    /// expected to be the charactera immediately preceding the identifier, so
    /// that calling `self.next()` will return the first character of the
    /// identifier.
    ///
    /// After calling this function:
    /// - Success: the reader will be pointing at the last character of the
    /// identifier.
    /// - Failure (wrong first character): the reader will be pointing at the
    /// character after the one it was pointing at when this function was
    /// called.
    fn eat_ident(&mut self) -> Result<Token<'src>, TokenizeError> {
        let mut ident_len: usize = 0;

        if let Some(c) = self.peek_char() {
            if !is_xid_start(c) {
                let err = TokenizeError {
                    kind: TokenizeErrorKind::BadFirstChar,
                    span: self.source_span_backward(1),
                };

                return Err(err);
            }
        } else {
            let err = TokenizeError {
                kind: TokenizeErrorKind::EmptyInput,
                span: self.source_span_backward(0),
            };

            return Err(err);
        }

        self.next_char();
        ident_len += 1;

        // The reader is now pointing at the first character of the identifier.

        while let Some(c) = self.peek_char() {
            if !is_xid_continue(c) {
                // TODO: should also verify c is whitespace, to catch the error
                // of invalid characters in the identifier.

                // NOTE: Not exactly. Doing so would break expressions like `a+b`, which is valid but there is no whitespace.
                break;
            }

            self.next_char();
            ident_len += 1;
        }

        // The reader is now pointing at the last character of the identifier.

        let to_return = Token {
            kind: TokenKind::Ident(self.slice_backward(ident_len)),
            span: self.source_span_backward(ident_len),
        };

        Ok(to_return)
    }
}

#[cfg(test)]
mod tests {
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
        )
    }
}
