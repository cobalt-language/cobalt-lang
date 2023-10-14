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

        while let Some(c) = self.peek() {
            match c {
                ' ' | '\n' | '\t' => {
                    self.next_char();
                }

                '#' => {
                    if let Err(err) = self.eat_comment() {
                        errors.push(err);
                    }
                }

                // An identifier is an xid_start followed by zero or more xid_continue.
                // Match an identifier.
                // TODO: include check for underscore, but then there must be at least
                // one xid_continue after the underscore.
                c if is_xid_start(*c) || *c == '_' => {
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
                '[' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Bracket),
                        span: self.source_span_backward(1),
                    });
                }
                ']' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Bracket),
                        span: self.source_span_backward(1),
                    });
                }

                // UNARY OPERATORS
                '?' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::UnOp(UnOpToken::Q),
                        span: self.source_span_backward(1),
                    });
                }

                // BINARY OPERATORS
                '=' => {
                    self.next_char();
                    match self.peek() {
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
                    match self.peek() {
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
                    match self.peek() {
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
                    match self.peek() {
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
                    match self.peek() {
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
                    match self.peek() {
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
                    match self.peek() {
                        Some('+') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::UnOp(UnOpToken::PlusPlus),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Add),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
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

                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit before the non-digit.

                    if self.peek() == Some(&'.') {
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

                    while let Some(c) = self.peek() {
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

                    if self.peek() == Some(&'?') {
                        self.next_char();
                        tokens.push(Token {
                            kind: TokenKind::BinOp(BinOpToken::Colonq),
                            span: self.source_span_backward(2),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::Colon,
                            span: self.source_span_backward(1),
                        });
                    }
                }

                ',' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Comma,
                        span: self.source_span_backward(1),
                    });
                }
                '.' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Dot,
                        span: self.source_span_backward(1),
                    });
                }
                '@' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::At,
                        span: self.source_span_backward(1),
                    });
                }

                // --- String literal.
                '"' => {
                    self.next_char();

                    let span_start = self.index;

                    let mut last_was_escape = false;
                    loop {
                        let c = self.peek();
                        if c.is_none() {
                            todo!()
                        }
                        let c = c.unwrap();

                        if c == &'\\' {
                            last_was_escape = !last_was_escape;
                            self.next_char();
                            continue;
                        }

                        if c == &'"' && !last_was_escape {
                            break;
                        }

                        last_was_escape = false;

                        self.next_char();
                    }

                    let span_end = self.index;
                    self.next_char();

                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Str(
                            &self.source[span_start..span_end],
                        )),
                        span: SourceSpan::from((span_start, span_end - span_start)),
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
        let start_idx = self.index;

        if let Some(c) = self.peek() {
            if !(is_xid_start(*c) || *c == '_') {
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

        // The reader is now pointing at the first character of the identifier.

        while let Some(c) = self.peek() {
            if !is_xid_continue(*c) {
                break;
            }

            self.next_char();
        }

        // The reader is now pointing at the last character of the identifier.

        let to_return = Token {
            kind: TokenKind::Ident(self.slice_from(start_idx)),
            span: self.source_span_from(start_idx),
        };

        Ok(to_return)
    }

    fn eat_comment(&mut self) -> Result<(), TokenizeError> {
        assert!(self.next_char() == Some('#'));

        let mut multiline_level = 0;
        loop {
            match self.next_char() {
                Some('=') => {
                    multiline_level += 1;
                }
                _ => break,
            }
        }

        if multiline_level == 0 {
            // Single line comment.
            loop {
                match self.next_char() {
                    None => break,
                    Some('\n') => break,
                    _ => {}
                }
            }

            return Ok(());
        }

        // Multi-line comment.

        let mut num_levels_eaten = 0;
        loop {
            match self.next_char() {
                Some('=') => {
                    num_levels_eaten += 1;
                }
                Some('#') => {
                    if num_levels_eaten < multiline_level {
                        num_levels_eaten = 0;
                    } else {
                        return Ok(());
                    }
                }
                Some(_) => {
                    num_levels_eaten = 0;
                }
                None => {
                    return Err(TokenizeError {
                        kind: TokenizeErrorKind::EmptyInput,
                        span: self.index.into(),
                    }); // TODO: return a better error?
                }
            }
        }
    }
}
