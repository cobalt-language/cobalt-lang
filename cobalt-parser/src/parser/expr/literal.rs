use std::iter::Peekable;
use std::{borrow::Cow, collections::HashMap, str::CharIndices};

use cobalt_ast::ast::CharLiteralAST;
use cobalt_ast::{ast::*, BoxedAST};
use cobalt_errors::{CobaltError, SourceSpan};

use crate::{
    lexer::tokens::{Delimiter, LiteralToken, Token, TokenKind},
    parser::Parser,
    utils::CharBytesIterator,
};

impl<'src> Parser<'src> {
    /// Parses a literal expression, e.g. `1`, `"hello"`, `true`, etc.
    ///
    /// Going into this function, the current token should be the first token
    /// of this grammar.
    ///
    /// ```text
    /// literal := LITERAL
    /// ```
    pub(crate) fn parse_literal(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let current = self.current_token.unwrap();

        let span = current.span;
        let mut suffix = None;
        let mut errors = vec![];

        match current.kind {
            TokenKind::Literal(LiteralToken::Int(s)) => {
                self.next();

                // Check if the next token is a type hint.

                if let Some(current) = self.current_token {
                    if let TokenKind::Ident(ident) = current.kind {
                        suffix = Some((Cow::from(ident), current.span));
                        self.next();
                    }
                }

                // Now we want to parse the string `s` to an `i128`. If there is a suffix,
                // we don't want to include this in the string we parse.

                let parsed_int = s.parse::<i128>();
                if parsed_int.is_err() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "integer literal",
                        found: Some(s.into()),
                        loc: span,
                    });
                }
                let parsed_int = parsed_int.unwrap();

                return (
                    Box::new(IntLiteralAST::new(span, parsed_int, suffix)),
                    errors,
                );
            }

            TokenKind::Literal(LiteralToken::Float(s)) => {
                self.next();

                // Check if the next token is a type hint.

                if let Some(current) = self.current_token {
                    if let TokenKind::Ident(ident) = current.kind {
                        suffix = Some((Cow::from(ident), current.span));
                        self.next();
                    }
                }

                // Now we want to parse the string `s` to an `f64`.

                let parsed_float = s.parse::<f64>();
                if parsed_float.is_err() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "float literal",
                        found: Some(s.into()),
                        loc: span,
                    });
                }
                let parsed_float = parsed_float.unwrap();

                return (
                    Box::new(cobalt_ast::ast::FloatLiteralAST::new(
                        span,
                        parsed_float,
                        suffix,
                    )),
                    errors,
                );
            }

            TokenKind::Literal(LiteralToken::Char(_)) => {
                let parsed_literal = self.parse_char_literal();
                if parsed_literal.0.is_none() {
                    return (Box::new(ErrorAST::new(span)), errors);
                }

                let mut ast = parsed_literal.0.unwrap();
                let errors = parsed_literal.1;

                if self.current_token.is_none() {
                    return (Box::new(ast), errors);
                }

                // --- Check for suffixes.

                let mut suffix: Option<(Cow<'src, str>, SourceSpan)> = None;

                if let Some(current) = self.current_token {
                    if let TokenKind::Ident(ident) = current.kind {
                        suffix = Some((Cow::from(ident), current.span));
                        self.next();
                    }
                }

                ast.suffix = suffix;

                // ---

                return (Box::new(ast), errors);
            }

            TokenKind::Literal(LiteralToken::Str(_)) => {
                let parsed_literal = self.parse_string_literal();
                if parsed_literal.0.is_none() {
                    return (Box::new(ErrorAST::new(span)), errors);
                }

                let mut ast = parsed_literal.0.unwrap();
                let errors = parsed_literal.1;

                if self.current_token.is_none() {
                    return (Box::new(ast), errors);
                }

                // --- Check for suffixes.

                let mut suffix: Option<(Cow<'src, str>, SourceSpan)> = None;

                if let Some(current) = self.current_token {
                    if let TokenKind::Ident(ident) = current.kind {
                        suffix = Some((Cow::from(ident), current.span));
                        self.next();
                    }
                }

                ast.suffix = suffix;

                // ---

                return (Box::new(ast), errors);
            }

            _ => {}
        }

        errors.push(CobaltError::ExpectedFound {
            ex: "literal",
            found: self.current_token.map(|tok| tok.kind.as_str().into()),
            loc: span,
        });
        self.next();
        (Box::new(ErrorAST::new(self.source.len().into())), errors)
    }

    /// Check if the current token begins a struct literal.
    ///
    /// In particular, we look for the following pattern:
    /// ```text
    /// '{' ident ':'
    /// ```
    /// This could be improved to next look for `expr [',' | '}']`.
    pub(crate) fn check_struct_literal(&mut self) -> bool {
        assert!(self.current_token.is_some());

        let idx_on_entry = self.cursor.index;

        // ---

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Brace) {
            return false;
        }

        self.next();

        // ---

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Ident(_),
                ..
            })
        ) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        self.next();

        // ---

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Colon,
                ..
            })
        ) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        // ---
        let _ = self.parse_expr();
        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Comma,
                ..
            })
        ) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        self.rewind_to_idx(idx_on_entry);
        true
    }

    /// Going into this function, the current token should be '{'.
    ///
    /// ```text
    /// struct_literal := '{' [ident ':' expr] [',' ident ':' expr]* [',']? '}'
    /// ````
    pub(crate) fn parse_struct_literal(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let Some(Token {
            kind: TokenKind::OpenDelimiter(Delimiter::Brace),
            span,
        }) = self.current_token
        else {
            unreachable!()
        };

        let mut errors = vec![];
        self.next();

        // ---

        let mut fields = HashMap::new();
        let mut field_spans: HashMap<Cow<'src, str>, SourceSpan> = HashMap::new();

        let start = 0;
        let at_least_one_field = 1;
        let mut local_state = start;

        loop {
            // Break conditions.

            let Some(current) = self.current_token else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "'}'",
                    found: None,
                    loc: span,
                });
                break;
            };

            if current.kind == TokenKind::CloseDelimiter(Delimiter::Brace) {
                self.next();
                break;
            }

            // If we just parsed a field, then a comma should separate it from the next field.
            if local_state == at_least_one_field {
                let Some(current) = self.current_token else {
                    errors.push(CobaltError::ExpectedFound {
                        ex: ",",
                        found: None,
                        loc: span,
                    });
                    break;
                };

                if current.kind != TokenKind::Comma {
                    errors.push(CobaltError::ExpectedFound {
                        ex: ",",
                        found: Some(self.current_token.unwrap().kind.as_str().into()),
                        loc: span,
                    });
                    break;
                }

                self.next();

                // The next token is allowed to be a brace, in which case break.
                if self.current_token.is_some()
                    && self.current_token.unwrap().kind
                        == TokenKind::CloseDelimiter(Delimiter::Brace)
                {
                    self.next();
                    break;
                }
            }

            // Field name.

            let field_name: Cow<'_, str>;
            let field_name_span: SourceSpan;
            let current = self.current_token.unwrap();
            if let TokenKind::Ident(ident) = current.kind {
                field_name = Cow::Borrowed(ident);
                field_name_span = current.span;
            } else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: Some(current.kind.as_str().into()),
                    loc: span,
                });
                break;
            }

            if let Some(prev_span) = field_spans.get(&field_name) {
                errors.push(CobaltError::RedefVariable {
                    name: field_name.to_string(),
                    loc: current.span,
                    prev: Some(*prev_span),
                });
                break;
            }

            self.next();

            // Colon.

            let Some(current) = self.current_token else {
                errors.push(CobaltError::ExpectedFound {
                    ex: ":",
                    found: None,
                    loc: span,
                });
                break;
            };

            if current.kind != TokenKind::Colon {
                errors.push(CobaltError::ExpectedFound {
                    ex: ":",
                    found: Some(current.kind.as_str().into()),
                    loc: span,
                });
                break;
            }

            self.next();

            // Expr.

            let (expr, expr_errors) = self.parse_expr();
            errors.extend(expr_errors);

            fields.insert(field_name.clone(), expr);
            field_spans.insert(field_name, field_name_span);

            local_state = at_least_one_field;
        }

        // ---

        (Box::new(StructLiteralAST::new(span, fields)), errors)
    }

    /// Going into this function, expect current token to be 'LiteralToken::Char'.
    pub(crate) fn parse_char_literal(
        &mut self,
    ) -> (Option<CharLiteralAST<'src>>, Vec<CobaltError<'src>>) {
        let current = self.current_token.unwrap();

        let span = current.span;
        let mut errors = vec![];

        let TokenKind::Literal(LiteralToken::Char(s)) = self.current_token.unwrap().kind else {
            self.next();
            errors.push(CobaltError::ExpectedFound {
                ex: "char literal",
                found: Some(self.current_token.unwrap().kind.as_str().into()),
                loc: span,
            });
            return (None, errors);
        };

        let mut char_indices = s[1..(s.len() - 1)].char_indices().peekable();

        self.next();

        // ---

        let val: u32 = match char_indices.next() {
            None => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "a char",
                    found: None,
                    loc: span,
                });
                0
            }
            Some((_, '\\')) => {
                match char_indices.next() {
                    None => {
                        errors.push(CobaltError::ExpectedFound {
                            ex: "a char",
                            found: None,
                            loc: span,
                        });
                        0
                    }
                    Some((_, '0')) => 0x00,
                    Some((_, 'n')) => 0x0a,
                    Some((_, 'r')) => 0x0d,
                    Some((_, 't')) => 0x09,
                    Some((_, 'v')) => 0x0a,
                    Some((_, 'b')) => 0x08,
                    Some((_, 'e')) => 0x1b,
                    Some((_, 'a')) => 0x07,
                    Some((offset, 'c')) => {
                        // --- Next are exactly two hex digits.

                        let parsed_hex = parse_hex_literal(&mut char_indices, 2, 2);

                        parsed_hex.unwrap_or_else(|| {
                            errors.push(CobaltError::InvalidThing {
                                ex: "two hex digits",
                                loc: SourceSpan::from((span.offset() + offset, 1)),
                            });
                            0
                        })
                    }
                    Some((offset, 'u')) => match parse_unicode_literal(&mut char_indices) {
                        Ok(res) => res.explode().0,
                        Err(ParseUnicodeLiteralError::HexIsNotValidUnicode) => {
                            errors.push(CobaltError::InvalidUnicodeLiteral {
                                loc: SourceSpan::from((span.offset() + offset, 1)),
                            });
                            0
                        }
                        Err(ParseUnicodeLiteralError::MissingOpenDelim) => {
                            errors.push(CobaltError::ExpectedHere {
                                ex: "'{'",
                                loc: SourceSpan::from((span.offset() + offset, 1)),
                            });
                            0
                        }
                        Err(ParseUnicodeLiteralError::MissingClosingDelim) => {
                            errors.push(CobaltError::ExpectedHere {
                                ex: "'}'",
                                loc: SourceSpan::from((span.offset() + offset, 1)),
                            });
                            0
                        }
                        Err(ParseUnicodeLiteralError::FailedToParseHex) => {
                            errors.push(CobaltError::InvalidThing {
                                ex: "hex literal",
                                loc: SourceSpan::from((span.offset() + offset, 1)),
                            });
                            0
                        }
                    },
                    Some((_, c)) => u32::from(c),
                }
            }
            Some((_, c)) => u32::from(c),
        };

        (Some(CharLiteralAST::new(span, val, None)), errors)
    }

    /// Going into this function, expect current token to be 'LiteralToken::Str'.
    pub(crate) fn parse_string_literal(
        &mut self,
    ) -> (Option<StringLiteralAST<'src>>, Vec<CobaltError<'src>>) {
        let current = self.current_token.unwrap();

        let span = current.span;
        let mut errors = vec![];

        let TokenKind::Literal(LiteralToken::Str(s)) = current.kind else {
            self.next();
            errors.push(CobaltError::ExpectedFound {
                ex: "string literal",
                found: Some(self.current_token.unwrap().kind.as_str().into()),
                loc: span,
            });
            return (None, errors);
        };
        let mut char_indices = s[1..(s.len() - 1)].char_indices().peekable();

        self.next();

        // ---
        let mut bytes = vec![];
        loop {
            let cbi = match char_indices.next() {
                None => break,
                Some((_, '\\')) => {
                    match char_indices.next() {
                        None => break,
                        Some((_, '0')) => CharBytesIterator::from_u8(0x00),
                        Some((_, 'n')) => CharBytesIterator::from_u8(0x0a),
                        Some((_, 'r')) => CharBytesIterator::from_u8(0x0d),
                        Some((_, 't')) => CharBytesIterator::from_u8(0x09),
                        Some((_, 'v')) => CharBytesIterator::from_u8(0x0a),
                        Some((_, 'b')) => CharBytesIterator::from_u8(0x08),
                        Some((_, 'e')) => CharBytesIterator::from_u8(0x1b),
                        Some((_, 'a')) => CharBytesIterator::from_u8(0x07),
                        Some((offset, 'c')) => {
                            // --- Next are exactly two hex digits.

                            let parsed_hex = parse_hex_literal(&mut char_indices, 2, 2);

                            let byte = parsed_hex.unwrap_or_else(|| {
                                errors.push(CobaltError::InvalidThing {
                                    ex: "two hex digits",
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                0
                            });
                            CharBytesIterator::from_u8(byte as _)
                        }
                        // This only differs from `\c` by ultimately calling `Cbi::raw` instead of `Cbi::from_u8`.
                        Some((offset, 'x')) => {
                            // --- Next are exactly two hex digits.

                            let parsed_hex = parse_hex_literal(&mut char_indices, 2, 2);

                            let byte = parsed_hex.unwrap_or_else(|| {
                                errors.push(CobaltError::InvalidThing {
                                    ex: "two hex digits",
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                0
                            });
                            CharBytesIterator::raw(byte as _)
                        }
                        Some((offset, 'u')) => match parse_unicode_literal(&mut char_indices) {
                            Ok(res) => res,
                            Err(ParseUnicodeLiteralError::HexIsNotValidUnicode) => {
                                errors.push(CobaltError::InvalidUnicodeLiteral {
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                CharBytesIterator::from_u8(0)
                            }
                            Err(ParseUnicodeLiteralError::MissingOpenDelim) => {
                                errors.push(CobaltError::ExpectedHere {
                                    ex: "'{'",
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                CharBytesIterator::from_u8(0)
                            }
                            Err(ParseUnicodeLiteralError::MissingClosingDelim) => {
                                errors.push(CobaltError::ExpectedHere {
                                    ex: "'}'",
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                CharBytesIterator::from_u8(0)
                            }
                            Err(ParseUnicodeLiteralError::FailedToParseHex) => {
                                errors.push(CobaltError::InvalidThing {
                                    ex: "hex literal",
                                    loc: SourceSpan::from((span.offset() + offset, 1)),
                                });
                                CharBytesIterator::from_u8(0)
                            }
                        },
                        Some((_, c)) => CharBytesIterator::from_char(c),
                    }
                }
                Some((_, c)) => CharBytesIterator::from_char(c),
            };
            bytes.extend(cbi);
        }

        (Some(StringLiteralAST::new(span, bytes, None)), errors)
    }
}

/// Parse 2-6 hex digits as a unicode value, delimited by '{}'.
fn parse_hex_literal(
    chars: &mut Peekable<CharIndices>,
    min_num_digits: u8,
    max_num_digits: u8,
) -> Option<u32> {
    assert!(min_num_digits >= 2 && max_num_digits <= 6 && min_num_digits <= max_num_digits);

    let mut out = 0u32;
    let mut count = 0;
    for _ in 0..max_num_digits {
        let Some(&(_idx, ch)) = chars.peek() else {
            break;
        };
        let Some(x) = ch.to_digit(16) else {
            break;
        };
        out <<= 4;
        out |= x;

        chars.next();
        count += 1;
    }

    // monadic stuff may be slightly slower
    if count < min_num_digits {
        None
    } else {
        Some(out)
    }
}

enum ParseUnicodeLiteralError {
    MissingOpenDelim,
    MissingClosingDelim,
    FailedToParseHex,
    HexIsNotValidUnicode,
}

fn parse_unicode_literal(
    chars: &mut Peekable<CharIndices>,
) -> Result<CharBytesIterator, ParseUnicodeLiteralError> {
    match chars.next() {
        Some((_, '{')) => {}
        _ => {
            loop {
                match chars.next() {
                    None => {
                        break;
                    }
                    Some((_, '}')) => {
                        break;
                    }
                    _ => {}
                }
            }

            return Err(ParseUnicodeLiteralError::MissingOpenDelim);
        }
    }

    let parsed_hex = parse_hex_literal(chars, 2, 6);
    if parsed_hex.is_none() {
        return Err(ParseUnicodeLiteralError::FailedToParseHex);
    }

    match chars.next() {
        Some((_, '}')) => {}
        _ => {
            loop {
                match chars.next() {
                    None => break,
                    Some((_, '}')) => {
                        break;
                    }
                    _ => {}
                }
            }

            return Err(ParseUnicodeLiteralError::MissingClosingDelim);
        }
    }

    CharBytesIterator::from_u32(parsed_hex.unwrap())
        .ok_or(ParseUnicodeLiteralError::HexIsNotValidUnicode)
}
