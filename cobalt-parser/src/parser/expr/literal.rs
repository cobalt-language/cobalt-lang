use std::{borrow::Cow, collections::HashMap, str::Chars};

use cobalt_ast::{
    ast::{ErrorAST, IntLiteralAST, StringLiteralAST, StructLiteralAST},
    BoxedAST,
};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

use crate::{
    lexer::tokens::{Delimiter, LiteralToken, TokenKind},
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
        assert!(self.current_token.is_some());

        let span = self.current_token.unwrap().span;
        let mut suffix = None;
        let mut errors = vec![];

        match self.current_token.unwrap().kind {
            TokenKind::Literal(LiteralToken::Int(s)) => {
                self.next();

                // Check if the next token is a type hint.

                if self.current_token.is_some() {
                    if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
                        if ident.starts_with('i') {
                            suffix = Some((Cow::from(ident), self.current_token.unwrap().span));
                            self.next();
                        }
                    }
                }

                // Now we want to parse the string `s` to an `i128`. If there is a suffix,
                // we don't want to include this in the string we parse.

                let parsed_int = s.parse::<i128>();
                if parsed_int.is_err() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "integer literal",
                        found: ParserFound::Str(s.to_string()),
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

                if self.current_token.is_some() {
                    if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
                        if ident.starts_with('f') {
                            suffix = Some((Cow::from(ident), self.current_token.unwrap().span));
                            self.next();
                        }
                    }
                }

                // Now we want to parse the string `s` to an `f64`.

                let parsed_float = s.parse::<f64>();
                if parsed_float.is_err() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "float literal",
                        found: ParserFound::Str(s.to_string()),
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

                if let TokenKind::Ident(pf) = self.current_token.unwrap().kind {
                    match pf {
                        "c" => {
                            suffix = Some((Cow::Borrowed(pf), self.current_token.unwrap().span));
                            self.next();
                        }
                        _ => {}
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
            found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
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

        if self.current_token.is_none() {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        if let TokenKind::Ident(_) = self.current_token.unwrap().kind {
        } else {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        self.next();

        // ---

        if self.current_token.is_none() {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        if self.current_token.unwrap().kind != TokenKind::Colon {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        // ---

        self.rewind_to_idx(idx_on_entry);
        true
    }

    /// Going into this function, the current token should be '{'.
    ///
    /// ```text
    /// struct_literal := '{' [ident ':' expr] [',' ident ':' expr]* [',']? '}'
    /// ````
    pub(crate) fn parse_struct_literal(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::OpenDelimiter(Delimiter::Brace));

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;
        self.next();

        // ---

        let mut fields = HashMap::new();
        let mut field_spans: HashMap<Cow<'src, str>, SourceSpan> = HashMap::new();

        let start = 0;
        let atleast_one_field = 1;
        let mut local_state = start;

        loop {
            // Break conditions.

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "'}'",
                    found: ParserFound::Eof,
                    loc: span,
                });
                break;
            }

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Brace) {
                self.next();
                break;
            }

            // If we just parsed a field, then a comma should separate it from the next field.
            if local_state == atleast_one_field {
                if self.current_token.is_none() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: ",",
                        found: ParserFound::Eof,
                        loc: span,
                    });
                    break;
                }

                if self.current_token.unwrap().kind != TokenKind::Comma {
                    errors.push(CobaltError::ExpectedFound {
                        ex: ",",
                        found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
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
            if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
                field_name = Cow::Borrowed(ident);
                field_name_span = self.current_token.unwrap().span;
            } else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: span,
                });
                break;
            }

            if let Some(prev_span) = field_spans.get(&field_name) {
                errors.push(CobaltError::RedefVariable {
                    name: field_name.to_string(),
                    loc: self.current_token.unwrap().span,
                    prev: Some(*prev_span),
                });
                break;
            }

            self.next();

            // Colon.

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: ":",
                    found: ParserFound::Eof,
                    loc: span,
                });
                break;
            }

            if self.current_token.unwrap().kind != TokenKind::Colon {
                errors.push(CobaltError::ExpectedFound {
                    ex: ":",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
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

            local_state = atleast_one_field;
        }

        // ---

        (Box::new(StructLiteralAST::new(span, fields)), errors)
    }

    /// Going into this function, expect current token to be 'LiteralToken::Str'.
    pub(crate) fn parse_string_literal(
        &mut self,
    ) -> (Option<StringLiteralAST<'src>>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let span = self.current_token.unwrap().span;
        let mut errors = vec![];

        let mut chars: Chars;
        if let TokenKind::Literal(LiteralToken::Str(s)) = self.current_token.unwrap().kind {
            chars = s.chars();
        } else {
            self.next();
            errors.push(CobaltError::ExpectedFound {
                ex: "string literal",
                found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                loc: span,
            });
            return (None, errors);
        }

        self.next();

        // ---

        let mut cbi_s: Vec<CharBytesIterator> = vec![];
        loop {
            let cbi = match chars.next() {
                None => break,
                Some('\0') => CharBytesIterator::from_u8(0x00),
                Some('\n') => CharBytesIterator::from_u8(0x0a),
                Some('\r') => CharBytesIterator::from_u8(0x0d),
                Some('\t') => CharBytesIterator::from_u8(0x09),
                Some('\\') => {
                    match chars.next() {
                        None => break,
                        Some('v') => CharBytesIterator::from_u8(0x0a),
                        Some('b') => CharBytesIterator::from_u8(0x08),
                        Some('e') => CharBytesIterator::from_u8(0x1b),
                        Some('a') => CharBytesIterator::from_u8(0x07),
                        //                 just("\\c").ignore_then(
                        //                     text::digits(16)
                        //                         .exactly(2)
                        //                         .slice()
                        //                         .map(|v| Cbi::from_u8(u8::from_str_radix(v, 16).unwrap()))
                        //                         .recover_with(via_parser(empty().to(Cbi::from_u8(0)))),
                        //                 ),
                        Some('c') => todo!(),
                        //                  just("\\x").ignore_then(
                        //                     text::digits(16)
                        //                         .exactly(2)
                        //                         .slice()
                        //                         .map(|v| Cbi::raw(u8::from_str_radix(v, 16).unwrap()))
                        //                         .recover_with(via_parser(empty().to(Cbi::from_u8(0)))),
                        //                 ),
                        Some('x') => todo!(),
                        //                 just("\\u").ignore_then(
                        //                     text::digits(16)
                        //                         .at_least(2)
                        //                         .at_most(6)
                        //                         .slice()
                        //                         .validate(|v, span, e| {
                        //                             let v = u32::from_str_radix(v, 16).unwrap();
                        //                             Cbi::from_u32(v).unwrap_or_else(|| {
                        //                                 e.emit(Rich::custom(
                        //                                     span,
                        //                                     format!("{v:0>4X} is not a valid Unicode codepoint"),
                        //                                 ));
                        //                                 Cbi::from_u8(0)
                        //                             })
                        //                         })
                        //                         .recover_with(skip_until(
                        //                             none_of("}'").ignored(),
                        //                             one_of("}'").ignored(),
                        //                             || Cbi::from_u8(0),
                        //                         ))
                        //                         .delimited_by(just('{'), just('}'))
                        //                         .recover_with(skip_until(
                        //                             none_of("}'").ignored(),
                        //                             one_of("}'").ignored(),
                        //                             || Cbi::from_u8(0),
                        //                         )),
                        //                 ),
                        Some('u') => todo!(),
                        Some(c) => CharBytesIterator::from_char(c),
                    }
                }
                Some(c) => CharBytesIterator::from_char(c),
            };

            cbi_s.push(cbi);
        }

        let bytes: Vec<u8> = cbi_s.into_iter().flatten().collect();

        (Some(StringLiteralAST::new(span, bytes, None)), errors)
    }
}
