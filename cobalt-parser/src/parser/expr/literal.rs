use std::{borrow::Cow, collections::HashMap};

use cobalt_ast::{
    ast::{IntLiteralAST, StructLiteralAST},
    BoxedAST,
};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

use crate::{
    lexer::tokens::{Delimiter, LiteralToken, TokenKind},
    parser::Parser,
};

impl<'src> Parser<'src> {
    /// Parses a literal expression, e.g. `1`, `"hello"`, `true`, etc.
    ///
    /// Going into this function, the current token should be the first token
    /// of this grammar.
    ///
    /// ```
    /// literal := LITERAL
    /// ```
    pub fn parse_literal(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
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

            _ => {}
        }

        errors.push(CobaltError::ExpectedFound {
            ex: "literal",
            found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
            loc: span,
        });
        (
            Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
            errors,
        )
    }

    /// Going into this function, the current token should be '{'.
    ///
    /// ```
    /// struct_literal := '{' [ident ':' expr] [',' ident ':' expr]* [',']? '}'
    /// ````
    pub fn parse_struct_literal(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::SourceReader;

    #[test]
    fn test_parse_literal() {
        let src = "1";
        let mut src_reader = SourceReader::new(src);
        let token_strem = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_strem);
        parser.next();
        let (ast, errors) = parser.parse_literal();
        assert!(errors.is_empty());
        dbg!(ast);

        let src = "1i32";
        let mut src_reader = SourceReader::new(src);
        let token_strem = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_strem);
        parser.next();
        let (ast, errors) = parser.parse_literal();
        assert!(errors.is_empty());
        dbg!(ast);

        let src = "1.0";
        let mut src_reader = SourceReader::new(src);
        let token_strem = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_strem);
        parser.next();
        let (ast, errors) = parser.parse_literal();
        assert!(errors.is_empty());
        dbg!(ast);

        let src = "1.0f32";
        let mut src_reader = SourceReader::new(src);
        let token_strem = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_strem);
        parser.next();
        let (ast, errors) = parser.parse_literal();
        assert!(errors.is_empty());
        dbg!(ast);
    }

    #[test]
    fn test_parse_struct_literal() {
        let src = "{size: u32, offset: u16}";
        let mut src_reader = SourceReader::new(src);
        let token_strem = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_strem);
        parser.next();
        let (ast, errors) = parser.parse_struct_literal();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }
}
