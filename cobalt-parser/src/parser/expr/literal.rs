use std::borrow::Cow;

use cobalt_ast::{ast::IntLiteralAST, BoxedAST};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

use crate::{
    lexer::tokens::{LiteralToken, TokenKind},
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
}
