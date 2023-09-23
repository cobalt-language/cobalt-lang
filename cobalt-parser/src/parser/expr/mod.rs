use std::borrow::Cow;

use cobalt_ast::{
    ast::{BlockAST, NullAST, PrefixAST, VarGetAST},
    BoxedAST,
};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

use crate::lexer::tokens::{Delimiter, Keyword, TokenKind, UnOpToken, UnOrBinOpToken};

use super::Parser;

mod binop_rhs;
mod literal;

impl<'src> Parser<'src> {
    /// Parse an expression.
    ///
    /// ```
    /// expr
    ///    := primary_expr [BINOP primary_expr]*
    /// ```
    pub fn parse_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let lhs = self.parse_primary_expr();

        if let Some(next_tok) = self.current_token {
            if let TokenKind::BinOp(_) = next_tok.kind {
                return self.parse_binop_rhs(0, lhs.0, lhs.1);
            }

            if let TokenKind::UnOrBinOp(_) = next_tok.kind {
                return self.parse_binop_rhs(0, lhs.0, lhs.1);
            }
        }

        lhs
    }

    /// Parse a primary expression. These are basically anything that can be (directly)
    /// on the left or right of a binary operator. For example:
    /// - Consider `a + b`. Both `a` and `b` are primary expressions.
    /// - Consider `a + b + c`. `a`, 'b', and 'c' are primary expressions.
    /// - Consider `(a + b) + c`. `(a + b)` and `c` are primary expressions. Note that
    /// `(a + b)` will be found to have primary expressions `a` and `b` upon being parsed
    /// recursively, but this is not the concern of the top level parsing.
    ///
    /// ```
    /// primary_expr
    ///    := ident_expr
    ///    := literal
    ///    := paren_expr
    ///    := block_expr
    ///    := prefix_expr
    /// ```
    fn parse_primary_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        match self.current_token.unwrap().kind {
            TokenKind::Literal(_) => {
                return self.parse_literal();
            }
            TokenKind::Ident(_) => {
                return self.parse_ident_expr();
            }
            TokenKind::OpenDelimiter(p) if p == Delimiter::Paren => {
                return self.parse_paren_expr();
            }
            TokenKind::OpenDelimiter(b) if b == Delimiter::Brace => {
                return self.parse_block_expr();
            }
            TokenKind::UnOp(_) => {
                return self.parse_prefix_expr();
            }
            TokenKind::UnOrBinOp(_) => {
                return self.parse_prefix_expr();
            }
            _ => {}
        }

        (
            Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
            vec![CobaltError::ExpectedFound {
                ex: "expression",
                found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                loc: self.current_token.unwrap().span,
            }],
        )
    }

    fn parse_ident_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let errors = vec![];

        let span = self.current_token.unwrap().span;
        let name = match self.current_token.unwrap().kind {
            TokenKind::Ident(name) => Cow::from(name),
            _ => {
                return (
                    Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                    vec![CobaltError::ExpectedFound {
                        ex: "identifier",
                        found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                        loc: span,
                    }],
                );
            }
        };
        let is_global = false;

        self.next();

        return (Box::new(VarGetAST::new(span, name, is_global)), errors);
    }

    /// Going into this function, `current_token` is assumed to be a unary operator.
    ///
    /// ```
    /// prefix_expr := [UNOP] primary_expr
    /// ```
    fn parse_prefix_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        let op = match self.current_token.unwrap().kind {
            TokenKind::UnOrBinOp(op) => match op {
                UnOrBinOpToken::And => "&",
                UnOrBinOpToken::Star => "*",
            },
            TokenKind::UnOp(op) => match op {
                UnOpToken::Not => "!",
            },
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "unary operator",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });
                return (
                    Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                    errors,
                );
            }
        };

        // Eat the operator.
        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "primary expression",
                found: ParserFound::Eof,
                loc: span,
            });
            return (
                Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                errors,
            );
        }

        let (val, val_errors) = self.parse_primary_expr();
        errors.extend(val_errors);

        return (Box::new(PrefixAST::new(span, op, val)), errors);
    }

    /// Going into this function, `current_token` is assumed to be an open paren.
    ///
    /// ```
    /// paren_expr := '(' expr ')'
    /// ```
    fn parse_paren_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: ParserFound::Eof,
                loc: span,
            });
            return (
                Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                errors,
            );
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: ParserFound::Eof,
                loc: span,
            });
            return (
                Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                errors,
            );
        }

        if self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Paren) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found,
                loc,
            });
            return (
                Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                errors,
            );
        }

        self.next();
        (expr, errors)
    }

    /// Going into this function, `current_token` is assumed to be a open brace.
    ///
    /// ```
    /// block_expr := '{' [ expr? ';' | decl ]* '}'
    /// ```
    fn parse_block_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::OpenDelimiter(Delimiter::Brace));

        let span_start = self.current_token.unwrap().span.offset();
        let mut span_len = self.current_token.unwrap().span.len();
        let mut vals: Vec<BoxedAST<'src>> = vec![];

        let mut errors: Vec<CobaltError<'src>> = vec![];

        // Eat the opening brace.
        self.next();

        let start = 0;
        let last_was_decl = 1;
        let semicolon_trailing_expr = 2;
        let last_was_expr = 3;
        let mut local_state = start;

        loop {
            if self.current_token.is_none() {
                return (
                    Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((0, 1)))),
                    vec![CobaltError::ExpectedFound {
                        ex: "'}'",
                        found: ParserFound::Eof,
                        loc: self.current_token.unwrap().span,
                    }],
                );
            }

            span_len += self.current_token.unwrap().span.len();

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Brace) {
                break;
            }

            // If it's just a semicolon that's ok.
            if self.current_token.unwrap().kind == TokenKind::Semicolon {
                self.next();
                if local_state == last_was_expr {
                    local_state = semicolon_trailing_expr;
                }
                continue;
            }

            if let TokenKind::Keyword(kw) = self.current_token.unwrap().kind {
                if kw == Keyword::Let || kw == Keyword::Type || kw == Keyword::Fn {
                    let (decl, decl_errors) = self.parse_decl();
                    errors.extend(decl_errors);
                    vals.push(decl);

                    local_state = last_was_decl;
                    continue;
                }
            }

            let (expr, expr_errors) = self.parse_expr();
            errors.extend(expr_errors);
            vals.push(expr);
            local_state = last_was_expr;
        }

        // Eat the closing brace.
        self.next();

        // If the last val was an expr followed by a semicolon, then making the last val a
        // null ast will indicate that the block should evaluate to null (and not the value
        // of the last expr).
        if local_state == semicolon_trailing_expr || local_state == start {
            vals.push(Box::new(cobalt_ast::ast::NullAST::new(SourceSpan::from((
                0, 1,
            )))));
        }

        (
            Box::new(BlockAST::new(
                SourceSpan::from((span_start, span_len)),
                vals,
            )),
            errors,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::SourceReader;

    #[test]
    fn test_simple_add() {
        let mut reader = SourceReader::new("a + b");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_paren_add() {
        let mut reader = SourceReader::new("a + (b + c)");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_mul_add() {
        let mut reader = SourceReader::new("a * b + c");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_mixed() {
        let mut reader = SourceReader::new("a * b + c / (d - (f + g * h))");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_block_expr() {
        let mut reader = SourceReader::new("{ a + b;; let x = 4; x }");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_block_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_prefix_expr() {
        let mut reader = SourceReader::new("!a");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_prefix_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());

        let mut reader = SourceReader::new("!&*a");
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse_primary_expr();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }
}
