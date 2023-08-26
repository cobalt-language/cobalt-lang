use std::borrow::Cow;

use cobalt_ast::{
    ast::{NullAST, VarDefAST},
    BoxedAST, DottedName,
};
use cobalt_errors::{CobaltError, ParserFound};

use crate::tokenizer::tokens::{BinOpToken, Keyword, TokenKind};

use super::Parser;

impl<'src> Parser<'src> {
    /// Parses a declaration.
    ///
    /// Going into this function, calling the current token should be the first
    /// token of this grammar.
    ///
    /// ```
    /// decl
    ///    := 'let' ['mut'] IDENT [':' IDENT] '=' expr ';'
    /// ```
    fn parse_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let first_token = self.current_token;
        if first_token.is_none() {
            return (Box::new(NullAST::new(first_token.unwrap().span)), vec![]);
        }
        let first_token = first_token.unwrap();

        let mut errors = vec![];

        let ast = match first_token.kind {
            TokenKind::Keyword(Keyword::Let) => {
                let (let_decl, let_decl_errors) = self.parse_let_decl();
                errors.extend(let_decl_errors);
                let_decl
            }

            _ => todo!(),
        };

        todo!()
    }

    /// Parses a let declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```
    /// let_decl
    ///   := 'let' ['mut'] IDENT [':' expr] '=' expr ';'
    /// ```
    fn parse_let_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Let));

        let first_token_loc = self.current_token.unwrap().span;
        self.next();

        let mut errors = vec![];

        let is_global = false;

        // Check if the variable is mutable.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let mut is_mutable = false;
        if self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Mut) {
            is_mutable = true;
            self.next();
        }

        // Get the name of the variable.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let name: Option<DottedName<'src>> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => Some(DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                is_global,
            )),

            _ => {
                let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });

                return (Box::new(NullAST::new(first_token_loc)), errors);
            }
        };
        self.next();

        // Get the (optional) type of the variable.

        let mut ty_expr = None;
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "':' or '='",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind == TokenKind::Colon {
            self.next();

            let (ty, ty_errors) = self.parse_expr();

            errors.extend(ty_errors);
            ty_expr = Some(ty);

            self.next();
        }

        // Next has to be an equals sign.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc,
            });

            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be an expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // Next has to be a semicolon.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Semicolon {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        self.next();

        // Create the AST node.

        let ast = Box::new(VarDefAST::new(
            first_token_loc,
            name.unwrap(),
            expr,
            ty_expr,
            vec![],
            false,
            is_mutable,
        ));

        (ast, errors)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::SourceReader;

    use super::*;

    #[test]
    fn test_parse_let_decl() {
        let src = "let x: i32 = 5i32;";
        let token_stream = SourceReader::new(src).tokenize().0;
        let mut parser = Parser::new(token_stream);
        parser.next();
        let (ast, errors) = parser.parse_let_decl();
        dbg!(ast);
        dbg!(errors);
    }
}
