use std::borrow::Cow;

use cobalt_ast::{
    ast::{FnDefAST, NullAST, ParamType, Parameter, TypeDefAST, VarDefAST},
    BoxedAST, DottedName,
};
use cobalt_errors::{CobaltError, ParserFound};

use crate::lexer::tokens::{BinOpToken, Delimiter, Keyword, TokenKind};

use super::Parser;

impl<'src> Parser<'src> {
    /// Parses a declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```
    /// decl
    ///    := let_decl
    ///    := type_decl
    /// ```
    fn parse_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let first_token = self.current_token.unwrap();
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

    /// Parses a type declaration.
    ///
    /// ```
    /// type_decl
    ///  := 'type' IDENT '=' expr ';'
    ///  := 'type' IDENT '=' expr '::' '{' fn_def* '}' ';'
    /// ```
    pub fn parse_type_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Type));

        let mut errors = vec![];
        let first_token_loc = self.current_token.unwrap().span;

        // Next has to be an identifier, the name of the type.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let name: DottedName<'src> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                false,
            ),

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

        // Next has to be an equals sign.

        self.next();

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

        // Next has to be an expression.

        self.next();

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

        // Next has to be a semicolon or a double colon.

        self.next();

        if self.current_token.is_none() {
            return (
                Box::new(NullAST::new(first_token_loc)),
                vec![CobaltError::ExpectedFound {
                    ex: "';' or '::'",
                    found: ParserFound::Eof,
                    loc: first_token_loc,
                }],
            );
        }

        // If it's a semicolon, we're done.

        if self.current_token.unwrap().kind == TokenKind::Semicolon {
            self.next();

            let ast = Box::new(TypeDefAST::new(
                first_token_loc,
                name,
                expr,
                vec![], // TODO: parse annotations
                vec![],
            ));

            return (ast, errors);
        }

        // Next has to be a double colon.

        if self.current_token.unwrap().kind != TokenKind::Colon {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found,
                loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Colon {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found,
                loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        // Next has to be a left brace.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Brace) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found,
                loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        // Next is 0 or more function definitions.

        self.next();

        let mut methods = vec![];
        while self.current_token.is_some()
            && self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Brace)
        {
            let (func, func_errors) = self.parse_fn_def(true);
            errors.extend(func_errors);
            methods.push(func);

            self.next();
        }

        // Next has to be a right brace.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Brace) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found,
                loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        // Next has to be a semicolon.

        self.next();

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

        // Done.

        let ast = Box::new(TypeDefAST::new(
            first_token_loc,
            name,
            expr,
            vec![], // TODO: parse annotations
            methods,
        ));

        (ast, errors)
    }

    /// Parses a function definition.
    ///
    /// ```
    /// fn_def
    ///   := 'fn' IDENT '(' [fn_param [',' fn_param]*] ')' [':' TYPE] ['=' expr] ';'
    pub fn parse_fn_def(&mut self, in_struct: bool) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert_eq!(
            self.current_token.unwrap().kind,
            TokenKind::Keyword(Keyword::Fn)
        );

        let mut errors = vec![];
        let first_token_loc = self.current_token.unwrap().span;

        // Next has to be an identifier.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let name: DottedName<'src> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                false,
            ),

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

        // Next has to be an open paren.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Paren) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found,
                loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        // Next is 0 or more function parameters. After the first, each one has to be preceded by a
        // comma. After this section, the current token should be the close paren.

        self.next();

        let mut params = vec![];
        loop {
            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "')'",
                    found: ParserFound::Eof,
                    loc: first_token_loc,
                });
                return (Box::new(NullAST::new(first_token_loc)), errors);
            }

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                break;
            }

            if !params.is_empty() {
                if self.current_token.unwrap().kind == TokenKind::Comma {
                    self.next();
                } else {
                    let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
                    let loc = self.current_token.unwrap().span;
                    errors.push(CobaltError::ExpectedFound {
                        ex: "','",
                        found,
                        loc,
                    });
                    return (Box::new(NullAST::new(first_token_loc)), errors);
                }
            }

            println!(
                "current token before parse_fn_param: {:?}",
                self.current_token
            );
            let (param, param_errors) = self.parse_fn_param();
            println!(
                "current token after parse_fn_param: {:?}",
                self.current_token
            );
            errors.extend(param_errors);
            params.push(param);
        }

        assert!(self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren));

        // Next is an optional return type.

        self.next();

        // TODO: no return type
        let mut ret: BoxedAST = Box::new(NullAST::new(first_token_loc));
        if self.current_token.is_some() && self.current_token.unwrap().kind == TokenKind::Colon {
            self.next();
            let (ret_type, ret_errors) = self.parse_expr();
            errors.extend(ret_errors);
            ret = ret_type;
        }

        // If next is a semicolon, we're done. Otherwise, next is an equals sign, and we have to
        // parse subsequent expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or expression",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (Box::new(NullAST::new(first_token_loc)), errors);
        }

        let mut body: BoxedAST = Box::new(NullAST::new(first_token_loc));

        if self.current_token.unwrap().kind == TokenKind::BinOp(BinOpToken::Eq) {
            self.next();
            let (expr, expr_errors) = self.parse_expr();
            errors.extend(expr_errors);
            body = expr;
        }

        // Next is a semicolon.

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

        // Done.

        let ast = Box::new(FnDefAST::new(
            first_token_loc,
            name,
            ret,
            params,
            body,
            vec![], // TODO: annotations
            in_struct,
        ));

        (ast, errors)
    }

    /// Parses a function parameter.
    ///
    /// ```
    /// fn_param
    ///  := ['mut' | 'const'] IDENT ':' ['&' | '*'] ['mut'] expr ['=' expr]
    /// ```
    pub fn parse_fn_param(&mut self) -> (Parameter<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];

        let first_token_loc = self.current_token.unwrap().span;

        // First is an optional mut or const.

        let mut param_type = ParamType::Normal;
        if self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Mut) {
            param_type = ParamType::Mutable;
            self.next();
        } else if self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Const) {
            param_type = ParamType::Constant;
            self.next();
        }

        // Next has to be an identifier.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(NullAST::new(first_token_loc)),
                    None,
                ),
                errors,
            );
        }

        let name: &'src str = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => s,

            _ => {
                let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });
                return (
                    (
                        first_token_loc,
                        Cow::from(""),
                        ParamType::Normal,
                        Box::new(NullAST::new(first_token_loc)),
                        None,
                    ),
                    errors,
                );
            }
        };

        // Next has to be a colon.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "':'",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(NullAST::new(first_token_loc)),
                    None,
                ),
                errors,
            );
        }

        if self.current_token.unwrap().kind != TokenKind::Colon {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "':'",
                found,
                loc,
            });
            return (
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(NullAST::new(first_token_loc)),
                    None,
                ),
                errors,
            );
        }

        // Next is an expression.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: ParserFound::Eof,
                loc: first_token_loc,
            });
            return (
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(NullAST::new(first_token_loc)),
                    None,
                ),
                errors,
            );
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // Next is an optional '=' and expression.

        let mut default = None;
        if self.current_token.is_some()
            && self.current_token.unwrap().kind == TokenKind::BinOp(BinOpToken::Eq)
        {
            self.next();

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "expression",
                    found: ParserFound::Eof,
                    loc: first_token_loc,
                });
                return (
                    (
                        first_token_loc,
                        Cow::from(""),
                        ParamType::Normal,
                        Box::new(NullAST::new(first_token_loc)),
                        None,
                    ),
                    errors,
                );
            }

            let (default_expr, default_expr_errors) = self.parse_expr();
            errors.extend(default_expr_errors);

            default = Some(default_expr);
        }

        // Done.

        (
            (first_token_loc, Cow::from(name), param_type, expr, default),
            errors,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::SourceReader;

    use super::*;

    #[test]
    fn test_parse_let_decl() {
        let src = "let x: i32 = 5i32;";
        let mut src_reader = SourceReader::new(src);
        let token_stream = src_reader.tokenize().0;
        let mut parser = Parser::new(&src_reader, token_stream);
        parser.next();
        let (ast, errors) = parser.parse_let_decl();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_fn_param() {
        let src = "x: i32";
        let mut src_reader = SourceReader::new(src);
        let token_stream1 = src_reader.tokenize().0;
        let mut parser1 = Parser::new(&src_reader, token_stream1);
        parser1.next();
        let (ast1, errors1) = parser1.parse_fn_param();
        dbg!(ast1);
        dbg!(&errors1);
        assert!(errors1.is_empty());

        let src = "mut x: i32";
        let mut src_reader = SourceReader::new(src);
        let token_stream2 = src_reader.tokenize().0;
        let mut parser2 = Parser::new(&src_reader, token_stream2);
        parser2.next();
        let (ast2, errors2) = parser2.parse_fn_param();
        dbg!(ast2);
        dbg!(&errors2);
        assert!(errors2.is_empty());

        let src = "const x: i32 = 5i32";
        let mut src_reader = SourceReader::new(src);
        let token_stream3 = src_reader.tokenize().0;
        let mut parser3 = Parser::new(&src_reader, token_stream3);
        parser3.next();
        let (ast3, errors3) = parser3.parse_fn_param();
        dbg!(ast3);
        dbg!(&errors3);
        assert!(errors3.is_empty());
    }

    #[test]
    fn test_parse_type_decl() {
        let src = "type Foo = i32;";
        let mut src_reader = SourceReader::new(src);
        let token_stream1 = src_reader.tokenize().0;
        let mut parser1 = Parser::new(&src_reader, token_stream1);
        parser1.next();
        let (ast1, errors1) = parser1.parse_type_decl();
        dbg!(ast1);
        dbg!(&errors1);
        assert!(errors1.is_empty());
    }

    #[test]
    fn test_fn_def() {
        let src = "fn foo(x: i32): i32 = 5i32;";
        let mut src_reader = SourceReader::new(src);
        let token_stream1 = src_reader.tokenize().0;
        let mut parser1 = Parser::new(&src_reader, token_stream1);
        parser1.next();
        let (ast1, errors1) = parser1.parse_fn_def(false);
        dbg!(ast1);
        dbg!(&errors1);
        assert!(errors1.is_empty());

        let src = "fn foo();";
        let mut src_reader = SourceReader::new(src);
        let token_stream2 = src_reader.tokenize().0;
        let mut parser2 = Parser::new(&src_reader, token_stream2);
        parser2.next();
        let (ast2, errors2) = parser2.parse_fn_def(false);
        dbg!(ast2);
        dbg!(&errors2);
        assert!(errors2.is_empty());
    }
}
