use super::*;
use crate::lexer::tokens::{BinOpToken, Delimiter, Keyword, Token, TokenKind};
use crate::loop_until;
use cobalt_ast::{ast::*, BoxedAST, DottedName};
use cobalt_errors::{CobaltError, SourceSpan};
use std::borrow::Cow;

#[inline(always)]
pub(crate) fn loop_until(this: &mut Parser) {
    loop {
        let Some(current) = this.current_token else {
            break;
        };
        match current.kind {
            TokenKind::Semicolon => {
                this.next();
                break;
            }
            TokenKind::CloseDelimiter(Delimiter::Brace) => break,
            _ => this.next(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DeclLoc {
    Local,
    Struct,
    Global,
}
impl<'src> Parser<'src> {
    pub(crate) fn parse_id(
        &mut self,
        is_global: bool,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> DottedName<'src> {
        let current = self.current_token.unwrap();
        if is_global {
            let is_global = if current.kind == TokenKind::Dot {
                self.next();
                true
            } else {
                false
            };
            let mut dotted_ids = vec![];

            // Parse name.
            let Some(current) = self.current_token else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                return DottedName::new(
                    vec![("<error>".into(), self.cursor.src_len().into())],
                    is_global,
                );
            };

            if let TokenKind::Ident(ident) = current.kind {
                let span = current.span;
                let id = std::borrow::Cow::Borrowed(ident);

                dotted_ids.push((id, span));
            }

            self.next();

            loop {
                if !matches!(
                    self.current_token,
                    Some(Token {
                        kind: TokenKind::Dot,
                        ..
                    })
                ) {
                    break;
                }

                self.next();

                let Some(current) = self.current_token else {
                    break;
                };

                if let TokenKind::Ident(ident) = current.kind {
                    let span = current.span;
                    let id = std::borrow::Cow::Borrowed(ident);

                    dotted_ids.push((id, span));
                } else {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "identifier",
                        found: Some(current.kind.as_str().into()),
                        loc: current.span,
                    });

                    loop_until!(self);

                    return DottedName::new(dotted_ids, is_global);
                }

                self.next();
            }
            DottedName::new(dotted_ids, is_global)
        } else {
            let Some(Token {
                kind: TokenKind::Ident(name),
                span,
            }) = self.current_token
            else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                return DottedName::new(
                    vec![("<error>".into(), self.cursor.src_len().into())],
                    is_global,
                );
            };
            self.next();
            DottedName::local((name.into(), span))
        }
    }

    /// Parses a declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```text
    /// decl
    ///    := let_decl
    ///    := type_decl
    ///    := fn_decl
    /// ```
    pub fn parse_decl(
        &mut self,
        loc: DeclLoc,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        assert!(self.current_token.is_some());

        let start_idx = self.cursor.index;
        loop {
            match self.current_token {
                None => {
                    self.rewind_to_idx(start_idx);
                    break;
                }
                Some(Token {
                    kind: TokenKind::Annotation(..),
                    ..
                }) => {
                    let _ = self.parse_annotation();
                }
                _ => break,
            }
        }
        let curr_idx = self.cursor.index;
        let tok = self.current_token;
        self.rewind_to_idx(start_idx);

        match tok {
            None => {
                errors.push(CobaltError::ExpectedFound {
                    ex: if loc == DeclLoc::Global {
                        "top-level declaration"
                    } else {
                        "declaration"
                    },
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                Box::new(ErrorAST::new(self.cursor.src_len().into())) as _
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Type),
                ..
            }) => self.parse_type_decl(loc == DeclLoc::Global, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                ..
            }) => self.parse_let_decl(loc, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Const),
                ..
            }) => self.parse_const_decl(loc == DeclLoc::Global, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                ..
            }) => self.parse_fn_def(loc, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Import),
                ..
            }) if loc != DeclLoc::Struct => self.parse_import(errors),

            Some(tok) => {
                self.rewind_to_idx(curr_idx);
                loop_until(self);
                errors.push(CobaltError::ExpectedFound {
                    ex: if loc == DeclLoc::Global {
                        "top-level declaration"
                    } else {
                        "declaration"
                    },
                    found: Some(tok.kind.as_str().into()),
                    loc: tok.span,
                });
                Box::new(ErrorAST::new(tok.span)) as _
            }
        }
    }

    /// Parses a let declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```text
    /// let_decl
    ///   := 'let' ['mut']? IDENT [':' primary_expr]? '=' expr ';'
    ///   := 'let' ['mut']? IDENT [':' primary_expr] ';'
    /// ```
    pub(crate) fn parse_let_decl(
        &mut self,
        loc: DeclLoc,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        // Annotations.

        let anns = self.parse_annotations();

        let current = self.current_token.unwrap();

        let first_token_loc = current.span;
        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                ..
            })
        ));

        self.next();

        // ---

        let mut is_mutable = false;
        if let Some(tok) = self.current_token {
            if tok.kind == TokenKind::Keyword(Keyword::Mut) {
                is_mutable = true;
                self.next();
            }
        } else {
            errors.push(CobaltError::ExpectedFound {
                ex: "variable name or modifier",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        }

        // Get the name of the variable.
        let name = self.parse_id(loc == DeclLoc::Global, errors);

        // Get the (optional) type of the variable.

        let mut ty_expr = None;
        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "':' or '='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        };

        if current.kind == TokenKind::Colon {
            self.next();

            let ty = self.parse_primary_expr(false, errors);

            ty_expr = Some(ty);
        }

        // If the next token is a semicolon, the value defaults to null.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        };

        if current.kind == TokenKind::Semicolon {
            if ty_expr.is_none() {
                let loc = current.span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "type",
                    found: Some(current.kind.as_str().into()),
                    loc,
                });
                return Box::new(ErrorAST::new(loc));
            }

            let semicolon_span = current.span;
            self.next();

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(NullAST::new(SourceSpan::from((
                    semicolon_span.offset() - 1,
                    0,
                )))),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        }

        // Next has to be an equals sign.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        };

        if current.kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = Some(current.kind.as_str().into());
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc: current.span,
            });

            loop_until(self);

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(current.span)),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        }

        self.next();

        // Next has to be an expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        }

        let expr = self.parse_expr(false, errors);

        // Next has to be a semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                expr,
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        };

        if current.kind != TokenKind::Semicolon {
            let found = Some(current.kind.as_str().into());
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc: current.span,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Brace)
            );

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name,
                expr,
                ty_expr,
                anns,
                loc != DeclLoc::Local,
                is_mutable,
            ));

            return ast;
        }

        self.next();

        // Create the AST node.

        let ast = Box::new(VarDefAST::new(
            first_token_loc,
            name,
            expr,
            ty_expr,
            anns,
            loc != DeclLoc::Local,
            is_mutable,
        ));

        ast
    }

    /// Parses a const declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```text
    /// const_decl
    ///   := 'const' IDENT [':' primary_expr]? '=' expr ';'
    ///   := 'const' IDENT [':' primary_expr] ';'
    /// ```
    pub(crate) fn parse_const_decl(
        &mut self,
        is_global: bool,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        // Annotations.

        let anns = self.parse_annotations();

        let current = self.current_token.unwrap();

        let first_token_loc = current.span;
        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Const),
                ..
            })
        ));

        self.next();

        // Get the name of the variable.

        let name = self.parse_id(is_global, errors);

        // Get the (optional) type of the variable.

        let mut ty_expr = None;
        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "':' or '='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
            ));

            return ast;
        };

        if current.kind == TokenKind::Colon {
            self.next();

            let ty = self.parse_primary_expr(false, errors);

            ty_expr = Some(ty);
        }

        // If the next token is a semicolon, the value defaults to null.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
            ));

            return ast;
        };

        if current.kind == TokenKind::Semicolon {
            if ty_expr.is_none() {
                let loc = current.span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "type",
                    found: Some(current.kind.as_str().into()),
                    loc,
                });
                return Box::new(ErrorAST::new(loc));
            }

            let semicolon_span = current.span;
            self.next();

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(NullAST::new(SourceSpan::from((
                    semicolon_span.offset() - 1,
                    0,
                )))),
                ty_expr,
                anns,
            ));

            return ast;
        }

        // Next has to be an equals sign.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
            ));

            return ast;
        };

        if current.kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc,
            });

            loop_until(self);

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(current.span)),
                ty_expr,
                anns,
            ));

            return ast;
        }

        self.next();

        // Next has to be an expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(ConstDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                ty_expr,
                anns,
            ));

            return ast;
        }

        let expr = self.parse_expr(false, errors);

        // Next has to be a semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            let ast = Box::new(ConstDefAST::new(first_token_loc, name, expr, ty_expr, anns));

            return ast;
        };

        if current.kind != TokenKind::Semicolon {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Brace)
            );

            let ast = Box::new(ConstDefAST::new(first_token_loc, name, expr, ty_expr, anns));

            return ast;
        }

        self.next();

        // Create the AST node.

        let ast = Box::new(ConstDefAST::new(first_token_loc, name, expr, ty_expr, anns));

        ast
    }

    /// Parses a type declaration.
    ///
    /// ```text
    /// type_decl
    ///  := annotation* 'type' IDENT '=' expr ';'
    ///  := annotation* 'type' IDENT '=' expr '::' '{' decl* '}' ';'
    /// ```
    pub(crate) fn parse_type_decl(
        &mut self,
        is_global: bool,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        assert!(self.current_token.is_some());

        let first_token_loc = self.current_token.unwrap().span;

        // Annotations.

        let anns = self.parse_annotations();

        // ---

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'type'",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        };

        if current.kind != TokenKind::Keyword(Keyword::Type) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'type'",
                found,
                loc,
            });

            loop_until(self);

            return Box::new(ErrorAST::new(first_token_loc));
        }

        self.next();

        // Next has to be an identifier, the name of the type.

        let name = self.parse_id(is_global, errors);

        // Next has to be an equals sign.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: None,
                loc: first_token_loc,
            });
            let ast = Box::new(TypeDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorTypeAST::new(self.cursor.src_len().into())),
                anns,
                vec![],
            ));

            return ast;
        };

        if current.kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc,
            });

            loop_until(self);

            let ast = Box::new(TypeDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorTypeAST::new(current.span)),
                anns,
                vec![],
            ));

            return ast;
        }

        // Next has to be an expression.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(TypeDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorTypeAST::new(self.cursor.src_len().into())),
                anns,
                vec![],
            ));

            return ast;
        }

        let expr = self.parse_expr(false, errors);

        // Next has to be a semicolon or a double colon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        };

        // If it's a semicolon, we're done.

        if current.kind == TokenKind::Semicolon {
            self.next();

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, vec![]));

            return ast;
        }

        // Next has to be a double colon.

        if current.kind != TokenKind::ColonColon {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found,
                loc,
            });

            loop_until!(
                self,
                TokenKind::OpenDelimiter(Delimiter::Brace)
                    | TokenKind::CloseDelimiter(Delimiter::Brace)
            );

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, vec![]));

            return ast;
        }

        self.next();

        // Next has to be a left brace.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        };

        if current.kind != TokenKind::OpenDelimiter(Delimiter::Brace) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found,
                loc,
            });

            loop_until!(self, TokenKind::CloseDelimiter(Delimiter::Brace));
            loop_until(self);

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, vec![]));

            return ast;
        }

        // Next is 0 or more function definitions.

        self.next();

        let mut methods = vec![];

        loop {
            if matches!(
                self.current_token,
                None | Some(Token {
                    kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                    ..
                })
            ) {
                break;
            }

            let decl = self.parse_decl(DeclLoc::Struct, errors);
            methods.push(decl);
        }

        // Next has to be a right brace.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found: None,
                loc: first_token_loc,
            });
            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

            return ast;
        };

        if current.kind != TokenKind::CloseDelimiter(Delimiter::Brace) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found,
                loc,
            });

            loop_until!(self, TokenKind::CloseDelimiter(Delimiter::Brace));
            loop_until(self);

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

            return ast;
        }

        // Next has to be a semicolon.

        self.next();

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        };

        if current.kind != TokenKind::Semicolon {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Brace)
            );

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

            return ast;
        }

        // Done.

        self.next();

        let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

        ast
    }
    /// Parses a function definition.
    ///
    /// ```text
    /// fn_def
    ///   := annotation* 'fn' IDENT '(' [fn_param [',' fn_param]*] ')' [':' primary_expr]? ['=' expr] ';'
    /// ```
    pub(crate) fn parse_fn_def(
        &mut self,
        loc: DeclLoc,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        assert!(self.current_token.is_some());
        let first_token_loc = self.current_token.unwrap().span;

        // Annotations.

        let anns = self.parse_annotations();

        // Next has to be 'fn'.
        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'fn'",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        };

        if current.kind != TokenKind::Keyword(Keyword::Fn) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "function definition",
                found,
                loc,
            });

            loop_until(self);

            return Box::new(ErrorAST::new(first_token_loc));
        }

        self.next();

        // Next has to be an identifier.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: first_token_loc,
            });
            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
        }

        let name = self.parse_id(loc == DeclLoc::Global, errors);

        // Next has to be an open paren.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(FnDefAST::new(
                first_token_loc,
                name,
                Box::new(ErrorTypeAST::new(self.cursor.src_len().into())),
                vec![],
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                anns,
                loc == DeclLoc::Struct,
            ));

            return ast;
        };

        if current.kind != TokenKind::OpenDelimiter(Delimiter::Paren) {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found,
                loc,
            });

            loop_until(self);

            return Box::new(ErrorAST::new(current.span));
        }

        // Next is 0 or more function parameters. After the first, each one has to be preceded by a
        // comma. After this section, the current token should be the close paren.

        self.next();

        let mut params = vec![];
        loop {
            let Some(current) = self.current_token else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "')'",
                    found: None,
                    loc: first_token_loc,
                });
                return Box::new(ErrorAST::new(self.cursor.src_len().into()));
            };

            if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                break;
            }

            if !params.is_empty() {
                if current.kind == TokenKind::Comma {
                    self.next();
                } else {
                    let found = Some(current.kind.as_str().into());
                    let loc = current.span;
                    errors.push(CobaltError::ExpectedFound {
                        ex: "','",
                        found,
                        loc,
                    });

                    loop {
                        let Some(current) = self.current_token else {
                            errors.push(CobaltError::ExpectedFound {
                                ex: "')'",
                                found: None,
                                loc: self.cursor.src_len().into(),
                            });
                            return Box::new(ErrorAST::new(self.cursor.src_len().into()));
                        };

                        if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                            break;
                        }

                        self.next();
                    }

                    break;
                }
            }

            let param = self.parse_fn_param(errors);
            params.push(param);
        }

        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::CloseDelimiter(Delimiter::Paren),
                ..
            })
        ));

        // Next is an optional return type.

        self.next();

        // TODO: no return type
        let mut ret: BoxedAST = Box::new(NullAST::new(first_token_loc));
        if matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Colon,
                ..
            })
        ) {
            self.next();
            ret = self.parse_primary_expr(false, errors);
        }

        // If next is a semicolon, we're done. Otherwise, next is an equals sign, and we have to
        // parse subsequent expression.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or expression",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(FnDefAST::new(
                first_token_loc,
                name,
                ret,
                params,
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                anns,
                loc == DeclLoc::Struct,
            ));

            return ast;
        };

        let mut body: BoxedAST = Box::new(NullAST::new(current.span));

        if current.kind == TokenKind::BinOp(BinOpToken::Eq) {
            self.next();
            body = self.parse_expr(false, errors);
        }

        // Next is a semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });

            let ast = Box::new(FnDefAST::new(
                first_token_loc,
                name,
                ret,
                params,
                body,
                anns,
                loc == DeclLoc::Struct,
            ));

            return ast;
        };

        if current.kind != TokenKind::Semicolon {
            let found = Some(current.kind.as_str().into());
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc: current.span,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Brace)
            );

            let ast = Box::new(FnDefAST::new(
                first_token_loc,
                name,
                ret,
                params,
                body,
                anns,
                loc == DeclLoc::Struct,
            ));

            return ast;
        }

        self.next();

        // Done.

        let ast = Box::new(FnDefAST::new(
            first_token_loc,
            name,
            ret,
            params,
            body,
            anns,
            loc == DeclLoc::Struct,
        ));

        ast
    }

    /// Parses a function parameter.
    ///
    /// ```text
    /// fn_param
    ///  := ['mut' | 'const'] IDENT ':' expr ['=' expr]
    /// ```
    pub(crate) fn parse_fn_param(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> Parameter<'src> {
        let Some(current) = self.current_token else {
            unreachable!()
        };

        let first_token_loc = current.span;

        // First is an optional mut or const.

        let mut param_type = ParamType::Normal;
        if current.kind == TokenKind::Keyword(Keyword::Mut) {
            param_type = ParamType::Mutable;
            self.next();
        } else if current.kind == TokenKind::Keyword(Keyword::Const) {
            param_type = ParamType::Constant;
            self.next();
        }

        // Next has to be an identifier.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: first_token_loc,
            });
            return (
                first_token_loc,
                Cow::from(""),
                ParamType::Normal,
                Box::new(ErrorAST::new(first_token_loc)),
                None,
            );
        };

        let name = if let TokenKind::Ident(s) = current.kind {
            s
        } else {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found,
                loc,
            });

            loop {
                let Some(current) = self.current_token else {
                    break;
                };

                if current.kind == TokenKind::Comma || current.kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                    break;
                }

                self.next();
            }

            return (
                first_token_loc,
                Cow::from(""),
                ParamType::Normal,
                Box::new(ErrorAST::new(first_token_loc)),
                None,
            );
        };

        // Next has to be a colon.

        self.next();

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "':'",
                found: None,
                loc: first_token_loc,
            });
            return (
                first_token_loc,
                Cow::from(""),
                ParamType::Normal,
                Box::new(ErrorAST::new(first_token_loc)),
                None,
            );
        };

        if current.kind != TokenKind::Colon {
            let found = Some(current.kind.as_str().into());
            let loc = current.span;
            errors.push(CobaltError::ExpectedFound {
                ex: "':'",
                found,
                loc,
            });

            loop {
                let Some(current) = self.current_token else {
                    break;
                };

                if current.kind == TokenKind::Comma || current.kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                    break;
                }

                self.next();
            }

            return (
                first_token_loc,
                Cow::from(""),
                ParamType::Normal,
                Box::new(ErrorAST::new(first_token_loc)),
                None,
            );
        }

        // Next is the type of the param.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });
            return (
                first_token_loc,
                Cow::from(""),
                ParamType::Normal,
                Box::new(ErrorAST::new(first_token_loc)),
                None,
            );
        }

        let expr = self.parse_expr(false, errors);

        // Next is an optional '=' and expression.

        let mut default = None;
        if self.current_token.is_some()
            && self.current_token.unwrap().kind == TokenKind::BinOp(BinOpToken::Eq)
        {
            self.next();

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "expression",
                    found: None,
                    loc: first_token_loc,
                });

                loop {
                    let Some(current) = self.current_token else {
                        break;
                    };

                    if current.kind == TokenKind::Comma || current.kind == TokenKind::Semicolon {
                        break;
                    }

                    if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                        break;
                    }

                    self.next();
                }

                return (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                );
            }

            let default_expr = self.parse_expr(false, errors);

            default = Some(default_expr);
        }

        // Done.

        (first_token_loc, Cow::from(name), param_type, expr, default)
    }

    pub(crate) fn parse_import(&mut self, errors: &mut Vec<CobaltError<'src>>) -> BoxedAST<'src> {
        let anns = self.parse_annotations();

        let Some(Token {
            kind: TokenKind::Keyword(Keyword::Import),
            span,
        }) = self.current_token
        else {
            unreachable!()
        };
        self.next();
        let path = self.parse_cdn(errors);

        if matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            })
        ) {
            self.next()
        } else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: self.current_token.map(|tok| tok.kind.as_str().into()),
                loc: self
                    .current_token
                    .map_or(self.cursor.src_len().into(), |tok| tok.span),
            });
            loop_until(self);
        }
        Box::new(ImportAST::new(span, path, anns))
    }
}
