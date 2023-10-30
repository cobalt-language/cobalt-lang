use super::*;
use crate::lexer::tokens::{BinOpToken, Delimiter, Keyword, Token, TokenKind};
use cobalt_ast::{ast::*, BoxedAST, DottedName};
use cobalt_errors::{CobaltError, SourceSpan};
use std::borrow::Cow;

macro_rules! loop_until {
    ($this:expr) => {
        loop {
            let Some(current) = $this.current_token else {
                break;
            };

            if current.kind == TokenKind::Semicolon {
                $this.next();
                break;
            }

            $this.next();
        }
    };
    ($this:expr, $pat:pat) => {
        loop {
            let Some(current) = $this.current_token else {
                break;
            };

            if current.kind == TokenKind::Semicolon {
                $this.next();
                break;
            }

            if matches!(current.kind, $pat) {
                break;
            }

            $this.next();
        }
    };
}
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
    pub fn parse_decl(&mut self, loc: DeclLoc) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let start_idx = self.cursor.index;
        loop {
            match self.current_token {
                None => {
                    self.rewind_to_idx(start_idx);
                    break;
                }
                Some(Token {
                    kind: TokenKind::At,
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
            None => (
                Box::new(ErrorAST::new(self.cursor.src_len().into())) as _,
                vec![CobaltError::ExpectedFound {
                    ex: if loc == DeclLoc::Global {
                        "top-level declaration"
                    } else {
                        "declaration"
                    },
                    found: None,
                    loc: self.cursor.src_len().into(),
                }],
            ),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Type),
                ..
            }) => self.parse_type_decl(loc == DeclLoc::Global),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                ..
            }) => self.parse_let_decl(loc),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Const),
                ..
            }) => self.parse_const_decl(loc == DeclLoc::Global),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                ..
            }) => self.parse_fn_def(loc),

            Some(tok) => {
                self.rewind_to_idx(curr_idx);
                loop_until(self);
                (
                    Box::new(ErrorAST::new(tok.span)) as _,
                    vec![CobaltError::ExpectedFound {
                        ex: if loc == DeclLoc::Global {
                            "top-level declaration"
                        } else {
                            "declaration"
                        },
                        found: Some(tok.kind.as_str().into()),
                        loc: tok.span,
                    }],
                )
            }
        }
    }

    /// Parses an annotation.
    ///
    /// Going into this function, the current token should be the '@'.
    ///
    /// ```text
    /// annotation := '@' ident ['(' ident ')']?
    /// ```
    pub(crate) fn parse_annotation(
        &mut self,
    ) -> (
        (Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan),
        Vec<CobaltError<'src>>,
    ) {
        let Some(Token {
            kind: TokenKind::At,
            span,
        }) = self.current_token
        else {
            unreachable!()
        };

        let mut errors = vec![];
        self.next();

        let secondary_ident: Option<Cow<'src, str>>;

        // Parse (first) identifier.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return ((Cow::Borrowed(""), None, span), errors);
        };

        let primary_ident = match current.kind {
            TokenKind::Ident(ident) => Cow::Borrowed(ident),
            TokenKind::Keyword(kw) => Cow::Owned(kw.to_string()),
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: Some(current.kind.as_str().into()),
                    loc: span,
                });
                return ((Cow::Borrowed(""), None, span), errors);
            }
        };

        self.next();

        // Optionally parse (second) identifier.
        // First eat the '('.

        let Some(current) = self.current_token else {
            return ((primary_ident, None, span), errors);
        };

        if let TokenKind::OpenDelimiter(delim) = current.kind {
            if delim != Delimiter::Paren {
                return ((primary_ident, None, span), errors);
            }
        } else {
            return ((primary_ident, None, span), errors);
        }

        self.next();

        // Parse identifier.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return ((primary_ident, None, span), errors);
        };

        if let TokenKind::Ident(ident) = current.kind {
            secondary_ident = Some(Cow::Borrowed(ident));
        } else {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: Some(current.kind.as_str().into()),
                loc: span,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Paren)
            );

            return ((primary_ident, None, span), errors);
        }

        self.next();

        // Parse ')'.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: None,
                loc: span,
            });
            return ((primary_ident, secondary_ident, span), errors);
        };

        if current.kind != TokenKind::CloseDelimiter(Delimiter::Paren) {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: Some(current.kind.as_str().into()),
                loc: span,
            });

            loop_until!(
                self,
                TokenKind::Semicolon | TokenKind::CloseDelimiter(Delimiter::Paren)
            );

            return ((primary_ident, secondary_ident, span), errors);
        }

        self.next();

        // ---

        ((primary_ident, secondary_ident, span), errors)
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
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let mut errors = vec![];

        // Annotations.

        let mut anns = vec![];
        while self.current_token.unwrap().kind == TokenKind::At {
            let (ann, mut ann_errors) = self.parse_annotation();
            errors.append(&mut ann_errors);
            anns.push(ann);

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "variable definition",
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                return (
                    Box::new(ErrorAST::new(self.cursor.src_len().into())),
                    errors,
                );
            }
        }

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
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
        }

        // Get the name of the variable.
        let name = self.parse_id(loc == DeclLoc::Global, &mut errors);

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

            return (ast, errors);
        };

        if current.kind == TokenKind::Colon {
            self.next();

            let (ty, mut ty_errors) = self.parse_primary_expr(false);

            errors.append(&mut ty_errors);
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

            return (ast, errors);
        };

        if current.kind == TokenKind::Semicolon {
            if ty_expr.is_none() {
                let loc = current.span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "type",
                    found: Some(current.kind.as_str().into()),
                    loc,
                });
                return (Box::new(ErrorAST::new(loc)), errors);
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

            return (ast, errors);
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

            return (ast, errors);
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

            return (ast, errors);
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

            return (ast, errors);
        }

        let (expr, mut expr_errors) = self.parse_expr(false);
        errors.append(&mut expr_errors);

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

            return (ast, errors);
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

            return (ast, errors);
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

        (ast, errors)
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
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let mut errors = vec![];

        // Annotations.

        let mut anns = vec![];
        while self.current_token.unwrap().kind == TokenKind::At {
            let (ann, mut ann_errors) = self.parse_annotation();
            errors.append(&mut ann_errors);
            anns.push(ann);

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "constant definition",
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                return (
                    Box::new(ErrorAST::new(self.cursor.src_len().into())),
                    errors,
                );
            }
        }

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

        let name = self.parse_id(is_global, &mut errors);

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

            return (ast, errors);
        };

        if current.kind == TokenKind::Colon {
            self.next();

            let (ty, mut ty_errors) = self.parse_primary_expr(false);

            errors.append(&mut ty_errors);
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

            return (ast, errors);
        };

        if current.kind == TokenKind::Semicolon {
            if ty_expr.is_none() {
                let loc = current.span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "type",
                    found: Some(current.kind.as_str().into()),
                    loc,
                });
                return (Box::new(ErrorAST::new(loc)), errors);
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

            return (ast, errors);
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

            return (ast, errors);
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

            return (ast, errors);
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

            return (ast, errors);
        }

        let (expr, mut expr_errors) = self.parse_expr(false);
        errors.append(&mut expr_errors);

        // Next has to be a semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            let ast = Box::new(ConstDefAST::new(first_token_loc, name, expr, ty_expr, anns));

            return (ast, errors);
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

            return (ast, errors);
        }

        self.next();

        // Create the AST node.

        let ast = Box::new(ConstDefAST::new(first_token_loc, name, expr, ty_expr, anns));

        (ast, errors)
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
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let first_token_loc = self.current_token.unwrap().span;

        // Annotations.

        let mut anns = vec![];
        while self.current_token.unwrap().kind == TokenKind::At {
            let (ann, mut ann_errors) = self.parse_annotation();
            errors.append(&mut ann_errors);
            anns.push(ann);

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "type definition",
                    found: None,
                    loc: first_token_loc,
                });
                return (
                    Box::new(ErrorAST::new(self.cursor.src_len().into())),
                    errors,
                );
            }
        }

        // ---

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'type'",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
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

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be an identifier, the name of the type.

        let name = self.parse_id(is_global, &mut errors);

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

            return (ast, errors);
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

            return (ast, errors);
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

            return (ast, errors);
        }

        let (expr, mut expr_errors) = self.parse_expr(false);
        errors.append(&mut expr_errors);

        // Next has to be a semicolon or a double colon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
        };

        // If it's a semicolon, we're done.

        if current.kind == TokenKind::Semicolon {
            self.next();

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, vec![]));

            return (ast, errors);
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

            return (ast, errors);
        }

        self.next();

        // Next has to be a left brace.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
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

            return (ast, errors);
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

            let (decl, mut decl_errors) = self.parse_decl(DeclLoc::Global);
            errors.append(&mut decl_errors);
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

            return (ast, errors);
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

            return (ast, errors);
        }

        // Next has to be a semicolon.

        self.next();

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
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

            return (ast, errors);
        }

        // Done.

        self.next();

        let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

        (ast, errors)
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
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        let first_token_loc = self.current_token.unwrap().span;
        let mut errors = vec![];

        // Annotations.

        let mut anns = vec![];
        while self.current_token.unwrap().kind == TokenKind::At {
            let (ann, mut ann_errors) = self.parse_annotation();
            errors.append(&mut ann_errors);
            anns.push(ann);

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "function definition",
                    found: None,
                    loc: first_token_loc,
                });
                return (
                    Box::new(ErrorAST::new(self.cursor.src_len().into())),
                    errors,
                );
            }
        }

        // Next has to be 'fn'.
        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "'fn'",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
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

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be an identifier.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: first_token_loc,
            });
            return (
                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                errors,
            );
        }

        let name = self.parse_id(loc == DeclLoc::Global, &mut errors);

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

            return (ast, errors);
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

            return (Box::new(ErrorAST::new(current.span)), errors);
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
                return (
                    Box::new(ErrorAST::new(self.cursor.src_len().into())),
                    errors,
                );
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
                            return (
                                Box::new(ErrorAST::new(self.cursor.src_len().into())),
                                errors,
                            );
                        };

                        if current.kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                            break;
                        }

                        self.next();
                    }

                    break;
                }
            }

            let (param, mut param_errors) = self.parse_fn_param();
            errors.append(&mut param_errors);
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
            let (ret_type, mut ret_errors) = self.parse_primary_expr(false);
            errors.append(&mut ret_errors);
            ret = ret_type;
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

            return (ast, errors);
        };

        let mut body: BoxedAST = Box::new(NullAST::new(current.span));

        if current.kind == TokenKind::BinOp(BinOpToken::Eq) {
            self.next();
            let (expr, mut expr_errors) = self.parse_expr(false);
            errors.append(&mut expr_errors);
            body = expr;
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

            return (ast, errors);
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

            return (ast, errors);
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

        (ast, errors)
    }

    /// Parses a function parameter.
    ///
    /// ```text
    /// fn_param
    ///  := ['mut' | 'const'] IDENT ':' expr ['=' expr]
    /// ```
    pub(crate) fn parse_fn_param(&mut self) -> (Parameter<'src>, Vec<CobaltError<'src>>) {
        let Some(current) = self.current_token else {
            unreachable!()
        };

        let mut errors = vec![];

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
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                ),
                errors,
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
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                ),
                errors,
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
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                ),
                errors,
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
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                ),
                errors,
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
                (
                    first_token_loc,
                    Cow::from(""),
                    ParamType::Normal,
                    Box::new(ErrorAST::new(first_token_loc)),
                    None,
                ),
                errors,
            );
        }

        let (expr, mut expr_errors) = self.parse_expr(false);
        errors.append(&mut expr_errors);

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
                    (
                        first_token_loc,
                        Cow::from(""),
                        ParamType::Normal,
                        Box::new(ErrorAST::new(first_token_loc)),
                        None,
                    ),
                    errors,
                );
            }

            let (default_expr, mut default_expr_errors) = self.parse_expr(false);
            errors.append(&mut default_expr_errors);

            default = Some(default_expr);
        }

        // Done.

        (
            (first_token_loc, Cow::from(name), param_type, expr, default),
            errors,
        )
    }
}
