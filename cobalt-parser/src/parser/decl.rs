use std::borrow::Cow;

use cobalt_ast::{
    ast::{ErrorAST, FnDefAST, NullAST, ParamType, Parameter, TypeDefAST, VarDefAST},
    BoxedAST, DottedName,
};
use cobalt_errors::{CobaltError, SourceSpan};

use crate::lexer::tokens::{BinOpToken, Delimiter, Keyword, TokenKind};

use super::Parser;

impl<'src> Parser<'src> {
    /// Parses a declaration.
    ///
    /// Going into this function, the current token should be the first
    /// token of this grammar.
    ///
    /// ```text
    /// decl
    ///    := let_decl
    ///    := type_decl
    /// ```
    pub fn parse_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let first_token = self.current_token.unwrap();
        let mut errors = vec![];

        let ast = match first_token.kind {
            TokenKind::Keyword(Keyword::Let) => {
                let (let_decl, let_decl_errors) = self.parse_let_decl();
                errors.extend(let_decl_errors);
                let_decl
            }

            TokenKind::Keyword(Keyword::Type) => {
                let (type_decl, type_decl_errors) = self.parse_type_decl();
                errors.extend(type_decl_errors);
                type_decl
            }

            TokenKind::Keyword(Keyword::Fn) => {
                let (fn_decl, fn_decl_errors) = self.parse_fn_def(false);
                errors.extend(fn_decl_errors);
                fn_decl
            }

            _ => todo!(),
        };

        (ast, errors)
    }

    pub(crate) fn check_module_decl(&mut self) -> bool {
        if self.current_token.is_none() {
            return false;
        }

        self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Module)
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// module_decl := 'module' [ident | dotted_expr] ';'
    /// ```
    pub(crate) fn parse_module_decl(
        &mut self,
    ) -> (Option<DottedName<'src>>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::Keyword(Keyword::Module));

        let span = self.current_token.unwrap().span;

        let mut errors = vec![];

        // Consume 'module'.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return (None, errors);
        }

        let mut dotted_ids = vec![];

        // Parse module name.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return (None, errors);
        }

        if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
            let span = self.current_token.unwrap().span;
            let id = std::borrow::Cow::Borrowed(ident);

            dotted_ids.push((id, span));
        }

        self.next();

        loop {
            if self.current_token.is_none() {
                break;
            }

            if self.current_token.unwrap().kind != TokenKind::Dot {
                break;
            }

            self.next();

            if self.current_token.is_none() {
                break;
            }

            if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
                let span = self.current_token.unwrap().span;
                let id = std::borrow::Cow::Borrowed(ident);

                dotted_ids.push((id, span));
            } else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: Some(self.current_token.unwrap().kind.as_str().into()),
                    loc: self.current_token.unwrap().span,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return (Some(DottedName::new(dotted_ids, true)), errors);
            }

            self.next();
        }

        let module_name = Some(DottedName::new(dotted_ids, true));

        // Next must be semicolon.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: span,
            });
            return (module_name, errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Semicolon {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: Some(self.current_token.unwrap().kind.as_str().into()),
                loc: span,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (module_name, errors);
        }

        self.next();

        // ---

        (module_name, errors)
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
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::At);

        let span = self.current_token.unwrap().span;
        let mut errors = vec![];
        self.next();

        let secondary_ident: Option<Cow<'src, str>>;

        // Parse (first) identifier.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return ((Cow::Borrowed("@"), None, span), errors);
        }

        let primary_ident = match self.current_token.unwrap().kind {
            TokenKind::Ident(ident) => Cow::Borrowed(ident),
            TokenKind::Keyword(kw) => Cow::Owned(kw.to_string()),
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: Some(self.current_token.unwrap().kind.as_str().into()),
                    loc: span,
                });
                return ((Cow::Borrowed("@"), None, span), errors);
            }
        };

        self.next();

        // Optionally parse (second) identifier.
        // First eat the '('.

        if self.current_token.is_none() {
            return ((primary_ident, None, span), errors);
        }

        if let TokenKind::OpenDelimiter(delim) = self.current_token.unwrap().kind {
            if delim != Delimiter::Paren {
                return ((primary_ident, None, span), errors);
            }
        } else {
            return ((primary_ident, None, span), errors);
        }

        self.next();

        // Parse identifier.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return ((primary_ident, None, span), errors);
        }

        if let TokenKind::Ident(ident) = self.current_token.unwrap().kind {
            secondary_ident = Some(Cow::Borrowed(ident));
        } else {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: Some(self.current_token.unwrap().kind.as_str().into()),
                loc: span,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren)
                    || self.current_token.unwrap().kind == TokenKind::Semicolon
                {
                    self.next();
                    break;
                }

                self.next();
            }

            return ((primary_ident, None, span), errors);
        }

        self.next();

        // Parse ')'.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: None,
                loc: span,
            });
            return ((primary_ident, secondary_ident, span), errors);
        }

        if let TokenKind::CloseDelimiter(delim) = self.current_token.unwrap().kind {
            if delim != Delimiter::Paren {
                errors.push(CobaltError::ExpectedFound {
                    ex: "')'",
                    found: Some(self.current_token.unwrap().kind.as_str().into()),
                    loc: span,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind
                        == TokenKind::CloseDelimiter(Delimiter::Paren)
                        || self.current_token.unwrap().kind == TokenKind::Semicolon
                    {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return ((primary_ident, secondary_ident, span), errors);
            }
        } else {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: Some(self.current_token.unwrap().kind.as_str().into()),
                loc: span,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren)
                    || self.current_token.unwrap().kind == TokenKind::Semicolon
                {
                    self.next();
                    break;
                }

                self.next();
            }

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
    pub(crate) fn parse_let_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
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
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
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
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let name: Option<DottedName<'src>> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => Some(DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                is_global,
            )),

            _ => {
                let found = Some(self.current_token.unwrap().kind.as_str().into());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return (Box::new(ErrorAST::new(first_token_loc)), errors);
            }
        };
        self.next();

        // Get the (optional) type of the variable.

        let mut ty_expr = None;
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "':' or '='",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind == TokenKind::Colon {
            self.next();

            let (ty, ty_errors) = self.parse_primary_expr();

            errors.extend(ty_errors);
            ty_expr = Some(ty);
        }

        // If the next token is a semicolon, the value defaults to null.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '='",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind == TokenKind::Semicolon {
            if ty_expr.is_none() {
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "type",
                    found: Some(self.current_token.unwrap().kind.as_str().into()),
                    loc,
                });
                return (Box::new(ErrorAST::new(loc)), errors);
            }

            let semicolon_span = self.current_token.unwrap().span;
            self.next();

            let ast = Box::new(VarDefAST::new(
                first_token_loc,
                name.unwrap(),
                Box::new(NullAST::new(SourceSpan::from((
                    semicolon_span.offset() - 1,
                    0,
                )))),
                ty_expr,
                vec![],
                false,
                is_mutable,
            ));

            return (ast, errors);
        }

        // Next has to be an equals sign.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be an expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // Next has to be a semicolon.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Semicolon {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
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

    /// Checks if the current token starts a type declaration.
    ///
    /// In particular, we check for this pattern:
    /// ```text
    /// annotation* 'type' ident '='
    /// ```
    pub(crate) fn check_type_decl(&mut self) -> bool {
        assert!(self.current_token.is_some());
        let token_on_entry = self.current_token;

        let idx_on_entry = self.cursor.index;

        // ---

        loop {
            if self.current_token.is_none() {
                self.rewind_to_idx(idx_on_entry);
                return false;
            }

            if self.current_token.unwrap().kind != TokenKind::At {
                break;
            }

            let _ = self.parse_annotation();
        }

        // ---

        if self.current_token.unwrap().kind != TokenKind::Keyword(Keyword::Type) {
            self.rewind_to_idx(idx_on_entry);
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

        if self.current_token.unwrap().kind != TokenKind::BinOp(BinOpToken::Eq) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        // ---

        self.rewind_to_idx(idx_on_entry);
        assert!(self.current_token == token_on_entry);
        true
    }

    /// Parses a type declaration.
    ///
    /// ```text
    /// type_decl
    ///  := annotation* 'type' IDENT '=' expr ';'
    ///  := annotation* 'type' IDENT '=' expr '::' '{' fn_def* '}' ';'
    /// ```
    pub(crate) fn parse_type_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let first_token_loc = self.current_token.unwrap().span;

        // Annotations.

        let mut anns = vec![];
        loop {
            if self.current_token.unwrap().kind == TokenKind::At {
                let (ann, ann_errors) = self.parse_annotation();
                errors.extend(ann_errors);
                anns.push(ann);

                if self.current_token.is_none() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "type definition",
                        found: None,
                        loc: first_token_loc,
                    });
                    return (Box::new(ErrorAST::new(self.source.len().into())), errors);
                }

                continue;
            }

            break;
        }

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'type'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Keyword(Keyword::Type) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'type'",
                found,
                loc,
            });

            loop {
                if let Some(current) = self.current_token {
                    if current.kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                    continue;
                }

                break;
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be an identifier, the name of the type.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let name: DottedName<'src> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                false,
            ),

            _ => {
                let found = Some(self.current_token.unwrap().kind.as_str().into());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return (Box::new(ErrorAST::new(first_token_loc)), errors);
            }
        };

        // Next has to be an equals sign.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::BinOp(BinOpToken::Eq) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'='",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        // Next has to be an expression.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "expression",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // Next has to be a semicolon or a double colon.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        // If it's a semicolon, we're done.

        if self.current_token.unwrap().kind == TokenKind::Semicolon {
            self.next();

            let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, vec![]));

            return (ast, errors);
        }

        // Next has to be a double colon.

        if self.current_token.unwrap().kind != TokenKind::ColonColon {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or '::'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        self.next();

        // Next has to be a left brace.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Brace) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'{'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        // Next is 0 or more function definitions.

        self.next();

        let mut methods = vec![];

        loop {
            if !self.check_fn_def() {
                break;
            }

            let (func, func_errors) = self.parse_fn_def(true);
            errors.extend(func_errors);
            methods.push(func);
        }

        // Next has to be a right brace.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Brace) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'}'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        // Next has to be a semicolon.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Semicolon {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
        }

        // Done.

        self.next();

        let ast = Box::new(TypeDefAST::new(first_token_loc, name, expr, anns, methods));

        (ast, errors)
    }

    /// Checks if the current token starts a function definition.
    ///
    /// In particular, it checks for the following pattern:
    /// ```text
    /// annotation* 'fn'
    /// ```
    pub(crate) fn check_fn_def(&mut self) -> bool {
        assert!(self.current_token.is_some());

        let idx_on_entry = self.cursor.index;

        // ---

        loop {
            if self.current_token.is_none() {
                self.rewind_to_idx(idx_on_entry);
                return false;
            }

            if self.current_token.unwrap().kind != TokenKind::At {
                break;
            }

            let _ = self.parse_annotation();
        }

        // ---

        if self.current_token.unwrap().kind != TokenKind::Keyword(Keyword::Fn) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        self.rewind_to_idx(idx_on_entry);
        true
    }

    /// Parses a function definition.
    ///
    /// ```text
    /// fn_def
    ///   := annotation* 'fn' IDENT '(' [fn_param [',' fn_param]*] ')' [':' primary_expr]? ['=' expr] ';'
    /// ```
    pub(crate) fn parse_fn_def(
        &mut self,
        in_struct: bool,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        let first_token_loc = self.current_token.unwrap().span;
        let mut errors = vec![];

        // Annotations.

        let mut anns = vec![];
        loop {
            if self.current_token.unwrap().kind == TokenKind::At {
                let (ann, ann_errors) = self.parse_annotation();
                errors.extend(ann_errors);
                anns.push(ann);

                if self.current_token.is_none() {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "function definition",
                        found: None,
                        loc: first_token_loc,
                    });
                    return (Box::new(ErrorAST::new(self.source.len().into())), errors);
                }

                continue;
            }

            break;
        }

        // Next has to be 'fn'.

        if self.current_token.unwrap().kind != TokenKind::Keyword(Keyword::Fn) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "function definition",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

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
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let name: DottedName<'src> = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => DottedName::new(
                vec![(Cow::from(s), self.current_token.unwrap().span)],
                false,
            ),

            _ => {
                let found = Some(self.current_token.unwrap().kind.as_str().into());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    errors,
                );
            }
        };

        // Next has to be an open paren.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Paren) {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Semicolon {
                    self.next();
                    break;
                }

                self.next();
            }

            return (
                Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                errors,
            );
        }

        // Next is 0 or more function parameters. After the first, each one has to be preceded by a
        // comma. After this section, the current token should be the close paren.

        self.next();

        let mut params = vec![];
        loop {
            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "')'",
                    found: None,
                    loc: first_token_loc,
                });
                return (Box::new(ErrorAST::new(self.source.len().into())), errors);
            }

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                break;
            }

            if !params.is_empty() {
                if self.current_token.unwrap().kind == TokenKind::Comma {
                    self.next();
                } else {
                    let found = Some(self.current_token.unwrap().kind.as_str().into());
                    let loc = self.current_token.unwrap().span;
                    errors.push(CobaltError::ExpectedFound {
                        ex: "','",
                        found,
                        loc,
                    });

                    loop {
                        if self.current_token.is_none() {
                            errors.push(CobaltError::ExpectedFound {
                                ex: "')'",
                                found: None,
                                loc: self.source.len().into(),
                            });
                            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
                        }

                        if self.current_token.unwrap().kind
                            == TokenKind::CloseDelimiter(Delimiter::Paren)
                        {
                            break;
                        }

                        self.next();
                    }

                    break;
                }
            }

            let (param, param_errors) = self.parse_fn_param();
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
            let (ret_type, ret_errors) = self.parse_primary_expr();
            errors.extend(ret_errors);
            ret = ret_type;
        }

        // If next is a semicolon, we're done. Otherwise, next is an equals sign, and we have to
        // parse subsequent expression.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "';' or expression",
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let mut body: BoxedAST = Box::new(NullAST::new(self.current_token.unwrap().span));

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
                found: None,
                loc: first_token_loc,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::Semicolon {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found,
                loc,
            });

            loop {
                if let Some(current) = self.current_token {
                    if current.kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                    continue;
                }

                break;
            }

            return (Box::new(ErrorAST::new(first_token_loc)), errors);
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
            in_struct,
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

        let name: &'src str = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => s,

            _ => {
                let found = Some(self.current_token.unwrap().kind.as_str().into());
                let loc = self.current_token.unwrap().span;
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found,
                    loc,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Comma
                        || self.current_token.unwrap().kind == TokenKind::Semicolon
                    {
                        self.next();
                        break;
                    }

                    if self.current_token.unwrap().kind
                        == TokenKind::CloseDelimiter(Delimiter::Paren)
                    {
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
        };

        // Next has to be a colon.

        self.next();

        if self.current_token.is_none() {
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
        }

        if self.current_token.unwrap().kind != TokenKind::Colon {
            let found = Some(self.current_token.unwrap().kind.as_str().into());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "':'",
                found,
                loc,
            });

            loop {
                if self.current_token.is_none() {
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::Comma
                    || self.current_token.unwrap().kind == TokenKind::Semicolon
                {
                    self.next();
                    break;
                }

                if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
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
                    found: None,
                    loc: first_token_loc,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Comma
                        || self.current_token.unwrap().kind == TokenKind::Semicolon
                    {
                        break;
                    }

                    if self.current_token.unwrap().kind
                        == TokenKind::CloseDelimiter(Delimiter::Paren)
                    {
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
