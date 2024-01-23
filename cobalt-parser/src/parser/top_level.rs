use super::*;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::loop_until;
use cobalt_ast::{ast::*, BoxedAST, DottedName};
use cobalt_errors::SourceSpan;
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CheckModuleDeclResult {
    None,
    Invalid,
    File,
    Inline,
    Alias,
}

impl<'src> Parser<'src> {
    /// Parses a top level item.
    ///
    /// ```text
    /// top_level
    ///    := type_decl
    ///    := let_decl
    ///    := const_decl
    ///    := fn_def
    ///    := inline_module_decl
    /// ```
    pub(crate) fn parse_top_level(
        &mut self,
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
                    kind: TokenKind::IntrinOrAnn(..),
                    ..
                }) => {
                    let _ = self.parse_annotation();
                }
                _ => break,
            }
        }
        let tok = self.current_token;
        self.rewind_to_idx(start_idx);

        match tok {
            None => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "top-level declaration",
                    found: None,
                    loc: self.cursor.src_len().into(),
                });
                Box::new(ErrorAST::new(self.cursor.src_len().into())) as _
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Type),
                ..
            }) => self.parse_type_decl(true, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                ..
            }) => self.parse_let_decl(DeclLoc::Global, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Const),
                ..
            }) => self.parse_const_decl(true, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                ..
            }) => self.parse_fn_def(DeclLoc::Global, errors),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Import),
                ..
            }) => self.parse_import(errors),

            Some(tok) => match self.check_module_decl() {
                CheckModuleDeclResult::None => {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "top-level declaration",
                        found: Some(tok.kind.as_str().into()),
                        loc: tok.span,
                    });
                    Box::new(ErrorAST::new(tok.span)) as _
                }
                CheckModuleDeclResult::Inline => self.parse_inline_module_decl(errors),
                CheckModuleDeclResult::Alias => self.parse_alias_module_decl(errors),
                CheckModuleDeclResult::File | CheckModuleDeclResult::Invalid => {
                    errors.push(CobaltError::InvalidThing {
                        ex: "module declaration",
                        loc: tok.span,
                    });
                    Box::new(ErrorAST::new(tok.span))
                }
            },
        }
    }

    pub(crate) fn check_module_decl(&mut self) -> CheckModuleDeclResult {
        let idx_on_entry = self.cursor.index;

        self.eat_annotations();

        // ---

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Module),
                ..
            })
        ) {
            self.rewind_to_idx(idx_on_entry);
            return CheckModuleDeclResult::None;
        }

        self.next();

        // ---

        let mut errors = vec![];
        _ = self.parse_id(true, &mut errors);

        // ---

        let res = match self.current_token {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => CheckModuleDeclResult::File,
            Some(Token {
                kind: TokenKind::ColonColon,
                ..
            }) => CheckModuleDeclResult::Inline,
            Some(Token {
                kind: TokenKind::BinOp(BinOpToken::Eq),
                ..
            }) => CheckModuleDeclResult::Alias,
            _ => CheckModuleDeclResult::Invalid,
        };
        self.rewind_to_idx(idx_on_entry);
        res
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// inline_module_decl := 'module' ID '::' '{' top_level* '}' ';'
    /// ```
    pub(crate) fn parse_inline_module_decl(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        let span = self.current_token.unwrap().span;

        // --- Annotations.

        let anns = self.parse_annotations();

        // ---

        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Module),
                ..
            })
        ));

        self.next();

        // --- Module name.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return Box::new(ErrorAST::new(span));
        }

        let module_name = self.parse_id(true, errors);

        let mut working_result = ModuleAST::new(span, module_name, vec![], anns);

        // --- '::'

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::ColonColon,
                ..
            })
        ) {
            errors.push(CobaltError::ExpectedFound {
                ex: "::",
                found: None,
                loc: span,
            });
            return Box::new(working_result);
        }

        self.next();

        // --- '{'

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::OpenDelimiter(Delimiter::Brace),
                ..
            })
        ) {
            errors.push(CobaltError::ExpectedFound {
                ex: "{",
                found: None,
                loc: span,
            });
            return Box::new(working_result);
        }

        self.next();

        // --- top_level_decl*

        let mut vals = vec![];
        loop {
            let Some(Token { kind, .. }) = self.current_token else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "}",
                    found: None,
                    loc: SourceSpan::from((self.cursor.src_len(), 0)),
                });
                working_result.vals = vals;
                return Box::new(working_result);
            };

            match kind {
                TokenKind::CloseDelimiter(Delimiter::Brace) => {
                    self.next();
                    break;
                }

                _ => {
                    let parsed_tl = self.parse_top_level(errors);
                    vals.push(parsed_tl);
                }
            }
        }

        working_result.vals = vals;

        // ---

        if !matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            })
        ) {
            if let Some(tok) = self.current_token {
                errors.push(CobaltError::ExpectedFound {
                    ex: ";",
                    found: Some(Cow::Borrowed(tok.kind.as_str())),
                    loc: tok.span,
                });
            } else {
                errors.push(CobaltError::ExpectedFound {
                    ex: ";",
                    found: None,
                    loc: SourceSpan::from((self.cursor.src_len(), 0)),
                });
            }

            return Box::new(working_result);
        }

        self.next();

        // ---

        Box::new(working_result)
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// file_module_decl := 'module' [ident | dotted_expr] ';'
    /// ```
    pub(crate) fn parse_file_module_decl(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> Option<DottedName<'src>> {
        let Some(Token {
            kind: TokenKind::Keyword(Keyword::Module),
            span,
        }) = self.current_token
        else {
            unreachable!()
        };

        // Consume 'module'.

        self.next();

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return None;
        }

        let module_name = Some(self.parse_id(true, errors));

        // Next must be semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: span,
            });
            return module_name;
        };

        if current.kind != TokenKind::Semicolon {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: Some(current.kind.as_str().into()),
                loc: span,
            });

            loop_until!(self, TokenKind::Semicolon);

            return module_name;
        }

        self.next();

        // ---

        module_name
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// alias_module_decl := 'module' [ident | dotted_expr] '=' cdn ';'
    /// ```
    pub(crate) fn parse_alias_module_decl(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> BoxedAST<'src> {
        let span = self.current_token.unwrap().span;

        // --- Annotations.

        let anns = self.parse_annotations();

        // ---

        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Module),
                ..
            })
        ));

        self.next();

        // --- Module name.

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: None,
                loc: span,
            });
            return Box::new(ErrorAST::new(span));
        }

        let module_name = self.parse_id(true, errors);

        // ---

        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::BinOp(BinOpToken::Eq),
                ..
            })
        ));

        self.next();

        let glob = self.parse_cdn(errors);

        Box::new(ModuleAST::new(
            span,
            module_name,
            vec![Box::new(ImportAST::new(span, glob, vec![]))],
            anns,
        ))
    }
}
