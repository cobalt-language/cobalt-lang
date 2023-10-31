use super::*;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::loop_until;
use cobalt_ast::{ast::*, BoxedAST, DottedName};
use cobalt_errors::SourceSpan;
use std::borrow::Cow;

#[derive(PartialEq)]
pub enum CheckModuleDeclResult {
    None,

    File,
    Inline,
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
    pub(crate) fn parse_top_level(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
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
        let tok = self.current_token;
        self.rewind_to_idx(start_idx);

        match tok {
            None => (
                Box::new(ErrorAST::new(self.cursor.src_len().into())) as _,
                vec![CobaltError::ExpectedFound {
                    ex: "top-level declaration",
                    found: None,
                    loc: self.cursor.src_len().into(),
                }],
            ),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Type),
                ..
            }) => self.parse_type_decl(true),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                ..
            }) => self.parse_let_decl(DeclLoc::Global),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Const),
                ..
            }) => self.parse_const_decl(true),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                ..
            }) => self.parse_fn_def(DeclLoc::Global),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Import),
                ..
            }) => self.parse_import(),

            Some(tok) => match self.check_module_decl() {
                CheckModuleDeclResult::None => (
                    Box::new(ErrorAST::new(tok.span)) as _,
                    vec![CobaltError::ExpectedFound {
                        ex: "top-level declaration",
                        found: Some(tok.kind.as_str().into()),
                        loc: tok.span,
                    }],
                ),
                CheckModuleDeclResult::Inline => self.parse_inline_module_decl(),
                CheckModuleDeclResult::File => (
                    Box::new(ErrorAST::new(tok.span)),
                    vec![CobaltError::InvalidThing {
                        ex: "file module declaration",
                        loc: tok.span,
                    }],
                ),
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

        if matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            })
        ) {
            self.rewind_to_idx(idx_on_entry);
            return CheckModuleDeclResult::File;
        }

        self.rewind_to_idx(idx_on_entry);
        CheckModuleDeclResult::Inline
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// inline_module_decl := 'module' ID '::' '{' top_level* '}' ';'
    /// ```
    pub(crate) fn parse_inline_module_decl(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        // --- Annotations.

        let (anns, mut anns_errs) = self.parse_annotations();
        errors.append(&mut anns_errs);

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
            return (Box::new(ErrorAST::new(span)), errors);
        }

        let module_name = self.parse_id(true, &mut errors);

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
            return (Box::new(working_result), errors);
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
            return (Box::new(working_result), errors);
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
                return (Box::new(working_result), errors);
            };

            match kind {
                TokenKind::CloseDelimiter(Delimiter::Brace) => {
                    self.next();
                    break;
                }

                _ => {
                    let (parsed_tl, mut parsed_errors) = self.parse_top_level();
                    vals.push(parsed_tl);
                    errors.append(&mut parsed_errors);
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

            return (Box::new(working_result), errors);
        }

        self.next();

        // ---

        (Box::new(working_result), errors)
    }

    /// Going into the function, the current token is assumed to be `module`.
    ///
    /// ```text
    /// file_module_decl := 'module' [ident | dotted_expr] ';'
    /// ```
    pub(crate) fn parse_file_module_decl(
        &mut self,
    ) -> (Option<DottedName<'src>>, Vec<CobaltError<'src>>) {
        let Some(Token {
            kind: TokenKind::Keyword(Keyword::Module),
            span,
        }) = self.current_token
        else {
            unreachable!()
        };

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

        let module_name = Some(self.parse_id(true, &mut errors));

        // Next must be semicolon.

        let Some(current) = self.current_token else {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: None,
                loc: span,
            });
            return (module_name, errors);
        };

        if current.kind != TokenKind::Semicolon {
            errors.push(CobaltError::ExpectedFound {
                ex: "';'",
                found: Some(current.kind.as_str().into()),
                loc: span,
            });

            loop_until!(self, TokenKind::Semicolon);

            return (module_name, errors);
        }

        self.next();

        // ---

        (module_name, errors)
    }
}
