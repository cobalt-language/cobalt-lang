use crate::loop_until;

use super::*;
use cobalt_ast::dottedname::*;

impl<'src> Parser<'src> {
    pub(crate) fn parse_cdns(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> Result<CompoundDottedNameSegment<'src>, CobaltError<'src>> {
        let res = match self.current_token {
            None => Err(CobaltError::ExpectedFound {
                ex: "identifier or glob",
                found: None,
                loc: self.cursor.src_len().into(),
            }),
            Some(Token {
                kind: TokenKind::Ident(id),
                span,
            }) => Ok(CompoundDottedNameSegment::Identifier(id.into(), span)),
            Some(Token {
                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Star),
                span,
            }) => Ok(CompoundDottedNameSegment::Glob(span)),
            Some(Token {
                kind: TokenKind::OpenDelimiter(Delimiter::Brace),
                ..
            }) => {
                let mut opts = vec![];
                self.next();
                loop {
                    match self.current_token {
                        None => {
                            errors.push(CobaltError::ExpectedFound {
                                ex: "'}'",
                                found: None,
                                loc: self.cursor.src_len().into(),
                            });
                            break;
                        }
                        Some(Token {
                            kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                            ..
                        }) => break,
                        _ => {
                            let opt = self.parse_cdn_list(true, errors);
                            opts.push(opt);
                            match self.current_token {
                                None => {
                                    errors.push(CobaltError::ExpectedFound {
                                        ex: "'}'",
                                        found: None,
                                        loc: self.cursor.src_len().into(),
                                    });
                                    break;
                                }
                                Some(Token {
                                    kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                                    ..
                                }) => break,
                                Some(Token {
                                    kind: TokenKind::Comma,
                                    ..
                                }) => self.next(),
                                Some(Token { kind, span }) => {
                                    errors.push(CobaltError::ExpectedFound {
                                        ex: "'}' or ','",
                                        found: Some(kind.as_str().into()),
                                        loc: span,
                                    });
                                    if kind == TokenKind::Semicolon {
                                        break;
                                    }
                                    loop_until!(
                                        self,
                                        TokenKind::Comma
                                            | TokenKind::CloseDelimiter(Delimiter::Brace)
                                    );
                                }
                            }
                        }
                    }
                }
                Ok(CompoundDottedNameSegment::Group(opts))
            }
            Some(Token { kind, span }) => Err(CobaltError::ExpectedFound {
                ex: "identifier or glob",
                found: Some(kind.as_str().into()),
                loc: span,
            }),
        };
        self.next();
        res
    }
    pub(crate) fn parse_cdn_list(
        &mut self,
        in_nested: bool,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> Vec<CompoundDottedNameSegment<'src>> {
        let mut out = vec![];
        loop {
            match self.parse_cdns(errors) {
                Ok(seg) => out.push(seg),
                Err(err) => {
                    errors.push(err);
                    loop {
                        let Some(current) = self.current_token else {
                            break;
                        };
                        if current.kind == TokenKind::Dot {
                            break;
                        }
                        if current.kind == TokenKind::Semicolon {
                            self.next();
                            return out;
                        }
                        if in_nested
                            && matches!(
                                current.kind,
                                TokenKind::Comma | TokenKind::CloseDelimiter(Delimiter::Brace)
                            )
                        {
                            return out;
                        }
                        self.next();
                    }
                }
            }
            match self.current_token {
                None => break,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => self.next(),
                Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::Comma | TokenKind::CloseDelimiter(Delimiter::Brace),
                    ..
                }) if in_nested => break,
                Some(Token { kind, span }) => loop {
                    if let Ok(seg) = self.parse_cdns(errors) {
                        errors.push(CobaltError::ExpectedFound {
                            ex: "'.'",
                            found: Some(kind.as_str().into()),
                            loc: span,
                        });
                        out.push(seg);
                    } else {
                        self.next();
                        break;
                    }
                },
            }
        }
        out
    }

    pub(crate) fn parse_cdn(
        &mut self,
        errors: &mut Vec<CobaltError<'src>>,
    ) -> CompoundDottedName<'src> {
        let global = if matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::Dot,
                ..
            })
        ) {
            self.next();
            true
        } else {
            false
        };
        let segs = self.parse_cdn_list(false, errors);
        CompoundDottedName::new(segs, global)
    }
}
