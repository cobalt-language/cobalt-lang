//! The parser is responsible for turning the tokens into an AST.
//!
//! ## Conventions
//!
//! For functions starting with `parse_`:
//! - Upon entering a parsing function, `current_token` is assumed to be the first
//! token of the grammar.
//! - Upon exiting a parsing function, `current_token` is assumed to be the first
//! token after the grammar.
//!
//! For functions starting with `check_`:
//! - Upon entering a parsing function, `current_token` is assumed to be the first
//! token of the grammar.
//! - Upon exiting the function, `current_token` should be the same as it was on
//! entry.
//!
//! A common idiom is to use a `check_` function to see if a token starts a particular
//! grammar, and subsequently use a `parse_` function to parse the the grammar.

use cobalt_ast::ast::{ErrorAST, TopLevelAST};
use cobalt_errors::CobaltError;

use crate::lexer::tokenizer::TokenStream;
use crate::lexer::tokens::*;

mod annotations;
mod cdn;
mod decl;
mod expr;
mod top_level;

use crate::parser::top_level::CheckModuleDeclResult;
pub use decl::DeclLoc;

/// This is what the parser uses to iterate over the tokens. Since the `TokenStream`
/// is immutable, we need to keep track of the index of the next token to be returned.
pub struct TokenStreamCursor<'src> {
    stream: TokenStream<'src>,
    /// The index of the next token to be returned.
    pub index: usize,
}

impl<'src> TokenStreamCursor<'src> {
    pub fn new(stream: TokenStream) -> TokenStreamCursor {
        TokenStreamCursor { stream, index: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token<'src>> {
        let token = self.stream.0.get(self.index).cloned();
        self.index += 1;
        token
    }

    pub fn src_len(&self) -> usize {
        self.stream.1.len()
    }

    pub fn len(&self) -> usize {
        self.stream.0.len()
    }

    pub fn src_is_empty(&self) -> bool {
        self.stream.1.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.stream.0.is_empty()
    }
}

pub struct Parser<'src> {
    cursor: TokenStreamCursor<'src>,
    current_token: Option<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(stream: TokenStream<'src>) -> Parser<'src> {
        let cursor = TokenStreamCursor::new(stream);
        Parser {
            cursor,
            current_token: None,
        }
    }

    pub fn next(&mut self) {
        self.current_token = self.cursor.next_token();
    }

    pub fn rewind_to_idx(&mut self, idx: usize) {
        self.cursor.index = idx - 1;
        self.next();
    }

    /// Main entry point for parsing.
    pub fn parse(&mut self, errs: &mut Vec<CobaltError<'src>>) -> Option<TopLevelAST<'src>> {
        if self.current_token.is_none() {
            return None;
        }

        let mut vals = vec![];
        let mut module = None;
        let mut module_span = None;

        loop {
            if self.current_token.is_none() {
                break;
            }

            if self.check_module_decl() == CheckModuleDeclResult::File {
                let module_parsed = self.parse_file_module_decl(errs);

                if module.is_some() {
                    errs.push(CobaltError::RedefModule {
                        loc: self
                            .current_token
                            .map_or(self.cursor.src_len().into(), |tok| tok.span),
                        prev: module_span.unwrap(),
                    });
                    continue;
                }

                module = module_parsed;
                module_span = Some(
                    self.current_token
                        .map_or(self.cursor.src_len().into(), |tok| tok.span),
                );
                continue;
            }

            let i = self.cursor.index;
            let val = self.parse_top_level(errs);
            if i == self.cursor.index {
                decl::loop_until(self);
            }
            vals.push(val);
        }

        Some(TopLevelAST::new(vals, module))
    }
}

#[macro_export]
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
    ($this:expr, $pat:pat if $cond:expr) => {
        loop {
            let Some(current) = $this.current_token else {
                break;
            };

            if current.kind == TokenKind::Semicolon {
                $this.next();
                break;
            }

            if matches!(current.kind, $pat) && $cond {
                break;
            }

            $this.next();
        }
    };
}
