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

mod decl;
mod expr;

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
}

pub struct Parser<'src> {
    #[allow(dead_code)]
    source: &'src str,
    cursor: TokenStreamCursor<'src>,
    current_token: Option<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str, stream: TokenStream<'src>) -> Parser<'src> {
        let cursor = TokenStreamCursor::new(stream);
        Parser {
            source,
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
    pub fn parse(&mut self) -> (Option<TopLevelAST<'src>>, Vec<CobaltError<'src>>) {
        if self.current_token.is_none() {
            return (None, vec![]);
        }

        let mut vals = vec![];
        let mut errs = vec![];
        let mut module = None;
        let mut module_span = None;

        loop {
            if self.current_token.is_none() {
                break;
            }

            if self.check_module_decl() {
                let (module_parsed, errs_parsed) = self.parse_file_module_decl();

                if module.is_some() {
                    errs.push(CobaltError::RedefModule {
                        loc: self
                            .current_token
                            .map_or(self.source.len().into(), |tok| tok.span),
                        prev: module_span.unwrap(),
                    });
                    continue;
                }

                errs.extend(errs_parsed);
                module = module_parsed;
                module_span = Some(
                    self.current_token
                        .map_or(self.source.len().into(), |tok| tok.span),
                );
                continue;
            }

            let (val, err) = self.parse_decl(DeclLoc::Global);
            vals.push(val);
            errs.extend(err);
        }

        (Some(TopLevelAST::new(vals, module)), errs)
    }
}
