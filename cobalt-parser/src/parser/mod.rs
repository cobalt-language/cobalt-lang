//! The parser is responsible for turning the tokens into an AST.
//!
//! ## Conventions
//! - Upon entering a parsing function, `current_token` is assumed to be the first
//! token of the grammar.
//! - Upon exiting a parsing function, `current_token` is assumed to be the first
//! token after the grammar.

use crate::lexer::{tokenizer::TokenStream, tokens::Token, SourceReader};

mod decl;
mod expr;

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

    pub fn peek_n_tokens_ahead(&mut self, n: usize) -> Option<Token<'src>> {
        let token = self.stream.0.get(self.index + n).cloned();
        token
    }
}

pub struct Parser<'src> {
    #[allow(dead_code)]
    source_reader: &'src SourceReader<'src>,
    cursor: TokenStreamCursor<'src>,
    current_token: Option<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src SourceReader<'src>, stream: TokenStream<'src>) -> Parser<'src> {
        let cursor = TokenStreamCursor::new(stream);
        Parser {
            source_reader: source,
            cursor,
            current_token: None,
        }
    }

    pub fn next(&mut self) {
        self.current_token = self.cursor.next_token();
    }

    pub fn peek_n(&mut self, n: usize) -> Option<Token<'src>> {
        self.cursor.peek_n_tokens_ahead(n)
    }

    pub fn parse(&mut self) {
        unimplemented!()
    }
}
