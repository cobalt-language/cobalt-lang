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

use cobalt_ast::{
    ast::{NullAST, TopLevelAST},
    BoxedAST,
};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

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

    pub fn rewind_to_idx(&mut self, idx: usize) {
        self.cursor.index = idx - 1;
        self.next();
    }

    /// Main entry point for parsing.
    pub fn parse(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        if self.current_token.is_none() {
            return (Box::new(NullAST::new(SourceSpan::from((0, 1)))), vec![]);
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
                let (module_parsed, errs_parsed) = self.parse_module_decl();

                if module.is_some() {
                    errs.push(CobaltError::RedefModule {
                        loc: self.current_token.unwrap().span,
                        prev: module_span.unwrap(),
                    });
                    continue;
                }

                errs.extend(errs_parsed);
                module = module_parsed;
                module_span = Some(self.current_token.unwrap().span);
                continue;
            }

            let (val, err) = self.parse_top_level();
            vals.push(val);
            errs.extend(err);
        }

        (Box::new(TopLevelAST::new(vals, module)), errs)
    }

    /// Parses a top level item.
    ///
    /// ```
    /// top_level
    ///    := type_decl
    ///    := fn_def
    /// ```
    pub fn parse_top_level(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        if self.check_type_decl() {
            return self.parse_type_decl();
        }

        if self.check_fn_def() {
            return self.parse_fn_def(false);
        }

        let span = self.current_token.unwrap().span;
        let errors = vec![CobaltError::ExpectedFound {
            ex: "function or type declaration",
            found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
            loc: span,
        }];

        self.next();
        (Box::new(NullAST::new(span)), errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_1() {
        let src = r#"module alloc;
@transparent
type layout = {size: u32, offset: u16} :: {
  @const
  fn new(size: u32, offset: u16): self_t = {size: size, offset: offset} :? self_t;
};"#;
        let mut reader = SourceReader::new(src);
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_2() {
        let src = r#"@export
module alloc._utils;
@link(weak) @forward
fn memclear(ptr: *mut null, size: usize) = {
  let ptr = ptr: *mut u8;
  let mut i = 0;
  while (i < size) {
    ptr[i] = 0;
    ++i;
  }
};
@link(weak) @forward
fn memcpy(dst: *mut null, src: *null, size: usize) = {
  let dst = dst: *mut u8;
  let src = src: *u8;
  let mut i = 0;
  while (i < size) {
    dst[i] = src[i];
    ++i;
  }
};"#;
        let mut reader = SourceReader::new(src);
        let tokens = reader.tokenize().0;
        let mut parser = Parser::new(&reader, tokens);
        parser.next();
        let (ast, errors) = parser.parse();
        dbg!(ast);
        dbg!(&errors);
        assert!(errors.is_empty());
    }
}
