use std::{iter::Peekable, str::CharIndices};

use cobalt_errors::SourceSpan;

pub mod tokenizer;
pub mod tokens;

pub struct SourceReader<'src> {
    pub source: &'src str,
    iter: Peekable<CharIndices<'src>>,
    /// The index of the current character.
    index: usize,
}

impl<'src> SourceReader<'src> {
    pub fn new(source: &'src str) -> SourceReader<'src> {
        SourceReader {
            source,
            iter: source.char_indices().peekable(),
            index: 0,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        self.iter.next().map(|(i, c)| {
            self.index = i;
            c
        })
    }

    pub fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|&(_, c)| c)
    }

    /// Returns the slice `self.source[(index - offset)..index]`.
    ///
    /// ## Example
    /// ```
    /// let source = "hello world";
    /// let mut reader = SourceReader::new(source);
    /// reader.next();
    /// reader.next();
    /// reader.next();
    /// assert_eq!(reader.slice_backward(3), "hel");
    /// ```
    pub fn slice_backward(&self, offset: usize) -> &'src str {
        &self.source[(self.index - offset)..self.index]
    }

    pub fn source_span_backward(&self, offset: usize) -> SourceSpan {
        SourceSpan::from((self.index - offset, offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_backward() {
        let source = "hello world";
        let mut reader = SourceReader::new(source);
        reader.next();
        reader.next();
        reader.next();
        assert_eq!(reader.slice_backward(3), "hel");
    }
}
