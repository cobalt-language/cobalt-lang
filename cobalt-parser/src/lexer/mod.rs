use std::str::CharIndices;

use cobalt_errors::SourceSpan;

pub mod tokenizer;
pub mod tokens;

pub struct SourceReader<'src> {
    pub source: &'src str,
    /// Calling `next()` will give us the index of the character returned, but
    /// since we actually want the index of the *next* character, this should
    /// be one chracter ahead. This also means we don't need the iterator itself
    /// to be peekable.
    iter: CharIndices<'src>,
    next_char: Option<char>,
    /// The index of the next character to be returned.
    pub index: usize,
}

impl<'src> SourceReader<'src> {
    pub fn new(source: &'src str) -> SourceReader<'src> {
        let mut iter = source.char_indices();
        let next = iter.next();
        let next_char = next.map(|next| next.1);

        SourceReader {
            source,
            iter,
            next_char,
            index: 0,
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let to_return = self.next_char;

        let iter_next = self.iter.next();
        if let Some(next) = iter_next {
            self.index = next.0;
            self.next_char = Some(next.1);
        } else {
            self.index = self.source.len();
            self.next_char = None
        }

        to_return
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.next_char.as_ref()
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

    pub fn slice_from(&self, offset: usize) -> &'src str {
        &self.source[offset..self.index]
    }

    pub fn source_span_backward(&self, offset: usize) -> SourceSpan {
        SourceSpan::from((self.index - offset, offset))
    }

    pub fn source_span_from(&self, offset: usize) -> SourceSpan {
        SourceSpan::from((offset, self.index - offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_backward() {
        let source = "hello world";
        let mut reader = SourceReader::new(source);
        reader.next_char();
        reader.next_char();
        reader.next_char();
        assert_eq!(reader.slice_backward(3), "hel");
    }
}
