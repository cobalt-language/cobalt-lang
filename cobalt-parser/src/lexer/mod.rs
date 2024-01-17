use cobalt_errors::SourceSpan;

pub mod tokenizer;
pub mod tokens;

pub struct SourceReader<'src> {
    pub source: &'src str,
    /// The index of the next character to be returned.
    pub index: usize,
}

impl<'src> SourceReader<'src> {
    pub fn new(source: &'src str) -> SourceReader<'src> {
        SourceReader { source, index: 0 }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let to_ret = self.source[self.index..].chars().next();

        self.index += to_ret.map_or(0, char::len_utf8);

        to_ret
    }

    pub fn peek(&mut self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    /// Returns the slice `self.source[(index - offset)..index]`.
    ///
    /// ## Example
    /// ```
    /// use cobalt_parser::lexer;
    /// use lexer::SourceReader;
    ///
    /// let source = "hello world";
    /// let mut reader = SourceReader::new(source);
    /// reader.next_char();
    /// reader.next_char();
    /// reader.next_char();
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
