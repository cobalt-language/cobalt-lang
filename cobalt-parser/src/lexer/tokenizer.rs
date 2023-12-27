use std::{iter::Peekable, ops::Range, rc::Rc, str::Chars};

use super::{
    tokens::{
        BinOpToken, Delimiter, Keyword, LiteralToken, Token, TokenKind, UnOpToken, UnOrBinOpToken,
    },
    SourceReader,
};

use cobalt_ast::intrinsics::{FUNCTION_INTRINSICS, VALUE_INTRINSICS};
use cobalt_errors::{CobaltError, SourceSpan};
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, PartialEq)]
pub enum TokenizeErrorKind {
    EmptyInput,
    BadFirstChar,
    UnexpectedCharacter(char),
}

#[derive(Debug)]
pub struct TokenizeError {
    pub kind: TokenizeErrorKind,
    pub span: SourceSpan,
}

pub fn is_ignored_char(c: char) -> bool {
    c.is_whitespace()
}

pub fn is_ident_start(c: char) -> bool {
    is_xid_start(c) || c == '_'
}

pub fn is_intrinsic_name(name: &str) -> bool {
    VALUE_INTRINSICS.pin().contains_key(name) || FUNCTION_INTRINSICS.pin().contains_key(name)
}

/// Consume ignored characters until we hit a non-ignored character. We
/// do not eat the first non-ignored character.
pub fn eat_ignored(input: &mut Peekable<Chars>) {
    while let Some(c) = input.peek() {
        if !is_ignored_char(*c) {
            break;
        }

        input.next();
    }
}

/// Consume characters until we hit an ignored character. We do not eat
/// the first ignored character.
pub fn eat_until_ignored(input: &mut Peekable<Chars>) {
    while let Some(c) = input.peek() {
        if is_ignored_char(*c) {
            break;
        }

        input.next();
    }
}

pub struct TokenStream<'src>(pub Rc<[Token<'src>]>, pub &'src str);
impl<'src> TokenStream<'src> {
    pub fn src_len(&self) -> usize {
        self.1.len()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn src_is_empty(&self) -> bool {
        self.1.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'src> SourceReader<'src> {
    pub fn tokenize(&mut self) -> (TokenStream<'src>, Vec<CobaltError<'src>>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(&c) = self.peek() {
            match c {
                ' ' | '\r' | '\n' | '\t' => {
                    self.next_char();
                }

                '#' => {
                    if let Err(err) = self.eat_comment() {
                        errors.push(err);
                    }
                }

                '$' => {
                    let start = self.index;
                    self.next_char();
                    let mut add_err = true;
                    loop {
                        match self.peek() {
                            None => {
                                if add_err {
                                    errors.push(CobaltError::ExpectedFound {
                                        ex: "symbol name",
                                        found: None,
                                        loc: self.index.into(),
                                    })
                                }
                            }
                            Some('"') => {
                                let mut span = self.eat_string(&mut errors);
                                span.start -= 1;
                                tokens.push(Token {
                                    kind: TokenKind::Literal(LiteralToken::Symbol(
                                        &self.source[span.clone()],
                                    )),
                                    span: span.into(),
                                });
                            }
                            Some(' ' | '\r' | '\n' | '\t') => {
                                self.next_char();
                                if add_err {
                                    add_err = false;
                                    errors.push(CobaltError::ExpectedFound {
                                        ex: "symbol name",
                                        found: Some("whitespace".into()),
                                        loc: self.index.into(),
                                    });
                                }
                                continue;
                            }
                            Some(&c) => {
                                if is_ident_start(c) {
                                    loop {
                                        self.next_char();
                                        let Some(&c) = self.peek() else { break };
                                        if !is_xid_continue(c) {
                                            break;
                                        }
                                    }
                                    tokens.push(Token {
                                        kind: TokenKind::Literal(LiteralToken::Symbol(
                                            &self.source[start..self.index],
                                        )),
                                        span: (start..self.index).into(),
                                    });
                                } else if add_err {
                                    errors.push(CobaltError::ExpectedFound {
                                        ex: "symbol name",
                                        found: Some(format!("{c:?}").into()),
                                        loc: self.index.into(),
                                    });
                                }
                            }
                        }
                        break;
                    }
                }

                // An identifier is an xid_start followed by zero or more xid_continue.
                // Match an identifier.
                // TODO: include check for underscore, but then there must be at least
                // one xid_continue after the underscore.
                c if is_ident_start(c) => {
                    let ident_parse_res = self.eat_ident();

                    if let Err(ident_parse_err) = ident_parse_res {
                        errors.push(ident_parse_err);
                        continue;
                    }

                    let ident_token = ident_parse_res.unwrap();

                    // Check if the identifier is reserved.
                    if let Some(keyword) = Keyword::from_token(&ident_token) {
                        tokens.push(Token {
                            kind: TokenKind::Keyword(keyword),
                            span: ident_token.span,
                        });
                    } else {
                        tokens.push(ident_token);
                    }
                }

                // Delimiters.
                '(' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Paren),
                        span: self.source_span_backward(1),
                    });
                }
                ')' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Paren),
                        span: self.source_span_backward(1),
                    });
                }
                '{' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Brace),
                        span: self.source_span_backward(1),
                    });
                }
                '}' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Brace),
                        span: self.source_span_backward(1),
                    });
                }
                '[' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::OpenDelimiter(Delimiter::Bracket),
                        span: self.source_span_backward(1),
                    });
                }
                ']' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::CloseDelimiter(Delimiter::Bracket),
                        span: self.source_span_backward(1),
                    });
                }

                // UNARY OPERATORS
                '?' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::UnOp(UnOpToken::Q),
                        span: self.source_span_backward(1),
                    });
                }

                // BINARY OPERATORS
                '=' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::EqEq),
                                span: self.source_span_backward(2),
                            });
                        }
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Eq),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '!' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Neq),
                                span: self.source_span_backward(2),
                            });
                        }
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOp(UnOpToken::Not),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '<' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Leq),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('<') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Shl),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Lt),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '>' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Geq),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('>') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Shr),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Gt),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '&' => {
                    self.next_char();
                    match self.peek() {
                        Some('?') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Andq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::And),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '|' => {
                    self.next_char();
                    match self.peek() {
                        Some('?') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Orq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Or),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '^' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Xor),
                        span: self.source_span_backward(1),
                    });
                }

                '+' => {
                    self.next_char();
                    match self.peek() {
                        Some('+') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::UnOp(UnOpToken::PlusPlus),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::PlusEq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Add),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '-' => {
                    self.next_char();
                    match self.peek() {
                        Some('-') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::UnOp(UnOpToken::MinusMinus),
                                span: self.source_span_backward(2),
                            });
                        }

                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::MinusEq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Sub),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '*' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::TimesEq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Star),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '/' => {
                    self.next_char();
                    match self.peek() {
                        Some('=') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::DivEq),
                                span: self.source_span_backward(2),
                            });
                        }

                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::UnOrBinOp(UnOrBinOpToken::Star),
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                '%' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::BinOp(BinOpToken::Mod),
                        span: self.source_span_backward(1),
                    });
                }
                // alt. base int. literals
                '0' => {
                    let mut num_len = 1;
                    self.next_char();
                    let (base, alt) = match self.peek() {
                        Some(&'x') => (16, true),
                        Some(&'o') => (8, true),
                        Some(&'b') => (2, true),
                        _ => (10, false)
                    };
                    if alt {
                        self.next_char();
                        num_len += 1;
                    }

                    // Now pointing to the first digit.

                    while let Some(c) = self.peek() {
                        if c.is_digit(base) {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit before the non-digit.

                    if !alt && self.peek() == Some(&'.') {
                        self.next_char();
                        num_len += 1;
                    } else {
                        // Not a float.
                        tokens.push(Token {
                            kind: TokenKind::Literal(LiteralToken::Int(
                                self.slice_backward(num_len),
                            )),
                            span: self.source_span_backward(num_len),
                        });
                        continue;
                    }

                    // Now pointing to the decimal point.

                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit of the float.

                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Float(self.slice_backward(num_len))),
                        span: self.source_span_backward(num_len),
                    });
                }

                // LITERALS.
                n if n.is_ascii_digit() => {
                    let mut num_len = 1;
                    self.next_char();

                    // Now pointing to the first digit.

                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit before the non-digit.

                    if self.peek() == Some(&'.') {
                        self.next_char();
                        num_len += 1;
                    } else {
                        // Not a float.
                        tokens.push(Token {
                            kind: TokenKind::Literal(LiteralToken::Int(
                                self.slice_backward(num_len),
                            )),
                            span: self.source_span_backward(num_len),
                        });
                        continue;
                    }

                    // Now pointing to the decimal point.

                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            self.next_char();
                            num_len += 1;
                        } else {
                            break;
                        }
                    }

                    // Now pointing to the last digit of the float.

                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Float(self.slice_backward(num_len))),
                        span: self.source_span_backward(num_len),
                    });
                }

                // MISC.
                ';' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Semicolon,
                        span: self.source_span_backward(1),
                    });
                }

                ':' => {
                    self.next_char();

                    match self.peek() {
                        Some(&'?') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::BinOp(BinOpToken::Colonq),
                                span: self.source_span_backward(2),
                            });
                        }
                        Some(&':') => {
                            self.next_char();
                            tokens.push(Token {
                                kind: TokenKind::ColonColon,
                                span: self.source_span_backward(2),
                            });
                        }
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::Colon,
                                span: self.source_span_backward(1),
                            });
                        }
                    }
                }

                ',' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Comma,
                        span: self.source_span_backward(1),
                    });
                }
                '.' => {
                    self.next_char();
                    tokens.push(Token {
                        kind: TokenKind::Dot,
                        span: self.source_span_backward(1),
                    });
                }
                '@' => {
                    let span_start = self.index;
                    self.next_char();

                    if let Some(&c) = self.peek() {
                        if !is_ident_start(c) {
                            errors.push(CobaltError::ExpectedFound {
                                ex: "start of identifier",
                                found: Some(c.to_string().into()),
                                loc: SourceSpan::from((self.index, 1)),
                            });
                            self.next_char();
                            continue;
                        }
                    } else {
                        errors.push(CobaltError::ExpectedFound {
                            ex: "name",
                            found: None,
                            loc: SourceSpan::from((self.index, 1)),
                        });
                        continue;
                    }

                    // --- Name.

                    let ident_parse_res = self.eat_ident();

                    if let Err(ident_parse_err) = ident_parse_res {
                        errors.push(ident_parse_err);
                        continue;
                    }

                    let ident_token = ident_parse_res.unwrap();
                    let name = &self.source[ident_token.span.offset()
                        ..ident_token.span.offset() + ident_token.span.len()];

                    // --- If intrinsic, end here.

                    if is_intrinsic_name(name) {
                        tokens.push(Token {
                            kind: TokenKind::Intrinsic(name),
                            span: SourceSpan::from((span_start, self.index)),
                        });
                        continue;
                    }

                    // --- Optional param.

                    match self.peek() {
                        Some(&'(') => {}
                        _ => {
                            tokens.push(Token {
                                kind: TokenKind::Annotation((name, None)),
                                span: SourceSpan::from((
                                    span_start,
                                    ident_token.span.offset() + ident_token.span.len(),
                                )),
                            });
                            continue;
                        }
                    }

                    // Eat the '('.
                    assert_eq!(self.peek(), Some(&'('));
                    self.next_char();

                    let arg_span_start = self.index;

                    let mut paren_depth = 1;
                    loop {
                        match self.peek().to_owned() {
                            Some('(') => paren_depth += 1,
                            Some(')') => {
                                if paren_depth == 1 {
                                    break;
                                }
                                paren_depth -= 1
                            }
                            _ => {}
                        }
                        self.next_char();
                    }

                    // Next will be the closing ')'.
                    let arg_span_end = self.index;
                    self.next_char();

                    let arg = &self.source[arg_span_start..arg_span_end];

                    tokens.push(Token {
                        kind: TokenKind::Annotation((name, Some(arg))),
                        span: SourceSpan::from((span_start, arg_span_end)),
                    });
                }

                // --- String literal.
                '"' => {
                    let Range { start, end } = self.eat_string(&mut errors);
                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Str(&self.source[start..end])),
                        span: SourceSpan::from((start, end - start)),
                    });
                }

                // --- Char literal
                '\'' => {
                    let span_start = self.index;
                    self.next_char();

                    let mut last_was_escape = false;
                    loop {
                        let c = self.peek();
                        if c.is_none() {
                            errors.push(CobaltError::ExpectedFound {
                                ex: "rest of char literal",
                                found: None,
                                loc: SourceSpan::from((self.source.len(), 0)),
                            });
                            continue;
                        }
                        let c = c.unwrap();

                        if c == &'\\' {
                            last_was_escape = !last_was_escape;
                            self.next_char();
                            continue;
                        }

                        if c == &'\'' && !last_was_escape {
                            break;
                        }

                        last_was_escape = false;

                        self.next_char();
                    }

                    self.next_char();
                    let span_end = self.index;

                    tokens.push(Token {
                        kind: TokenKind::Literal(LiteralToken::Char(
                            &self.source[span_start..span_end],
                        )),
                        span: SourceSpan::from((span_start, span_end - span_start)),
                    });
                }

                _ => {
                    let span_start = self.index;
                    self.next_char();
                    let span_end = self.index;
                    errors.push(CobaltError::UnexpectedChar {
                        loc: SourceSpan::from((span_start, span_end - span_start)),
                        ch: c,
                    });
                }
            }
        }

        (TokenStream(tokens.into(), self.source), errors)
    }

    /// Parse an identifier.
    ///
    /// When calling this function, the reader should be pointing to what is
    /// expected to be the charactera immediately preceding the identifier, so
    /// that calling `self.next()` will return the first character of the
    /// identifier.
    ///
    /// After calling this function:
    /// - Success: the reader will be pointing at the last character of the
    /// identifier.
    /// - Failure (wrong first character): the reader will be pointing at the
    /// character after the one it was pointing at when this function was
    /// called.
    fn eat_ident(&mut self) -> Result<Token<'src>, CobaltError<'src>> {
        let start_idx = self.index;

        if let Some(&c) = self.peek() {
            if !(is_xid_start(c) || c == '_') {
                self.next_char();
                let err = CobaltError::ExpectedFound {
                    ex: "xid_start or _",
                    found: Some(self.source[start_idx..(start_idx + c.len_utf8())].into()),
                    loc: SourceSpan::from((start_idx, self.index - start_idx)),
                };

                return Err(err);
            }
        } else {
            let err = CobaltError::UnexpectedEndOfInput {
                loc: SourceSpan::from((self.source.len(), 0)),
            };

            return Err(err);
        }

        self.next_char();

        // The reader is now pointing at the first character of the identifier.

        while let Some(c) = self.peek() {
            if !is_xid_continue(*c) {
                break;
            }

            self.next_char();
        }

        // The reader is now pointing at the last character of the identifier.

        let to_return = Token {
            kind: TokenKind::Ident(self.slice_from(start_idx)),
            span: self.source_span_from(start_idx),
        };

        Ok(to_return)
    }

    fn eat_comment(&mut self) -> Result<(), CobaltError<'src>> {
        assert_eq!(self.next_char(), Some('#'));

        let mut multiline_level = 0;
        while let Some('=') = self.next_char() {
            multiline_level += 1;
        }

        if multiline_level == 0 {
            // Single line comment.
            loop {
                match self.next_char() {
                    None => break,
                    Some('\n') => break,
                    _ => {}
                }
            }

            return Ok(());
        }

        // Multi-line comment.

        let mut num_levels_eaten = 0;
        loop {
            match self.next_char() {
                Some('=') => {
                    num_levels_eaten += 1;
                }
                Some('#') => {
                    if num_levels_eaten < multiline_level {
                        num_levels_eaten = 0;
                    } else {
                        return Ok(());
                    }
                }
                Some(_) => {
                    num_levels_eaten = 0;
                }
                None => {
                    return Err(CobaltError::UnexpectedEndOfInput {
                        loc: SourceSpan::from((self.source.len(), 0)),
                    });
                }
            }
        }
    }

    fn eat_string(&mut self, errors: &mut Vec<CobaltError<'src>>) -> Range<usize> {
        let span_start = self.index;
        self.next_char();

        let mut last_was_escape = false;
        loop {
            let c = self.peek();
            if c.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "rest of string literal",
                    found: None,
                    loc: SourceSpan::from((self.source.len(), 0)),
                });
                continue;
            }
            let c = c.unwrap();

            if c == &'\\' {
                last_was_escape = !last_was_escape;
                self.next_char();
                continue;
            }

            if c == &'"' && !last_was_escape {
                break;
            }

            last_was_escape = false;

            self.next_char();
        }

        self.next_char();

        span_start..self.index
    }
}
