use cobalt_errors::SourceSpan;

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Fn,
}

impl Keyword {
    /// Returns `None` if the identifier is not a keyword. Otherwise, returns
    /// the keyword.
    pub(crate) fn from_str(ident: &str) -> Option<Keyword> {
        match ident {
            "fn" => Some(Keyword::Fn),
            _ => None,
        }
    }

    /// Returns `None` if the token is not a keyword. Otherwise, returns
    /// the keyword.
    ///
    /// This is useful for checking if an identifier is a keyword.
    pub(crate) fn from_token(token: &Token) -> Option<Keyword> {
        match &token.kind {
            TokenKind::Keyword(keyword) => Some(keyword.clone()),
            TokenKind::Ident(ident) => Keyword::from_str(ident),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpToken {
    Eq,   // =
    EqEq, // ==
    Neq,  // !=

    Lt,   // <
    Leq,  // <=
    Gt,   // >
    Geq,  // >=
    Andq, // &?
    Or,   // |
    Orq,  // |?
    Xor,  // ^

    Add, // +
    Sub, // -
    Div, // /
    Mod, // %
    Shl, // <<
    Shr, // >>
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOpToken {
    Not, // !
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOrBinOpToken {
    Star, // *
    And,  // &
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind<'src> {
    Keyword(Keyword),
    Ident(&'src str),
    OpenDelimiter(Delimiter),
    CloseDelimiter(Delimiter),
    Semicolon,
    Colon,
    Comma,
    UnOp(UnOpToken),
    BinOp(BinOpToken),
    UnOrBinOp(UnOrBinOpToken),
}

/// `'src` is the lifetime of the source code, for example.
#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: SourceSpan,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
