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
            TokenKind::Ident(ident) => Keyword::from_str(ident.as_str()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, PartialEq)]
pub enum BinOpToken {
    Eq,   // =
    EqEq, // ==
    Neq,  // !=

    Lt,     // <
    Leq,    // <=
    Gt,     // >
    Geq,    // >=
    And,    // &
    AndAnd, // &&
    Andq,   // &?
    Or,     // |
    OrOr,   // ||
    Orq,    // |?
    Xor,    // ^

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Shl, // <<
    Shr, // >>
}

#[derive(Debug, PartialEq)]
pub enum UnOpToken {
    Not, // !
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Ident(String),
    OpenDelimiter(Delimiter),
    CloseDelimiter(Delimiter),
    Semicolon,
    Colon,
    Comma,
    UnOp(UnOpToken),
    BinOp(BinOpToken),
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
