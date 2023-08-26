use std::fmt::Display;

use cobalt_errors::SourceSpan;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Type,
    Fn,
}

impl Keyword {
    /// Returns `None` if the identifier is not a keyword. Otherwise, returns
    /// the keyword.
    pub(crate) fn from_str(ident: &str) -> Option<Keyword> {
        match ident {
            "let" => Some(Keyword::Let),
            "mut" => Some(Keyword::Mut),
            "const" => Some(Keyword::Const),
            "type" => Some(Keyword::Type),
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOpToken {
    Not, // !
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOrBinOpToken {
    Star, // *
    And,  // &
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LiteralToken<'src> {
    Int(&'src str),
    Float(&'src str),
    Str(&'src str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Literal(LiteralToken<'src>),
}

impl<'src> Display for TokenKind<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// `'src` is the lifetime of the source code, for example.
#[derive(Debug, Clone, Copy)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: SourceSpan,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
