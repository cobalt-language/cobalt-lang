use std::fmt::Display;

use cobalt_errors::SourceSpan;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Type,
    Fn,
    If,
    Else,
    Module,
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
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "module" => Some(Keyword::Module),
            _ => None,
        }
    }

    /// Returns `None` if the token is not a keyword. Otherwise, returns
    /// the keyword.
    ///
    /// This is useful for checking if an identifier is a keyword.
    pub(crate) fn from_token(token: &Token) -> Option<Keyword> {
        match &token.kind {
            TokenKind::Keyword(keyword) => Some(*keyword),
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

impl BinOpToken {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinOpToken::Eq => "=",
            BinOpToken::EqEq => "==",
            BinOpToken::Neq => "!=",

            BinOpToken::Lt => "<",
            BinOpToken::Leq => "<=",
            BinOpToken::Gt => ">",
            BinOpToken::Geq => ">=",
            BinOpToken::Andq => "&&",
            BinOpToken::Or => "|",
            BinOpToken::Orq => "||",
            BinOpToken::Xor => "^",

            BinOpToken::Add => "+",
            BinOpToken::Sub => "-",
            BinOpToken::Div => "/",
            BinOpToken::Mod => "%",
            BinOpToken::Shl => "<<",
            BinOpToken::Shr => ">>",
        }
    }
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

impl UnOrBinOpToken {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnOrBinOpToken::Star => "*",
            UnOrBinOpToken::And => "&",
        }
    }
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
    At,
}

impl<'src> TokenKind<'src> {
    pub fn precedence_value(&self) -> u8 {
        match self {
            TokenKind::BinOp(bop) => match bop {
                BinOpToken::Lt => 10,
                BinOpToken::Leq => 10,
                BinOpToken::Gt => 10,
                BinOpToken::Geq => 10,

                BinOpToken::Sub => 20,
                BinOpToken::Add => 25,

                BinOpToken::Div => 30,
                BinOpToken::Mod => 30,

                BinOpToken::Eq => 100,
                BinOpToken::EqEq => 100,
                BinOpToken::Neq => 100,

                _ => 0,
            },

            TokenKind::UnOrBinOp(UnOrBinOpToken::Star) => 35,

            _ => 0,
        }
    }
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
