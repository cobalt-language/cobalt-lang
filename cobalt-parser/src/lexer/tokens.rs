use cobalt_errors::SourceSpan;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Type,
    Fn,
    If,
    Else,
    While,
    Module,
    Import,
    Trait,
    Break,
    Continue,
    Yield,
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
            "while" => Some(Keyword::While),
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "trait" => Some(Keyword::Trait),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "yield" => Some(Keyword::Yield),
            _ => None,
        }
    }

    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            Keyword::Let => "let",
            Keyword::Mut => "mut",
            Keyword::Const => "const",
            Keyword::Type => "type",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Module => "module",
            Keyword::Import => "import",
            Keyword::Trait => "trait",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Yield => "yield",
        }
    }

    /// Returns `None` if the token is not a keyword. Otherwise, returns
    /// the keyword.
    ///
    /// This is useful for checking if an identifier is a keyword.
    pub(crate) fn from_token(token: &Token) -> Option<Keyword> {
        match token.kind {
            TokenKind::Keyword(keyword) => Some(keyword),
            TokenKind::Ident(ident) => Keyword::from_str(ident),
            _ => None,
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Delimiter {
    Paren,
    Brace,
    Bracket,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOpToken {
    Eq,      // =
    PlusEq,  // +=
    MinusEq, // -=
    TimesEq, // *=
    DivEq,   // /=
    ModEq,   // %=
    AndEq,   // &=
    OrEq,    // |=
    XorEq,   // ^=
    ShlEq,   // <<=
    ShrEq,   // >>=
    EqEq,    // ==
    Neq,     // !=

    Lt,     // <
    Leq,    // <=
    Gt,     // >
    Geq,    // >=
    Andq,   // &?
    Orq,    // |?
    Colonq, // :?

    Div, // /
    Mod, // %
    Or,  // |
    Xor, // ^
    Shl, // <<
    Shr, // >>
}

impl BinOpToken {
    pub fn as_str(self) -> &'static str {
        match self {
            BinOpToken::Eq => "=",
            BinOpToken::PlusEq => "+=",
            BinOpToken::MinusEq => "-=",
            BinOpToken::TimesEq => "*=",
            BinOpToken::DivEq => "/=",
            BinOpToken::ModEq => "%=",
            BinOpToken::AndEq => "&=",
            BinOpToken::OrEq => "|=",
            BinOpToken::XorEq => "^=",
            BinOpToken::ShlEq => "<<=",
            BinOpToken::ShrEq => ">>=",
            BinOpToken::EqEq => "==",
            BinOpToken::Neq => "!=",

            BinOpToken::Lt => "<",
            BinOpToken::Leq => "<=",
            BinOpToken::Gt => ">",
            BinOpToken::Geq => ">=",
            BinOpToken::Andq => "&?",
            BinOpToken::Orq => "|?",
            BinOpToken::Colonq => ":?",

            BinOpToken::Div => "/",
            BinOpToken::Mod => "%",
            BinOpToken::Or => "|",
            BinOpToken::Xor => "^",
            BinOpToken::Shl => "<<",
            BinOpToken::Shr => ">>",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOpToken {
    Not,        // !
    Q,          // ?
    PlusPlus,   // ++
    MinusMinus, // --
}
impl UnOpToken {
    pub fn as_str(self) -> &'static str {
        match self {
            UnOpToken::Not => "!",
            UnOpToken::Q => "?",
            UnOpToken::PlusPlus => "++",
            UnOpToken::MinusMinus => "--",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOrBinOpToken {
    Star, // *
    And,  // &
    Add,  // +
    Sub,  // -
}

impl UnOrBinOpToken {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnOrBinOpToken::Star => "*",
            UnOrBinOpToken::And => "&",
            UnOrBinOpToken::Add => "+",
            UnOrBinOpToken::Sub => "-",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LiteralToken<'src> {
    Int(&'src str),
    Float(&'src str),
    /// The slice includes the double quotations.
    Str(&'src str),
    /// The slice includes the single quotes.
    Char(&'src str),
}
impl<'src> LiteralToken<'src> {
    pub fn as_str(self) -> &'src str {
        match self {
            LiteralToken::Int(s)
            | LiteralToken::Float(s)
            | LiteralToken::Str(s)
            | LiteralToken::Char(s) => s,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind<'src> {
    Keyword(Keyword),
    Ident(&'src str),
    OpenDelimiter(Delimiter),
    CloseDelimiter(Delimiter),
    Semicolon,
    Colon,
    ColonColon,
    Comma,
    UnOp(UnOpToken),
    BinOp(BinOpToken),
    UnOrBinOp(UnOrBinOpToken),
    Literal(LiteralToken<'src>),
    At,
    Dot,
}

impl<'src> TokenKind<'src> {
    pub fn precedence_value(self) -> u8 {
        match self {
            TokenKind::BinOp(bop) => match bop {
                BinOpToken::Eq
                | BinOpToken::PlusEq
                | BinOpToken::MinusEq
                | BinOpToken::TimesEq
                | BinOpToken::DivEq
                | BinOpToken::ModEq
                | BinOpToken::AndEq
                | BinOpToken::OrEq
                | BinOpToken::XorEq
                | BinOpToken::ShlEq
                | BinOpToken::ShrEq => 10,

                BinOpToken::Andq => 18,
                BinOpToken::Orq => 22,

                BinOpToken::EqEq | BinOpToken::Neq => 30,

                BinOpToken::Lt | BinOpToken::Gt | BinOpToken::Leq | BinOpToken::Geq => 40,

                BinOpToken::Colonq => 50,

                BinOpToken::Shl | BinOpToken::Shr => 60,

                BinOpToken::Or | BinOpToken::Xor => 70,

                BinOpToken::Div | BinOpToken::Mod => 90,
            },

            TokenKind::UnOrBinOp(unop) => match unop {
                UnOrBinOpToken::And => 70,

                UnOrBinOpToken::Add | UnOrBinOpToken::Sub => 80,

                UnOrBinOpToken::Star => 90,
            },

            TokenKind::Colon => 50,

            _ => 0,
        }
    }
    pub fn as_str(self) -> &'src str {
        use Delimiter::*;
        use TokenKind::*;
        match self {
            Keyword(kw) => kw.as_str(),
            Ident(id) => id,
            OpenDelimiter(Paren) => "(",
            OpenDelimiter(Bracket) => "[",
            OpenDelimiter(Brace) => "{",
            CloseDelimiter(Paren) => ")",
            CloseDelimiter(Bracket) => "]",
            CloseDelimiter(Brace) => "}",
            Semicolon => ";",
            Colon => ":",
            ColonColon => "::",
            Comma => ",",
            UnOp(op) => op.as_str(),
            BinOp(op) => op.as_str(),
            UnOrBinOp(op) => op.as_str(),
            Literal(lit) => lit.as_str(),
            At => "@",
            Dot => ".",
        }
    }
}

impl<'src> Display for TokenKind<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
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
