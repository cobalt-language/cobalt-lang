use crate::{Location, Flags, Error};
#[derive(Clone, PartialEq, Debug)]
pub enum TokenData {
    Int(i128),
    Float(f64),
    Char(char),
    Str(String),
    Special(char),
    Operator(String),
    Identifier(String),
    Keyword(String)
}
#[derive(Clone, PartialEq, Debug)]
pub struct Token<'a> {
    pub loc: Location<'a>,
    pub data: TokenData
}
impl<'a> Token<'a> {
    pub fn new(loc: Location<'a>, data: TokenData) -> Self {Token{loc, data}}
}
use TokenData::*;
fn step(up: bool, loc: &mut Location, c: &char) {
    if up {
        loc.offset += 1;
        if *c == '\n' {
            loc.line += 1;
            loc.col = 1
        }
        else {loc.col += 1}
    }
}
fn parse_num<'a>(it: &mut std::iter::Peekable<std::str::Chars>, c: char, loc: &mut Location<'a>) -> Result<Token<'a>, Error<'a>> {
    Ok(Token::new(loc.clone(), Int(0)))
}
pub fn lex<'a>(data: &str, mut loc: Location<'a>, flags: Flags) -> (Vec<Token<'a>>, Vec<Error<'a>>) {
    let mut outs = vec![];
    let mut errs = vec![];
    let mut it = data.chars().peekable(); 
    while let Some(c) = it.next() {
        match c {
            ' ' | '\r' | '\n' | '\t' => {},
            '_' |
            'a'..='z' |
            'A'..='Z' | 
            'À'..='Ö' |
            'Ø'..='ö' |
            'ø'..='ʯ' |
             '̀'..='ӿ' |
            'Ḁ'..='ἕ' => {
                let mut s = String::from(c);
                while let Some(c) = it.peek() {
                    match c {
                        '_' |
                        '0'..='9' |
                        'a'..='z' |
                        'A'..='Z' | 
                        'À'..='Ö' |
                        'Ø'..='ö' |
                        'ø'..='ʯ' |
                         '̀'..='ӿ' |
                        'Ḁ'..='ἕ' => s.push(*c),
                        _ => break
                    };
                    step(flags.update_location, &mut loc, &c);
                    it.next();
                }
                outs.push(Token::new(loc.clone(), match s.as_str() {
                    "let" | "mut" | "fn" | "cr" | "module" | "import" | "if" | "else" | "while" => Keyword(s),
                    _ if s.starts_with("__builtin_") => Keyword(s),
                    _ => Identifier(s)
                }));
            },
            '0'..='9' => match parse_num(&mut it, c, &mut loc) {
                Ok(val) => outs.push(val),
                Err(val) => errs.push(val)
            },
            '(' | ')' | '[' | ']' | '{' | '}' | ';' | ':' | ',' => outs.push(Token::new(loc.clone(), Special(c))),
            '.' => match it.peek() {
                Some(x) if *x >= '0' && *x <= '9' => match parse_num(&mut it, c, &mut loc) {
                    Ok(val) => outs.push(val),
                    Err(val) => errs.push(val)
                },
                _ => outs.push(Token::new(loc.clone(), Special('.')))
            },
            _ => errs.push(Error::new(loc.clone(), 101, format!("U+{:04X} is not a valid character", c as i32)))
        }
        step(flags.update_location, &mut loc, &c);
    }
    (outs, errs)
}
