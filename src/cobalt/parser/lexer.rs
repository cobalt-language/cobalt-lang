use crate::{Location, Flags, Error};
use std::fmt::{self, Display, Formatter};
use unicode_ident::*;
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
pub struct Token {
    pub loc: Location,
    pub data: TokenData
}
impl Token {
    pub fn new(loc: Location, data: TokenData) -> Self {Token{loc, data}}
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {write!(f, "{}: {:?}", self.loc, self.data)}
        else {write!(f, "{:?}", self.data)}
    }
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
#[allow(unreachable_code)]
fn parse_num(it: &mut std::iter::Peekable<std::str::Chars>, c: char, loc: &mut Location, up: bool) -> Result<Token, Error> {
    let start = loc.clone();
    match c {
        '+' => {
            step(up, loc, &'+');
            let c = it.next().unwrap();
            return parse_num(it, c, loc, up).map(|x| Token::new(start, x.data));
        },
        '-' => {
            step(up, loc, &'-');
            let c = it.next().unwrap();
            return parse_num(it, c, loc, up).map(|x| Token::new(start, match x.data {
                Int(x) => Int(-x),
                Float(x) => Float(-x),
                _ => unreachable!("parse_num returns Int, Float, or Error")
            }));
        },
        '0'..='9' => {
            let mut val = c.to_digit(10).unwrap() as i128;
            loop {
                match it.peek() {
                    Some(c @ '0'..='9') => {
                        val *= 10;
                        val += c.to_digit(10).unwrap() as i128;
                    },
                    Some('.') => {
                        let mut val = val as f64;
                        let mut dec_places = -1.0;
                        it.next();
                        loop {
                            match it.peek() {
                                Some(c @ '0'..='9') => {
                                    val += c.to_digit(10).unwrap() as f64 * (10f64).powf(dec_places);
                                    dec_places -= 1.0;
                                }
                                _ => return Ok(Token::new(start, Float(val)))
                            };
                            it.next();
                        }
                        unreachable!("loop is guaranteed to terminate");
                    }
                    _ => return Ok(Token::new(start, Int(val)))
                };
                it.next();
            }
            unreachable!("loop is guaranteed to terminate");
        },
        '.' => {
            let mut val = 0.0;
            let mut dec_places = -1.0;
            loop {
                match it.peek() {
                    Some(c @ '0'..='9') => {
                        val += c.to_digit(10).unwrap() as f64 * (10f64).powf(dec_places);
                        dec_places -= 1.0;
                    }
                    _ => return Ok(Token::new(start, Float(val)))
                };
                it.next();
            }
            unreachable!("loop is guaranteed to terminate");
        },
        _ => unreachable!("invalid first character to parse_num")
    }
}
pub fn lex(data: &str, mut loc: Location, flags: &Flags) -> (Vec<Token>, Vec<Error>) {
    let mut outs = vec![];
    let mut errs = vec![];
    let mut it = data.chars().peekable(); 
    while let Some(c) = it.next() {
        match c {
            ' ' | '\r' | '\n' | '\t' => {},
            _ if is_xid_start(c) => {
                let mut s = c.to_string();
                let start = loc.clone();
                while let Some(c) = it.peek() {
                    if is_xid_continue(*c) {s.push(*c);}
                    else {break;}
                    step(flags.up, &mut loc, &c);
                    it.next();
                }
                outs.push(Token::new(start, match s.as_str() {
                    "let" | "mut" | "fn" | "cr" | "module" | "import" | "if" | "else" | "while" => Keyword(s),
                    _ if s.starts_with("__builtin_") => Keyword(s),
                    _ => Identifier(s)
                }));
            },
            '0'..='9' => match parse_num(&mut it, c, &mut loc, flags.up) {
                Ok(val) => outs.push(val),
                Err(val) => errs.push(val)
            },
            '(' | ')' | '[' | ']' | '{' | '}' | ';' | ':' | ',' => outs.push(Token::new(loc.clone(), Special(c))),
            '.' => match it.peek() {
                Some(x) if *x >= '0' && *x <= '9' => match parse_num(&mut it, c, &mut loc, flags.up) {
                    Ok(val) => outs.push(val),
                    Err(val) => errs.push(val)
                },
                _ => outs.push(Token::new(loc.clone(), Special('.')))
            },
            '?' => { // operator of the from @
                outs.push(Token::new(loc.clone(), Operator("?".to_string())));
            },
            '=' | '!' | '%' | '*' => { // operator of the form @, @=
                if it.peek() == Some(&'=') {
                    it.next();
                    outs.push(Token::new(loc.clone(), Operator([c, '='].iter().collect())));
                }
                else {
                    outs.push(Token::new(loc.clone(), Operator(c.to_string())));
                }
            },
            '+' | '-' | '&' | '|' | '^' => { // operator of the form @, @@, @=
                if let Some(c2) = it.peek() {
                    let c3 = *c2;
                    drop(c2);
                    if c3 == '=' || c3 == c {
                        it.next();
                        outs.push(Token::new(loc.clone(), Operator([c, c3].iter().collect())));
                    }
                }
                else {
                    outs.push(Token::new(loc.clone(), Operator(c.to_string())));
                }
            },
            '<' | '>' => { // operator of the form @, @@, @=, @@=
                let mut s = c.to_string();
                if it.peek() == Some(&c) {
                    s.push(c);
                }
                if it.peek() == Some(&'=') {
                    s.push('=');
                }
                outs.push(Token::new(loc.clone(), Operator(s)));
            },
            _ => errs.push(Error::new(loc.clone(), 101, format!("U+{:04X} is not a valid character", c as i32)))
        }
        step(flags.up, &mut loc, &c);
    }
    (outs, errs)
}
