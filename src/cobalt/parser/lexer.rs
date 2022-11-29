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
#[allow(unreachable_code)]
fn parse_num<'a>(it: &mut std::iter::Peekable<std::str::Chars>, c: char, loc: &mut Location<'a>, up: bool) -> Result<Token<'a>, Error<'a>> {
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
                let start = loc.clone();
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
            _ => errs.push(Error::new(loc.clone(), 101, format!("U+{:04X} is not a valid character", c as i32)))
        }
        step(flags.up, &mut loc, &c);
    }
    (outs, errs)
}
