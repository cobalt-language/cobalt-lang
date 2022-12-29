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
        if f.alternate() {write!(f, "{:#}: {:?}", self.loc, self.data)}
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
    'main: while let Some(c) = it.next() {
        match c {
            '#' => {
                let start = loc.clone();
                match it.next() {
                    None => break, // single-line, followed by EOF
                    Some('\n') => { // single-line, empty
                        if flags.up {
                            loc.line += 1;
                            loc.col = 1;
                            loc.offset += 2;
                        }
                        continue;
                    },
                    Some('=') => { // multiline
                        let mut count = 1;
                        loop { // count '='s
                            match it.next() {
                                None => {
                                    errs.push(Error::new(start, 102, "unterminated multiline comment".to_string()));
                                    break 'main;
                                },
                                Some('=') => {count += 1;},
                                _ => break
                            }
                        }
                        loop {
                            while let Some(c) = it.next_if(|&x| x != '=') { // skip characters that aren't '='
                                step(flags.up, &mut loc, &c);
                            }
                            if it.peek() == None {
                                errs.push(Error::new(start, 102, "unterminated multiline comment".to_string()));
                                break 'main;
                            }
                            let mut rem = count;
                            while rem > 0 && it.peek() == Some(&'=') { // the number of consecutive '='s
                                if flags.up {
                                    loc.col += 1;
                                    loc.offset += 1;
                                }
                                if rem > 0 { // it's ok if there's extra '='s
                                    rem -= 1;
                                }
                                it.next();
                            }
                            if it.peek() == Some(&'#') { // check to make sure that it's actually ended
                                if flags.up {
                                    loc.col += 1;
                                    loc.offset += 1;
                                }
                                break;
                            }
                        }
                        continue;
                    },
                    Some(_) => { // single-line, non-empty
                        if let Some(pos) = it.position(|x| x == '\n') {
                            if flags.up {
                                loc.line += 1;
                                loc.col = 1;
                                loc.offset += pos as u64 + 1;
                            }
                            continue;
                        }
                        else {break}
                    }
                }
            },
            ' ' | '\r' | '\n' | '\t' => {},
            '\'' => {
                if let Some(c) = it.next() {
                    let start = loc.clone();
                    step(flags.up, &mut loc, &c);
                    if match c {
                        '\\' => 'early_exit: {
                            if let Some(c) = it.next() {
                                step(flags.up, &mut loc, &c);
                                outs.push(Token::new(start.clone(), Char(match c {
                                    'x' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap())));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap()
                                    },
                                    'u' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap_or_else(|| {
                                            errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                            '\0'
                                        })
                                    },
                                    'U' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap())));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            step(flags.up, &mut loc, &c);
                                            if c == '\'' {
                                                outs.push(Token::new(start, Char(char::from_u32(x).unwrap_or_else(|| {
                                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                                    '\0'
                                                }))));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                        }
                                        else {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap_or_else(|| {
                                            errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                            '\0'
                                        })
                                    },
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    'f' => '\x0c',
                                    'v' => '\x0b',
                                    'e' => '\x1b',
                                    'a' => '\x07',
                                    x => x
                                })));
                                true
                            }
                            else {
                                errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                false
                            }
                        },
                        '\'' => {
                            outs.push(Token::new(start.clone(), Char('\0')));
                            errs.push(Error::new(start.clone(), 20, "empty character literal".to_string()));
                            false
                        },
                        x => {
                            outs.push(Token::new(start.clone(), Char(x)));
                            true
                        },
                    } {
                        match it.next() {
                            Some('\'') => if flags.up {loc.col += 1;},
                            Some(x) => {
                                step(flags.up, &mut loc, &x);
                                errs.push(Error::new(loc.clone(), 112, format!("expected end of character literal, got '{x}'")));
                                loop {
                                    match it.next() {
                                        Some('\'') => break,
                                        Some(_) => {},
                                        None => {
                                            errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()));
                                            break 'main;
                                        }
                                    }
                                }
                            },
                            None => errs.push(Error::new(start.clone(), 110, "unterminated character literal".to_string()))
                        }
                    }
                }
                else {
                    errs.push(Error::new(loc.clone(), 110, "unterminated character literal".to_string()));
                }
            },
            '"' => {
                let start = loc.clone();
                let mut out = String::new();
                let mut lwbs = false;
                step(flags.up, &mut loc, &c);
                while let Some(c) = it.next() {
                    step(flags.up, &mut loc, &c);
                    if lwbs {
                        out.push(match c {
                            'x' => {
                                let mut x = 0u32;
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {x |= v;}
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                char::from_u32(x).unwrap()
                            },
                            'u' => {
                                let mut x = 0u32;
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {x |= v;}
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                char::from_u32(x).unwrap_or_else(|| {
                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                    '\0'
                                })
                            },
                            'U' => {
                                let mut x = 0u32;
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {x |= v;}
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Error::new(loc.clone(), 111, format!("unexpected character '{c}' in hex escape sequence")));}
                                }
                                else {
                                    errs.push(Error::new(start.clone(), 113, "unterminated string literal".to_string()));
                                    break 'main;
                                }
                                char::from_u32(x).unwrap_or_else(|| {
                                    errs.push(Error::new(loc.clone(), 114, format!("invalid hex character U+{x:<04X}")));
                                    '\0'
                                })
                            },
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\r',
                            'v' => '\x0b',
                            'f' => '\x0c',
                            'e' => '\x1b',
                            'a' => '\x07',
                            _ => c
                        });
                        lwbs = false;
                    }
                    else {
                        match c {
                            '"' => {
                                outs.push(Token::new(start, Str(out)));
                                continue 'main
                            },
                            '\\' => lwbs = true,
                            _ => out.push(c)
                        }
                    }
                }
                errs.push(Error::new(start, 113, "unterminated string literal".to_string()));
            }
            _ if is_xid_start(c) || c == '$' || c == '_'  => {
                let mut s = c.to_string();
                let start = loc.clone();
                while let Some(c) = it.peek() {
                    if *c == '_' || *c == '$' || is_xid_continue(*c) {s.push(*c);}
                    else {break;}
                    step(flags.up, &mut loc, &c);
                    it.next();
                }
                outs.push(Token::new(start, match s.as_str() {
                    "let" | "mut" | "const" | "fn" | "cr" | "module" | "import" | "if" | "else" | "while" => Keyword(s),
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
            '?' | '~' => { // operator of the from @
                outs.push(Token::new(loc.clone(), Operator(c.to_string())));
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
                    else {
                        outs.push(Token::new(loc.clone(), Operator(c.to_string())));
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
            _ => errs.push(Error::new(loc.clone(), 101, format!("source file cannot contain {:?} outside of a string", c)))
        }
        step(flags.up, &mut loc, &c);
    }
    (outs, errs)
}
