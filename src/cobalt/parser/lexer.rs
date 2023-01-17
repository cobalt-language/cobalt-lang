use crate::*;
use crate::errors::*;
use std::fmt::{self, Display, Formatter};
use unicode_ident::*;
use codespan_reporting::files::Files;
#[derive(Clone, PartialEq, Debug)]
pub enum TokenData {
    Int(i128),
    Float(f64),
    Char(char),
    Str(String),
    Special(char),
    Operator(String),
    Identifier(String),
    Keyword(String),
    Statement(String),
    Macro(String, Option<String>)
}
#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub loc: Location,
    pub data: TokenData
}
impl Token {
    pub fn new(loc: Location, data: TokenData) -> Self {Token {loc, data}}
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            let lock = errors::files::FILES.read().unwrap();
            let start = lock.location(self.loc.0, self.loc.1.start).unwrap();
            let end = lock.location(self.loc.0, self.loc.1.end).unwrap();
            write!(f, "{:?} @ {}:{}..{}:{} (bytes {}..{}) in {}", self.data, start.line_number, start.column_number, end.line_number, end.column_number, self.loc.1.start, self.loc.1.end, lock.name(self.loc.0).unwrap())
        }
        else {write!(f, "{:?}", self.data)}
    }
}
use TokenData::*;
fn parse_num(it: &mut std::iter::Peekable<std::str::Chars>, c: char, loc: &mut (FileId, usize), up: bool) -> Result<Token, Diagnostic> {
    let start = loc.1;
    match c {
        '+' => {
            if up {loc.1 += 1;}
            let c = it.next().unwrap();
            return parse_num(it, c, loc, up).map(|x| Token::new((loc.0, start..x.loc.1.end), x.data));
        },
        '-' => {
            if up {loc.1 += 1;}
            let c = it.next().unwrap();
            return parse_num(it, c, loc, up).map(|x| Token::new((loc.0, start..x.loc.1.end), match x.data {
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
                        if up {loc.1 += 1};
                    },
                    Some('.') => {
                        let mut val = val as f64;
                        let mut dec_places = -1.0;
                        it.next();
                        if up {loc.1 += 1};
                        loop {
                            match it.peek() {
                                Some(c @ '0'..='9') => {
                                    val += c.to_digit(10).unwrap() as f64 * (10f64).powf(dec_places);
                                    dec_places -= 1.0;
                                    if up {loc.1 += 1};
                                }
                                _ => return Ok(Token::new((loc.0, start..(loc.1 + 1)), Float(val)))
                            };
                            it.next();
                        }
                    }
                    _ => return Ok(Token::new((loc.0, start..(loc.1 + 1)), Int(val)))
                };
                it.next();
            }
        },
        '.' => {
            let mut val = 0.0;
            let mut dec_places = -1.0;
            loop {
                match it.peek() {
                    Some(c @ '0'..='9') => {
                        val += c.to_digit(10).unwrap() as f64 * (10f64).powf(dec_places);
                        dec_places -= 1.0;
                        if up {loc.1 += 1};
                    }
                    _ => return Ok(Token::new((loc.0, start..(loc.1 + 1)), Float(val)))
                };
                it.next();
            }
        },
        _ => unreachable!("invalid first character to parse_num")
    }
}
fn parse_macro(it: &mut std::iter::Peekable<std::str::Chars>, loc: &mut (FileId, usize), flags: &Flags) -> (Vec<Token>, Vec<Diagnostic>) {
    let mut name = "".to_string();
    let start = loc.1;
    let mut parse_params = false;
    while let Some(c) = it.peek() {
        parse_params = *c == '(';
        if !(is_xid_continue(*c) || *c == '$' || *c == '_') {break}
        if flags.up {loc.1 += c.len_utf8();}
        name.push(*c);
        it.next();
    }
    if name.len() == 0 {return (vec![], vec![Diagnostic::error((loc.0, start..(start + 1)), 104, None)])}
    let mut errs = vec![];
    let mut param_start = None;
    let params = if parse_params {
        let mut params = "".to_string();
        let mut depth = 1;
        if flags.up {loc.1 += 1};
        param_start = Some(loc.1);
        it.next();
        loop {
            if depth == 0 {break}
            match it.next() {
                Some('(') => {params.push('('); depth += 1; if flags.up {loc.1 += 1;}},
                Some(')') => {if depth > 1 {params.push(')')}; depth -= 1; if flags.up {loc.1 += 1;}},
                Some(c) => {params.push(c); if flags.up {loc.1 += c.len_utf8();}},
                None => {
                    errs.push(Diagnostic::error((loc.0, param_start.unwrap()..loc.1), 103, None).note((loc.0, param_start.unwrap()..(param_start.unwrap() + 1)), "argument list started here".to_string()));
                    break;
                }
            }
        }
        Some(params)
    }
    else {None};
    (match name.as_str() {
        "version" => match params.as_ref().map(|x| x.as_str()) {
            Some("major") => vec![Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap_or(0)))],
            Some("minor") => vec![Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_MINOR").parse().unwrap_or(0)))],
            Some("patch") => vec![Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_PATCH").parse().unwrap_or(0)))],
            Some("array") => vec![
                Token::new((loc.0, (start..(loc.1 + 1))), Special('[')),
                Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap_or(0))),
                Token::new((loc.0, (start..(loc.1 + 1))), Special(',')),
                Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_MINOR").parse().unwrap_or(0))),
                Token::new((loc.0, (start..(loc.1 + 1))), Special(',')),
                Token::new((loc.0, (start..(loc.1 + 1))), Int(env!("CARGO_PKG_VERSION_PATCH").parse().unwrap_or(0))),
                Token::new((loc.0, (start..(loc.1 + 1))), Special(']'))
            ],
            None | Some("") => vec![Token::new((loc.0, start..(loc.1 + 1)), Str(env!("CARGO_PKG_VERSION").to_string()))],
            Some(x) => {
                errs.push(Diagnostic::error((loc.0, (param_start.unwrap() + 1)..loc.1), 110, Some(format!(r#"expected "major", "minor", "patch", or "array", got {x:?}"#))));
                vec![]
            }
        },
        _ => vec![Token::new((loc.0, start..(loc.1 + 1)), Macro(name, params))]
    }, errs)
}
pub fn lex(data: &str, mut loc: (FileId, usize), flags: &Flags) -> (Vec<Token>, Vec<Diagnostic>) {
    let mut outs = vec![];
    let mut errs = vec![];
    let mut it = data.chars().peekable();
    'main: while let Some(c) = it.next() {
        match c {
            '@' => {
                let (mut ts, mut es) = parse_macro(&mut it, &mut loc, flags);
                outs.append(&mut ts);
                errs.append(&mut es);
            },
            '#' => {
                let start = loc.1;
                match it.next() {
                    None => break, // single-line, followed by EOF
                    Some('\n') => { // single-line, empty
                        if flags.up {loc.1 += 1}
                        continue;
                    },
                    Some('=') => { // multiline
                        let mut count = 1;
                        loop { // count '='s
                            match it.next() {
                                None => {
                                    errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 102, None));
                                    break 'main;
                                },
                                Some('=') => {count += 1;},
                                _ => break
                            }
                        }
                        loop {
                            while let Some(c) = it.next_if(|&x| x != '=') { // skip characters that aren't '='
                                if flags.up {loc.1 += c.len_utf8();}
                            }
                            if it.peek() == None {
                                errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 102, None));
                                break 'main;
                            }
                            let mut rem = count;
                            while rem > 0 && it.peek() == Some(&'=') { // the number of consecutive '='s
                                if flags.up {loc.1 += 1}
                                if rem > 0 { // it's ok if there's extra '='s
                                    rem -= 1;
                                }
                                it.next();
                            }
                            if it.peek() == Some(&'#') { // check to make sure that it's actually ended
                                if flags.up {loc.1 += 1}
                                break;
                            }
                        }
                        continue;
                    },
                    Some(_) => { // single-line, non-empty
                        if let Some(pos) = it.position(|x| x == '\n') {
                            if flags.up {loc.1 +=pos + 1;}
                            continue;
                        }
                        else {break}
                    }
                }
            },
            ' ' | '\r' | '\n' | '\t' => {},
            '\'' => {
                if let Some(c) = it.next() {
                    let start = loc.1;
                    if flags.up {loc.1 += c.len_utf8()};
                    if match c {
                        '\\' => 'early_exit: {
                            if let Some(c) = it.next() {
                                if flags.up {loc.1 += c.len_utf8()};
                                let val = match c {
                                    'x' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, None));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap()
                                    },
                                    'u' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap_or_else(|| {
                                            errs.push(Diagnostic::error((loc.0, (start + 1)..(loc.1 + 5)), 133, Some(format!("got value U+{x:0>4X}"))));
                                            '\0'
                                        })
                                    },
                                    'U' => {
                                        let mut x = 0u32;
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {x |= v;}
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        if let Some(c) = it.next() {
                                            if flags.up {loc.1 += c.len_utf8()};
                                            if c == '\'' {
                                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                                break 'early_exit false;
                                            }
                                            if let Some(v) = c.to_digit(16) {
                                                x <<= 4;
                                                x |= v;
                                            }
                                            else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                        }
                                        else {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                        char::from_u32(x).unwrap_or_else(|| {
                                            errs.push(Diagnostic::error((loc.0, (start + 1)..(loc.1 + 10)), 133, Some(format!("got value U+{x:0>4X}"))));
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
                                };
                                outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char(val)));
                                true
                            }
                            else {
                                errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                false
                            }
                        },
                        '\'' => {
                            outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                            errs.push(Diagnostic::warning((loc.0, start..(start + 2)), 20, None));
                            false
                        },
                        x => {
                            if flags.up {loc.1 += x.len_utf8()};
                            outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char(x)));
                            true
                        },
                    } {
                        match it.next() {
                            Some('\'') => {},
                            Some(x) => {
                                if flags.up {loc.1 += x.len_utf8();}
                                loop {
                                    match it.next() {
                                        Some('\'') => break,
                                        Some(x) => if flags.up {loc.1 += x.len_utf8();},
                                        None => {
                                            errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None));
                                            break 'main;
                                        }
                                    }
                                }
                                errs.push(Diagnostic::error((loc.0, (start + 2)..(loc.1 + 1)), 134, Some(r#"expected "'""#.to_string())));
                            },
                            None => errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 130, None))
                        }
                    }
                }
                else {
                    errs.push(Diagnostic::error((loc.0, loc.1..(loc.1 + 1)), 130, None));
                }
            },
            '"' => {
                let start = loc.1;
                let mut out = String::new();
                let mut lwbs = false;
                if flags.up {loc.1 += c.len_utf8()};
                'early_exit: while let Some(c) = it.next() {
                    if flags.up {loc.1 += c.len_utf8()};
                    if lwbs {
                        out.push(match c {
                            'x' => {
                                let mut x = 0u32;
                                if let Some(c) = it.next() {
                                    if flags.up {loc.1 += c.len_utf8()};
                                    if c == '"' {
                                        outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                        break 'early_exit;
                                    }
                                    if let Some(v) = c.to_digit(16) {x |= v;}
                                    else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                }
                                else {
                                    errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 131, None));
                                    break 'main;
                                }
                                if let Some(c) = it.next() {
                                    if flags.up {loc.1 += c.len_utf8()};
                                    if c == '"' {
                                        outs.push(Token::new((loc.0, start..(loc.1 + 1)), Char('\0')));
                                        break 'early_exit;
                                    }
                                    if let Some(v) = c.to_digit(16) {
                                        x <<= 4;
                                        x |= v;
                                    }
                                    else {errs.push(Diagnostic::error((loc.0, (start + 1)..loc.1), 132, Some(format!("expected [0-9a-fA-F], got {c:?}"))));}
                                }
                                else {
                                    errs.push(Diagnostic::error((loc.0, start..(loc.1 + 1)), 131, None));
                                    break 'main;
                                }
                                char::from_u32(x).unwrap()
                            },
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            'v' => '\x0b',
                            'f' => '\x0c',
                            'e' => '\x1b',
                            'a' => '\x07',
                            _ => c
                        });
                        if flags.up {loc.1 += 1};
                        lwbs = false;
                    }
                    else {
                        match c {
                            '"' => {
                                outs.push(Token::new((loc.0, start..loc.1), Str(out)));
                                continue 'main
                            },
                            '\\' => lwbs = true,
                            _ => out.push(c)
                        }
                    }
                }
                errs.push(Diagnostic::error((loc.0, start..loc.1), 131, None));
            }
            _ if is_xid_start(c) || c == '$' || c == '_'  => {
                let mut s = c.to_string();
                let start = loc.1;
                while let Some(c) = it.peek() {
                    let len = c.len_utf8();
                    if *c == '_' || *c == '$' || is_xid_continue(*c) {s.push(*c);}
                    else {break;}
                    it.next();
                    if flags.up {loc.1 += len};
                }
                outs.push(Token::new((loc.0, start..(loc.1 + 1)), match s.as_str() {
                    "let" | "mut" | "const" | "fn" | "cr" | "module" | "import" => Statement(s),
                    "if" | "else" | "while" => Keyword(s),
                    _ => Identifier(s)
                }));
            },
            '0'..='9' => match parse_num(&mut it, c, &mut loc, flags.up) {
                Ok(val) => outs.push(val),
                Err(val) => errs.push(val)
            },
            '(' | ')' | '[' | ']' | '{' | '}' | ';' | ':' | ',' => outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Special(c))),
            '.' => match it.peek() {
                Some(x) if *x >= '0' && *x <= '9' => match parse_num(&mut it, c, &mut loc, flags.up) {
                    Ok(val) => outs.push(val),
                    Err(val) => errs.push(val)
                },
                _ => outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Special('.')))
            },
            '?' | '~' => { // operator of the from @
                outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Operator(c.to_string())));
            },
            '=' | '!' | '%' | '*' => { // operator of the form @, @=
                if it.peek() == Some(&'=') {
                    it.next();
                    outs.push(Token::new((loc.0, loc.1..(loc.1 + 2)), Operator([c, '='].iter().collect())));
                    if flags.up {loc.1 += 1};
                }
                else {
                    outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Operator(c.to_string())));
                }
            },
            '+' | '-' | '&' | '|' | '^' => { // operator of the form @, @@, @=
                if let Some(c2) = it.peek() {
                    let c3 = *c2;
                    if c3 == '=' || c3 == c {
                        it.next();
                        outs.push(Token::new((loc.0, loc.1..(loc.1 + 2)), Operator([c, c3].iter().collect())));
                        if flags.up {loc.1 += 1};
                    }
                    else {
                        outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Operator(c.to_string())));
                    }
                }
                else {
                    outs.push(Token::new((loc.0, loc.1..(loc.1 + 1)), Operator(c.to_string())));
                }
            },
            '<' | '>' => { // operator of the form @, @@, @=, @@=
                let start = loc.1;
                let mut s = c.to_string();
                if it.peek() == Some(&c) {
                    s.push(c);
                    if flags.up {loc.1 += 1;}
                }
                if it.peek() == Some(&'=') {
                    s.push('=');
                    if flags.up {loc.1 += 1;}
                }
                outs.push(Token::new((loc.0, (start..(loc.1 + 1))), Operator(s)));
            },
            _ => errs.push(Diagnostic::error((loc.0, loc.1..(loc.1 + 1)), 101, Some(format!("character is {c:?} (U+{:0>4X})", c as u32))))
        }
        if flags.up {loc.1 += c.len_utf8()};
    }
    (outs, errs)
}
