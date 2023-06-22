use cobalt_errors::*;
use cobalt_ast::{*, ast::*};
use unicode_ident::*;
use either::Either;
mod utils;

/// General parsing return, return a value, the span it covers, the remaining string, and any
/// errors that were encountered
type ParserReturn<'a, T> = Option<(T, SourceSpan, &'a str, Vec<CobaltError>)>;
/// Get what comes at the start of a string.
/// If it is a valid identifier, return the full identifier, otherwise return the character
fn got(src: &str) -> (ParserFound, usize) {
    let mut it = src.char_indices();
    if let Some((_, c)) = it.next() {
        if is_xid_start(c) {
            let idx = it.find(|x| !is_xid_continue(x.1)).map_or(src.len(), |x| x.0);
            (ParserFound::Str((&src[..idx]).into()), idx)
        }
        else {(ParserFound::Char(c), c.len_utf8())}
    }
    else {(ParserFound::Eof, 0)}
}
/// Take a parser function and update `src`, `start`, and `errs`
fn process<'a, T>(parser: impl FnOnce(&'a str, usize) -> ParserReturn<'a, T>, src: &mut &'a str, start: &mut usize, errs: &mut Vec<CobaltError>) -> Option<(T, SourceSpan)> {
    let (found, span, rem, mut es) = parser(src, *start)?;
    *start += span.len();
    *src = rem;
    errs.append(&mut es);
    Some((found, span))
}

/// Things an identifier cannot be
const KEYWORDS: &[&str] = &[
    "let", "mut", "const", "type", "fn", "module", "import", "if", "else", "while", // currently in use
    "trait", "spec", "break", "continue" // future-proofing
];

/// Parse an identifier
fn ident(allow_empty: bool, src: &str, start: usize) -> ParserReturn<&str> {
    let mut it = src.char_indices();
    let first = it.next()?;
    let first = first.1;
    if !(is_xid_start(first) || first == '$' || first == '_') {return allow_empty.then_some(("", start.into(), src, vec![]))}
    let idx = it.find(|x| !is_xid_continue(x.1)).map_or(src.len(), |x| x.0);
    let (mut id, rem) = src.split_at(idx);
    let mut errs = vec![];
    if KEYWORDS.contains(&id) {
        if allow_empty {
            errs.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Str(id.into()),
                loc: (start, idx).into()
            });
            id = "<error>";
        }
        else {return None}
    }
    Some((id, (start, idx).into(), rem, errs))
}
/// Parse any kind of whitepace
fn whitespace(src: &str, start: usize) -> ParserReturn<()> {
    let idx = src.char_indices().find(|x| !x.1.is_whitespace()).map_or(src.len(), |x| x.0);
    (idx > 0).then_some(((), (start, idx).into(), &src[idx..], vec![]))
}
/// Parse a comment
fn comment(src: &str, start: usize) -> ParserReturn<()> {
    let mut it = src.char_indices();
    if it.next() != Some((0, '#')) {return None}
    match it.next() {
        Some((1, '=')) => {
            if let Some(c) = it.by_ref().find(|x| x.1 != '=').map(|x| x.0) {
                if let Some(idx) = src[c..].find(&("=".repeat(c - 1) + "#")) {
                    let final_len = idx + 2 * c;
                    Some(((), (start, final_len).into(), &src[final_len..], vec![]))
                }
                else {Some(((), (start, c).into(), "", vec![CobaltError::UnclosedComment {loc: (start, c).into()}]))}
            }
            else {Some(((), (start, src.len()).into(), "", vec![CobaltError::UnclosedComment {loc: (start, src.len()).into()}]))}
        },
        Some((1, '\n')) => Some(((), (start, 1).into(), &src[1..], vec![])),
        Some(_) => {
            let idx = it.find(|x| x.1 == '\n').map_or(src.len(), |x| x.0);
            Some(((), (start, idx).into(), &src[idx..], vec![]))
        },
        None => Some(((), (start, 1).into(), "", vec![]))
    }
}
/// Match anything that should be ignored: whitespace or comments
fn ignored(mut src: &str, start: usize) -> ParserReturn<()> {
    let mut cont = true;
    let mut good = false;
    let mut errs = vec![];
    let mut current = start;
    while cont {
        cont = false;
        if let Some((_, span, next, _)) = whitespace(src, current) {
            cont = true;
            good = true;
            src = next;
            current += span.len();
        }
        if let Some((_, span, next, mut es)) = comment(src, current) {
            cont = true;
            good = true;
            src = next;
            current += span.len();
            errs.append(&mut es);
        }
    }
    good.then_some(((), (start..current).into(), src, errs))
}
/// Used for parsing a keyword, followed by some whitespace
fn start_match<'a>(kw: &'static str, mut src: &'a str, mut start: usize) -> ParserReturn<'a, ()> {
    let begin = start;
    let kwl = kw.len();
    src.starts_with(kw).then_some(())?;
    src = &src[kwl..];
    start += kwl;
    (!src.chars().next().map_or(false, is_xid_continue)).then_some(())?;
    let mut errs = vec![];
    process(ignored, &mut src, &mut start, &mut errs);
    Some(((), (begin..start).into(), src, errs))
}
/// A local identifier is just an ident
fn local_id(mut src: &str, mut start: usize) -> ParserReturn<DottedName> {
    let mut errs = vec![];
    let old = start;
    if src.starts_with('.') {
        errs.push(CobaltError::UnexpectedGlobal {loc: (start, 1).into()});
        src = &src[1..];
    }
    process(ignored, &mut src, &mut start, &mut errs);
    let name = vec![process(|src, start| ident(true, src, start), &mut src, &mut start, &mut errs).map_or((String::new(), start.into()), |(seg, loc)| (seg.to_string(), loc))];
    process(ignored, &mut src, &mut start, &mut errs);
    let begin = start;
    process(global_id, &mut src, &mut start, &mut vec![]); // in case someone decided to try to make a global variable
    if begin != start {
        errs.push(CobaltError::UnexpectedGlobal {loc: (begin, 1).into()});
    }
    Some((DottedName::new(name, false), (old..start).into(), src, errs))
}
/// A global identifier is an optional '.', and then a series of idents separated by '.'s
fn global_id(mut src: &str, mut start: usize) -> ParserReturn<DottedName> {
    let old = start;
    let global = if src.starts_with('.') {src = &src[1..]; start += 1; true} else {false};
    let mut errs = vec![];
    process(ignored, &mut src, &mut start, &mut errs);
    let mut name = vec![process(|src, start| ident(true, src, start), &mut src, &mut start, &mut errs).map_or((String::new(), start.into()), |(seg, loc)| (seg.to_string(), loc))];
    loop {
        process(ignored, &mut src, &mut start, &mut errs);
        if !src.starts_with('.') {break}
        src = &src[1..];
        start += 1;
        process(ignored, &mut src, &mut start, &mut errs);
        if let Some((seg, loc)) = process(|src, start| ident(true, src, start), &mut src, &mut start, &mut errs) {name.push((seg.to_string(), loc))}
        else {
            // simple error recovery: look for a '.' (the next element) or ';' (the end)
            let idx = src.find(['.', ';']);
            let g = got(src);
            if let Some(mut idx) = idx {
                if src.as_bytes()[idx] == b';' {
                    idx -= 1;
                    while !src.is_char_boundary(idx) {idx -= 1} // src.floor_char_boundary isn't stable
                }
                start += idx;
                src = &src[idx..];
            }
            else {
                start += src.len();
                src = "";
            }
            errs.push(CobaltError::ExpectedFound {
                ex: r#""." between name elements"#,
                found: g.0,
                loc: (start, g.1).into()
            });
        }
    }
    Some((DottedName::new(name, global), (old..start).into(), src, errs))
}
/// Parse an annotation
fn annotation(mut src: &str, mut start: usize) -> ParserReturn<(&str, Option<&str>, SourceSpan)> {
    let begin = start;
    src.starts_with('@').then_some(())?;
    src = &src[1..];
    start += 1;
    let mut errs = vec![];
    let name = process(move |src, start| ident(true, src, start), &mut src, &mut start, &mut errs).unwrap().0;
    let src_ = src;
    let start_ = start;
    process(ignored, &mut src, &mut start, &mut errs);
    let arg = if src.starts_with('(') {
        src = &src[1..];
        start += 1;
        let mut depth = 1;
        while let Some(next) = src.find(['(', ')']) {
            match src.as_bytes()[next] {
                b'(' => depth += 1,
                b')' => depth -= 1,
                _ => unreachable!()
            }
            start += next + 1;
            src = &src[(next + 1)..];
            if depth == 0 {break}
        }
        if depth > 0 {
            let got = got(src);
            errs.push(CobaltError::UnmatchedDelimiter {
                expected: ')',
                found: got.0,
                start: (start_ + 1, 1).into(),
                end: (start, got.1).into()
            });
        }
        let arg = &src_[1..(start - start_ - 1)];
        Some(arg)
    }
    else {
        src = src_;
        start = start_;
        None
    };
    let end = start;
    process(ignored, &mut src, &mut start, &mut errs);
    Some(((name, arg, (begin..end).into()), (begin..start).into(), src, errs))
}
/// Location of the declarations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DeclLoc {Local, Method, Global}
/// Parse declarations: `let`, `mut`, `const`, `type`, and `fn`
fn declarations<'a>(loc: DeclLoc, anns: Option<Vec<(&'a str, Option<&'a str>, SourceSpan)>>, mut src: &'a str, mut start: usize) -> ParserReturn<'a, Box<dyn AST>> {
    let id_parser = if loc == DeclLoc::Global {global_id} else {local_id};
    let mut errs = vec![];
    let begin = start;
    let anns = anns.unwrap_or_else(|| std::iter::from_fn(|| process(annotation, &mut src, &mut start, &mut errs)).map(|x| x.0).collect());
    match src.as_bytes()[0] { // do a trie-like lookup for speed
        b'l' => {
            let (_, start_span, mut src, mut errs) = start_match("let", src, start)?;
            start += start_span.len();
            let name = process(id_parser, &mut src, &mut start, &mut errs).map_or(DottedName::local((String::new(), start.into())), |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let ty = src.starts_with(':').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(0, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten();
            let val = src.starts_with('=').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten().unwrap_or_else(|| Box::new(NullAST::new(ty.as_ref().map_or(start_span, |x| x.loc()))));
            let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
            let ast = Box::new(VarDefAST::new((begin, 3).into(), name, val, ty, anns, loc != DeclLoc::Local));
            Some((ast, merge_spans(start_span, start.into()), src, errs))
        },
        b'm' => {
            let (_, start_span, mut src, mut errs) = start_match("mut", src, start)?;
            start += start_span.len();
            let name = process(id_parser, &mut src, &mut start, &mut errs).map_or(DottedName::local((String::new(), start.into())), |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let ty = src.starts_with(':').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(0, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten();
            let val = src.starts_with('=').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten().unwrap_or_else(|| Box::new(NullAST::new(ty.as_ref().map_or(start_span, |x| x.loc()))));
            let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
            let ast = Box::new(MutDefAST::new((begin, 3).into(), name, val, ty, anns, loc != DeclLoc::Local));
            Some((ast, merge_spans(start_span, start.into()), src, errs))
        },
        b'c' => {
            let (_, start_span, mut src, mut errs) = start_match("const", src, start)?;
            start += start_span.len();
            let name = process(id_parser, &mut src, &mut start, &mut errs).map_or(DottedName::local((String::new(), start.into())), |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let ty = src.starts_with(':').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(0, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten();
            let val = src.starts_with('=').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten().unwrap_or_else(|| Box::new(NullAST::new(ty.as_ref().map_or(start_span, |x| x.loc()))));
            let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
            let ast = Box::new(ConstDefAST::new((begin, 5).into(), name, val, ty, anns));
            Some((ast, merge_spans(start_span, start.into()), src, errs))
        },
        b't' => {
            let (_, start_span, mut src, mut errs) = start_match("type", src, start)?;
            start += start_span.len();
            let name = process(id_parser, &mut src, &mut start, &mut errs).map_or(DottedName::local((String::new(), start.into())), |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let val = src.starts_with('=').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                let res = process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                process(ignored, &mut src, &mut start, &mut errs);
                res
            }).flatten().unwrap_or_else(|| Box::new(NullAST::new(start_span)));
            process(ignored, &mut src, &mut start, &mut errs);
            let metds = src.starts_with("::").then(|| {
                src = &src[2..];
                start += 2;
                process(ignored, &mut src, &mut start, &mut errs);
                if !src.starts_with('{') {return vec![]}
                let open = start;
                src = &src[1..];
                start += 1;
                let metds = std::iter::from_fn(|| {
                    process(ignored, &mut src, &mut start, &mut errs);
                    if src.starts_with(';') {return Some(None)}
                    let ast = process(|src, start| declarations(DeclLoc::Method, None, src, start), &mut src, &mut start, &mut errs)?.0;
                    process(ignored, &mut src, &mut start, &mut errs);
                    if src.starts_with(';') {
                        src = &src[1..];
                        start += 1;
                    }
                    else {
                        let got = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: r#"";" after declaration"#,
                            found: got.0,
                            loc: (start, got.1).into()
                        });
                    }
                    Some(Some(ast))
                }).flatten().collect::<Vec<_>>();
                process(ignored, &mut src, &mut start, &mut errs);
                if src.starts_with('}') {
                    src = &src[1..];
                    start += 1;
                }
                else {
                    let g = got(src);
                    errs.push(CobaltError::UnmatchedDelimiter {
                        expected: '}',
                        found: g.0,
                        start: (open, 1).into(),
                        end: (start, g.1).into()
                    });
                    let idx = src.find(['}', ';']).unwrap_or(src.len());
                    src = &src[idx..];
                    start += idx;
                }
                metds
            }).unwrap_or_default();
            let ast = Box::new(TypeDefAST::new((begin, 4).into(), name, val, anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect(), metds));
            Some((ast, merge_spans(start_span, start.into()), src, errs))
        },
        b'f' => {
            let (_, start_span, mut src, mut errs) = start_match("fn", src, start)?;
            start += start_span.len();
            let name = process(id_parser, &mut src, &mut start, &mut errs).map_or(DottedName::local((String::new(), start.into())), |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let open = start;
            if !src.starts_with('(') {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: "function parameters",
                    found: got.0,
                    loc: (start, got.1).into()
                });
            }
            else {
                src = &src[1..];
                start += 1;
            }
            fn param(mut src: &str, mut start: usize) -> ParserReturn<ast::funcs::Parameter> {
                let mut errs = vec![];
                let begin = start;
                loop {
                    process(ignored, &mut src, &mut start, &mut errs);
                    if src.is_empty() || src.starts_with(')') {return None}
                    if src.starts_with(',') {
                        let got = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: "parameter",
                            found: got.0,
                            loc: (start, got.1).into()
                        });
                        process(ignored, &mut src, &mut start, &mut errs);
                    } else {break}
                }
                let pt = process(|src, start| start_match("const", src, start), &mut src, &mut start, &mut errs).map(|_| ParamType::Constant)
                    .or_else(|| process(|src, start| start_match("const", src, start), &mut src, &mut start, &mut errs).map(|_| ParamType::Mutable))
                    .unwrap_or(ParamType::Normal);
                let (id, iloc) = process(|src, start| ident(true, src, start), &mut src, &mut start, &mut errs).unwrap_or_else(|| {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: "a parameter",
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    let s = start;
                    process(ignored, &mut src, &mut start, &mut errs);
                    ("", s.into())
                });
                process(ignored, &mut src, &mut start, &mut errs);
                let ty = src.starts_with(':').then(|| {
                    src = &src[1..];
                    start += 1;
                    process(ignored, &mut src, &mut start, &mut errs);
                    let res = process(|src, start| expr(0, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                    process(ignored, &mut src, &mut start, &mut errs);
                    res
                }).flatten().unwrap_or_else(|| {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: "parameter type",
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    process(ignored, &mut src, &mut start, &mut errs);
                    Box::new(ErrorTypeAST::new(iloc))
                });
                let val = src.starts_with('=').then(|| {
                    src = &src[1..];
                    start += 1;
                    process(ignored, &mut src, &mut start, &mut errs);
                    let res = process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map(|x| x.0);
                    process(ignored, &mut src, &mut start, &mut errs);
                    res
                }).flatten();
                match src.as_bytes().first().copied() {
                    Some(b')') => {},
                    Some(b',') => {
                        src = &src[1..];
                        start += 1;
                    },
                    _ => {
                        let got = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: r#""," between parameters"#,
                            found: got.0,
                            loc: (start, got.1).into()
                        });
                    }
                }
                Some(((id.to_string(), pt, ty, val), (begin..start).into(), src, errs))
            }
            let params = std::iter::from_fn(|| process(param, &mut src, &mut start, &mut errs)).map(|x| x.0).collect::<Vec<_>>();
            process(ignored, &mut src, &mut start, &mut errs);
            if src.starts_with(')') {
                src = &src[1..];
                start += 1;
            }
            else {
                let got = got(src);
                errs.push(CobaltError::UnmatchedDelimiter {
                    expected: ')',
                    found: got.0,
                    start: (open, 1).into(),
                    end: (start, got.1).into()
                });
            }
            let cparen = start;
            process(ignored, &mut src, &mut start, &mut errs);
            let ret = src.starts_with(':').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                process(|src, start| expr(0, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: "return type",
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    Box::new(ErrorTypeAST::new(cparen.into())) as _
                }, |x| x.0)
            }).unwrap_or_else(|| Box::new(NullAST::new(cparen.into())) as _);
            process(ignored, &mut src, &mut start, &mut errs);
            let body = src.starts_with('=').then(|| {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                process(|src, start| expr(1, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: "function body",
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    Box::new(ErrorTypeAST::new(ret.loc())) as _
                }, |x| x.0)
            }).unwrap_or_else(|| Box::new(NullAST::new(ret.loc())) as _);
            let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
            Some((Box::new(FnDefAST::new((begin, 2).into(), name, ret, params, body, anns, loc == DeclLoc::Method)), (begin..start).into(), src, errs))
        },
        _ => None
    }
}
/// Parse a statement
/// A statement is a local declaration, import, or expression
fn stmt(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
    let mut errs = vec![];
    process(ignored, &mut src, &mut start, &mut errs);
    let src_ = src;
    let start_ = start;
    let anns: Vec<_> = std::iter::from_fn(|| process(annotation, &mut src, &mut start, &mut errs)).map(|x| x.0).collect();
    None
        .or_else(|| import(&anns, src, start))
        .or_else(move || declarations(DeclLoc::Local, Some(anns), src, start))
        .or_else(|| expr(1, src_, start_))
}
/// Parse an atom
/// An atom cannot be subdivided
fn atom(src: &str, start: usize) -> ParserReturn<Box<dyn AST>> {
    fn varget(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let global = if src.starts_with('.') {
            src = &src[1..];
            start += 1;
            true
        } else {false};
        ident(false, src, start).map(|(name, loc, rem, errs)| (Box::new(VarGetAST::new(loc, name.to_string(), global)) as _, if global {(loc.offset() - 1, loc.len() + 1).into()} else {loc}, rem, errs))
    }
    fn intrin(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        src.starts_with('@').then_some(())?;
        let begin = start;
        src = &src[1..];
        start += 1;
        let name = process(|src, start| ident(true, src, start), &mut src, &mut start, &mut vec![]).unwrap().0;
        Some((Box::new(IntrinsicAST::new((begin..start).into(), name.to_string())), (begin..start).into(), src, vec![]))
    }
    fn    num(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let mut errs = vec![];
        let begin = start;
        if src.starts_with('0') {
            let val = match src.as_bytes().get(1).copied() {
                Some(b'o') => {
                    src = &src[2..];
                    start += 2;
                    let mut val = 0u64;
                    while let Some(d) = src.chars().next().and_then(|c| c.to_digit(8)) {
                        val *= 10;
                        val += d as u64;
                        src = &src[1..];
                        start += 1;
                    }
                    if src.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        errs.push(CobaltError::UnexpectedDecimal {
                            loc: (start, 1).into(),
                            lit: "octal"
                        });
                        let idx = src.find(|c: char| !c.is_ascii_digit()).unwrap_or(src.len()); // recovery: skip all decimal digits
                        start += idx;
                        src = &src[idx..];
                    }
                    let val = if src.starts_with('.') && src[1..].starts_with(|c: char| c.is_digit(8)) {
                        let mut val = val as f64;
                        let mut exp = 0.125f64;
                        while let Some(d) = src.chars().next().and_then(|c| c.to_digit(8)) {
                            val += d as f64 * exp;
                            src = &src[1..];
                            start += 1;
                            exp *= 0.125;
                        }
                        Either::Right(val)
                    } else {Either::Left(val)};
                    if src.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        errs.push(CobaltError::UnexpectedDecimal {
                            loc: (start, 1).into(),
                            lit: "octal"
                        });
                        let idx = src.find(|c: char| !c.is_ascii_digit()).unwrap_or(src.len()); // recovery: skip all decimal digits
                        start += idx;
                        src = &src[idx..];
                    }
                    val
                },
                Some(b'x') => {
                    src = &src[2..];
                    start += 2;
                    let mut val = 0u64;
                    while let Some(d) = src.chars().next().and_then(|c| c.to_digit(16)) {
                        val *= 10;
                        val += d as u64;
                        src = &src[1..];
                        start += 1;
                    }
                    if src.starts_with('.') && src[1..].starts_with(|c: char| c.is_ascii_hexdigit()) {
                        let mut val = val as f64;
                        let mut exp = 0.0625f64;
                        while let Some(d) = src.chars().next().and_then(|c| c.to_digit(16)) {
                            val += d as f64 * exp;
                            src = &src[1..];
                            start += 1;
                            exp *= 0.0625;
                        }
                        Either::Right(val)
                    } else {Either::Left(val)}
                },
                Some(b'b') => {
                    src = &src[2..];
                    start += 2;
                    let mut val = 0u64;
                    while let Some(d) = src.chars().next().and_then(|c| c.to_digit(2)) {
                        val *= 10;
                        val += d as u64;
                        src = &src[1..];
                        start += 1;
                    }
                    if src.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        errs.push(CobaltError::UnexpectedDecimal {
                            loc: (start, 1).into(),
                            lit: "binary"
                        });
                        let idx = src.find(|c: char| !c.is_ascii_digit()).unwrap_or(src.len()); // recovery: skip all decimal digits
                        start += idx;
                        src = &src[idx..];
                    }
                    let val = if src.starts_with('.') && src[1..].starts_with(|c: char| c.is_digit(2)) {
                        let mut val = val as f64;
                        let mut exp = 0.5f64;
                        while let Some(d) = src.chars().next().and_then(|c| c.to_digit(2)) {
                            val += d as f64 * exp;
                            src = &src[1..];
                            start += 1;
                            exp *= 0.5;
                        }
                        Either::Right(val)
                    } else {Either::Left(val)};
                    if src.chars().next().map(|c: char| c.is_ascii_digit()).unwrap_or(false) {
                        errs.push(CobaltError::UnexpectedDecimal {
                            loc: (start, 1).into(),
                            lit: "binary"
                        });
                        let idx = src.find(|c: char| !c.is_ascii_digit()).unwrap_or(src.len()); // recovery: skip all decimal digits
                        start += idx;
                        src = &src[idx..];
                    }
                    val
                },
                Some(b'0'..=b'9') => {
                    let mut val = 0u64;
                    while let Some(d) = src.chars().next().and_then(|c| c.to_digit(10)) {
                        val *= 10;
                        val += d as u64;
                        src = &src[1..];
                        start += 1;
                    }
                    if src.starts_with('.') && src[1..].starts_with(|c: char| c.is_ascii_digit()) {
                        let mut val = val as f64;
                        let mut exp = 0.1f64;
                        while let Some(d) = src.chars().next().and_then(|c| c.to_digit(10)) {
                            val += d as f64 * exp;
                            src = &src[1..];
                            start += 1;
                            exp *= 0.1;
                        }
                        Either::Right(val)
                    } else {Either::Left(val)}
                },
                _ => {
                    src = &src[1..];
                    start += 1;
                    Either::Left(0)
                },
            };
            let end = start;
            process(ignored, &mut src, &mut start, &mut errs);
            let suf = process(|src, start| ident(false, src, start), &mut src, &mut start, &mut errs).map(|(suf, loc)| (suf.to_string(), loc));
            let ast = match val {
                Either::Left(val)  => Box::new(IntLiteralAST::new((begin..end).into(), val as i128, suf)) as Box<dyn AST>,
                Either::Right(val) => Box::new(FloatLiteralAST::new((begin..end).into(), val, suf)) as Box<dyn AST>
            };
            Some((ast, (begin..start).into(), src, errs))
        }
        else if src.starts_with(['1', '2', '3', '4', '5', '6', '7', '8', '9']) {
            let mut val = 0u64;
            while let Some(d) = src.chars().next().and_then(|c| c.to_digit(10)) {
                val *= 10;
                val += d as u64;
                src = &src[1..];
                start += 1;
            }
            let val = if src.starts_with('.') && src[1..].starts_with(|c: char| c.is_ascii_digit()) {
                let mut val = val as f64;
                let mut exp = 0.1f64;
                while let Some(d) = src.chars().next().and_then(|c| c.to_digit(10)) {
                    val += d as f64 * exp;
                    src = &src[1..];
                    start += 1;
                    exp *= 0.1;
                }
                Either::Right(val)
            } else {Either::Left(val)};
            let end = start;
            process(ignored, &mut src, &mut start, &mut errs);
            let suf = process(|src, start| ident(false, src, start), &mut src, &mut start, &mut errs).map(|(suf, loc)| (suf.to_string(), loc));
            let ast = match val {
                Either::Left(val)  => Box::new(IntLiteralAST::new((begin..end).into(), val as i128, suf)) as Box<dyn AST>,
                Either::Right(val) => Box::new(FloatLiteralAST::new((begin..end).into(), val, suf)) as Box<dyn AST>
            };
            Some((ast, (begin..start).into(), src, errs))
        }
        else {None}
    }
    fn strlit(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        if !src.starts_with('"') {return None}
        let begin = start;
        src = &src[1..];
        start += 1;
        let mut errs = vec![];
        let mut out = vec![];
        let mut happy = false;
        while let Some(idx) = src.find(['\\', '"']) {
            out.extend_from_slice(&src.as_bytes()[..idx]);
            src = &src[idx..];
            start += idx;
            if src.as_bytes()[0] == b'"' {
                happy = true;
                src = &src[1..];
                start += 1;
                break
            }
            src = &src[1..];
            start += 1;
            match src.chars().next() {
                None => break,
                Some(c @ ('n' | 'r' | 't' | 'a' | '0')) => {
                    src = &src[1..];
                    start += 1;
                    out.push(match c {
                        'n' => b'\n',
                        'r' => b'\r',
                        't' => b'\t',
                        '0' => b'\0',
                        'a' => 0x07,
                        'e' => 0x1b,
                        _ => unreachable!()
                    });
                }
                Some('x') => {
                    let mut byte = 0u8;
                    src = &src[1..];
                    start += 1;
                    if let Some(c) = src.chars().next() {
                        if let Some(d) = c.to_digit(16) {
                            byte = (d as u8) << 4;
                            src = &src[1..];
                            start += 1;
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: "hexadecimal character in escape code",
                                found: got.0,
                                loc: (start, got.1).into(),
                            });
                            src = &src[c.len_utf8()..];
                            start += c.len_utf8();
                        }
                    }
                    else {break}
                    if let Some(c) = src.chars().next() {
                        if let Some(d) = c.to_digit(16) {
                            byte |= d as u8;
                            src = &src[1..];
                            start += 1;
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: "hexadecimal character in escape code",
                                found: got.0,
                                loc: (start, got.1).into(),
                            });
                            src = &src[c.len_utf8()..];
                            start += c.len_utf8();
                        }
                    }
                    else {break}
                    out.push(byte);
                },
                Some('c') => {
                    let mut byte = 0u8;
                    src = &src[1..];
                    start += 1;
                    if let Some(c) = src.chars().next() {
                        if let Some(d) = c.to_digit(16) {
                            byte = (d as u8) << 4;
                            src = &src[1..];
                            start += 1;
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: "hexadecimal character in escape code",
                                found: got.0,
                                loc: (start, got.1).into(),
                            });
                            src = &src[c.len_utf8()..];
                            start += c.len_utf8();
                        }
                    }
                    else {break}
                    if let Some(c) = src.chars().next() {
                        if let Some(d) = c.to_digit(16) {
                            byte |= d as u8;
                            src = &src[1..];
                            start += 1;
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: "hexadecimal character in escape code",
                                found: got.0,
                                loc: (start, got.1).into(),
                            });
                            src = &src[c.len_utf8()..];
                            start += c.len_utf8();
                        }
                    }
                    else {break}
                    out.extend(utils::CharBytesIterator::from_u8(byte));
                },
                Some('u') => {
                    let mut cp = 0u32;
                    src = &src[1..];
                    start += 1;
                    let open = start;
                    if !src.starts_with('{') {
                        let got = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: r#""{" for unicode escape"#,
                            found: got.0,
                            loc: (start, got.1).into()
                        });
                        continue
                    }
                    src = &src[1..];
                    start += 1;
                    let mut happy = false;
                    let mut rem = false;
                    let mut chars = 0;
                    while let Some(c) = src.chars().next() {
                        if let Some(d) = c.to_digit(16) {
                            if chars < 6 {
                                cp <<= 4;
                                cp |= d;
                                src = &src[1..];
                                start += 1;
                            }
                            else {
                                errs.push(CobaltError::UnicodeSequenceTooLong {loc: (open + 1, 7).into()});
                                break
                            }
                        }
                        else if c == '}' {
                            happy = true;
                            rem = true;
                            src = &src[1..];
                            start += 1;
                            break
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: "hexadecimal character in escape code",
                                found: got.0,
                                loc: (start, got.1).into(),
                            });
                            src = &src[c.len_utf8()..];
                            start += c.len_utf8();
                            rem = true;
                            break
                        }
                        chars += 1;
                    }
                    if !rem {break}
                    if !happy {
                        let got = got(src);
                        errs.push(CobaltError::UnmatchedDelimiter {
                            expected: '}',
                            found: got.0,
                            start: (open, 1).into(),
                            end: (start, got.1).into()
                        });
                    }
                    out.extend(utils::CharBytesIterator::from_u32(cp).unwrap_or_else(|| {
                        errs.push(CobaltError::InvalidCodepoint {
                            val: cp,
                            loc: ((open + 1)..(start - 1)).into()
                        });
                        utils::CharBytesIterator::from_u8(0)
                    }));
                },
                Some(c) => {
                    out.extend(utils::CharBytesIterator::from_char(c));
                    src = &src[c.len_utf8()..];
                    start += c.len_utf8();
                }
            }
        }
        if !happy {
            errs.push(CobaltError::UnmatchedDelimiter {
                expected: '"',
                found: ParserFound::Eof,
                start: (begin, 1).into(),
                end: start.into()
            });
        }
        let end = start;
        process(ignored, &mut src, &mut start, &mut errs);
        let suf = process(|src, start| ident(false, src, start), &mut src, &mut start, &mut errs).map(|(suf, loc)| (suf.to_string(), loc));
        Some((Box::new(StringLiteralAST::new((begin..end).into(), out, suf)), (begin..start).into(), src, errs))
    }
    fn chrlit(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        if !src.starts_with('\'') {return None}
        let begin = start;
        src = &src[1..];
        start += 1;
        let mut errs = vec![];
        let ch = 'main: {
            if src.starts_with('\\') {
                src = &src[1..];
                start += 1;
                match src.chars().next() {
                    None => {
                        errs.push(CobaltError::UnmatchedDelimiter {
                            expected: '\'',
                            found: ParserFound::Eof,
                            start: (begin, 1).into(),
                            end: start.into()
                        });
                        0u32
                    },
                    Some(c @ ('n' | 'r' | 't' | 'a' | '0')) => {
                        src = &src[1..];
                        start += 1;
                        (match c {
                            'n' => b'\n',
                            'r' => b'\r',
                            't' => b'\t',
                            '0' => b'\0',
                            'a' => 0x07,
                            'e' => 0x1b,
                            _ => unreachable!()
                        }) as u32
                    },
                    Some('c') => {
                        let mut byte = 0u32;
                        src = &src[1..];
                        start += 1;
                        if let Some(c) = src.chars().next() {
                            if let Some(d) = c.to_digit(16) {
                                byte = d << 4;
                                src = &src[1..];
                                start += 1;
                            }
                            else {
                                let got = got(src);
                                errs.push(CobaltError::ExpectedFound {
                                    ex: "hexadecimal character in escape code",
                                    found: got.0,
                                    loc: (start, got.1).into(),
                                });
                                src = &src[c.len_utf8()..];
                                start += c.len_utf8();
                            }
                        }
                        else {break 'main 0}
                        if let Some(c) = src.chars().next() {
                            if let Some(d) = c.to_digit(16) {
                                byte |= d;
                                src = &src[1..];
                                start += 1;
                            }
                            else {
                                let got = got(src);
                                errs.push(CobaltError::ExpectedFound {
                                    ex: "hexadecimal character in escape code",
                                    found: got.0,
                                    loc: (start, got.1).into(),
                                });
                                src = &src[c.len_utf8()..];
                                start += c.len_utf8();
                            }
                        }
                        else {break 'main byte}
                        byte
                    },
                    Some('u') => {
                        let mut cp = 0u32;
                        src = &src[1..];
                        start += 1;
                        let open = start;
                        if !src.starts_with('{') {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: r#""{" for unicode escape"#,
                                found: got.0,
                                loc: (start, got.1).into()
                            });
                            break 'main 0
                        }
                        src = &src[1..];
                        start += 1;
                        let mut happy = false;
                        let mut rem = false;
                        let mut chars = 0;
                        while let Some(c) = src.chars().next() {
                            if let Some(d) = c.to_digit(16) {
                                if chars < 6 {
                                    cp <<= 4;
                                    cp |= d;
                                    src = &src[1..];
                                    start += 1;
                                }
                                else {
                                    errs.push(CobaltError::UnicodeSequenceTooLong {loc: (open + 1, 7).into()});
                                    break
                                }
                            }
                            else if c == '}' {
                                happy = true;
                                rem = true;
                                src = &src[1..];
                                start += 1;
                                break
                            }
                            else {
                                let got = got(src);
                                errs.push(CobaltError::ExpectedFound {
                                    ex: "hexadecimal character in escape code",
                                    found: got.0,
                                    loc: (start, got.1).into(),
                                });
                                src = &src[c.len_utf8()..];
                                start += c.len_utf8();
                                rem = true;
                                break
                            }
                            chars += 1;
                        }
                        if !rem {break 'main cp}
                        if !happy {
                            let got = got(src);
                            errs.push(CobaltError::UnmatchedDelimiter {
                                expected: '}',
                                found: got.0,
                                start: (open, 1).into(),
                                end: (start, got.1).into()
                            });
                        }
                        if cp < 0x200000 {cp} else {
                            errs.push(CobaltError::InvalidCodepoint {
                                val: cp,
                                loc: ((open + 1)..(start - 1)).into()
                            });
                            0
                        }
                    },
                    Some(c) => c as u32
                }
            }
            else if src.starts_with('\'') {b'\'' as u32}
            else {
                src.chars().next().map_or(0, |c| {
                    src = &src[c.len_utf8()..];
                    start += c.len_utf8();
                    c as u32
                })
            }
        };
        if src.starts_with('\'') {
            src = &src[1..];
            start += 1;
        }
        else {
            let got = got(src);
            errs.push(CobaltError::UnmatchedDelimiter {
                expected: '\'',
                found: got.0,
                start: (begin, 1).into(),
                end: (start, got.1).into()
            });
        }
        let end = start;
        process(ignored, &mut src, &mut start, &mut errs);
        let suf = process(|src, start| ident(false, src, start), &mut src, &mut start, &mut errs).map(|(suf, loc)| (suf.to_string(), loc));
        Some((Box::new(CharLiteralAST::new((begin..end).into(), ch, suf)), (begin..start).into(), src, errs))
    }
    fn parens(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        if !src.starts_with('(') {return None}
        src = &src[1..];
        start += 1;
        process(ignored, &mut src, &mut start, &mut errs);
        let mut ast = process(|src, start| expr(2, src, start), &mut src, &mut start, &mut errs).map_or_else(|| Box::new(NullAST::new(start.into())) as _, |x| x.0);
        process(ignored, &mut src, &mut start, &mut errs);
        if src.starts_with(',') {
            let mut vals = vec![ast];
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            loop {
                if src.starts_with(')') {break}
                if src.is_empty() {
                    errs.push(CobaltError::UnmatchedDelimiter {
                        expected: ')',
                        found: ParserFound::Eof,
                        start: (begin, 1).into(),
                        end: start.into()
                    });
                    break
                }
                vals.push(process(|src, start| expr(2, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                    errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                    Box::new(NullAST::new(start.into())) as _
                }, |x| x.0));
                process(ignored, &mut src, &mut start, &mut errs);
                if !src.starts_with([',', ')']) {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: r#""," between tuple elements"#,
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    let idx = src.find([',', ')']).unwrap_or(src.len());
                    src = &src[idx..];
                    start += idx;
                }
                if src.starts_with(',') {
                    src = &src[1..];
                    start += 1;
                }
                process(ignored, &mut src, &mut start, &mut errs);
            }
            ast = Box::new(TupleLiteralAST::new(vals));
        }
        if src.starts_with(')') {
            src = &src[1..];
            start += 1;
        }
        else {
            let got = got(src);
            errs.push(CobaltError::UnmatchedDelimiter {
                expected: ')',
                found: got.0,
                start: (begin, 1).into(),
                end: (start, got.1).into()
            });
        }
        Some((Box::new(ParenAST::new((begin..start).into(), ast)), (begin..start).into(), src, errs))
    }
    fn blocks(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        if !src.starts_with('{') {return None}
        src = &src[1..];
        start += 1;
        process(ignored, &mut src, &mut start, &mut errs);
        let mut stmts = vec![];
        loop {
            let val = process(stmt, &mut src, &mut start, &mut errs).map_or_else(|| Box::new(NullAST::new(start.into())) as _, |x| x.0);
            stmts.push(val);
            process(ignored, &mut src, &mut start, &mut errs);
            if src.as_bytes().first() != Some(&b';') {break}
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
        }
        if src.starts_with('}') {
            src = &src[1..];
            start += 1;
        }
        else {
            let got = got(src);
            errs.push(CobaltError::UnmatchedDelimiter {
                expected: '}',
                found: got.0,
                start: (begin, 1).into(),
                end: (start, got.1).into()
            });
        }
        Some((Box::new(BlockAST::new((begin..start).into(), stmts)), (begin..start).into(), src, errs))
    }
    fn arrays(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        src.starts_with('[').then_some(())?;
        let begin = start;
        let mut vals = vec![];
        let mut errs = vec![];
        src = &src[1..];
        start += 1;
        process(ignored, &mut src, &mut start, &mut errs);
        let end;
        loop {
            if src.starts_with(']') {
                end = start;
                src = &src[1..];
                start += 1;
                break
            }
            if src.is_empty() {
                end = start;
                errs.push(CobaltError::UnmatchedDelimiter {
                    expected: ']',
                    found: ParserFound::Eof,
                    start: (begin, 1).into(),
                    end: start.into()
                });
                break
            }
            vals.push(process(|src, start| expr(2, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0));
            process(ignored, &mut src, &mut start, &mut errs);
            if !src.starts_with([',', ']']) {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#""," between array elements"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
                let idx = src.find([',', ']']).unwrap_or(src.len());
                src = &src[idx..];
                start += idx;
            }
            if src.starts_with(',') {
                src = &src[1..];
                start += 1;
            }
            process(ignored, &mut src, &mut start, &mut errs);
        }
        Some((Box::new(ArrayLiteralAST::new((begin, 1).into(), (end, 1).into(), vals)), (begin..start).into(), src, errs))
    }
    None // the None is unneccessary, but it makes the code prettier
        .or_else(|| parens(src, start))
        .or_else(|| blocks(src, start))
        .or_else(|| strlit(src, start))
        .or_else(|| chrlit(src, start))
        .or_else(|| intrin(src, start))
        .or_else(|| arrays(src, start))
        .or_else(||    num(src, start))
        .or_else(|| varget(src, start))
    // the order for these isn't hugely important, but it should (in theory) put the slower calls later
    // TODO: trie-based lookup to eliminate inaccessible paths
}
/// Parse an expression
/// The mode specifies how much can be parsed:
/// - 0 starts at `log_or`
/// - 1 starts at `assigns`
/// - 2 starts at `compound`
/// - anything higher is truncated
fn expr(mode: u8, src: &str, start: usize) -> ParserReturn<Box<dyn AST>> {
    fn postfix(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let ast = process(atom, &mut src, &mut start, &mut errs)?.0;
        enum PostfixType<'a> {
            Operator(String, usize),
            Subscript(Box<dyn AST>, usize),
            Call(Vec<Box<dyn AST>>, SourceSpan),
            Attribute(&'a str, usize)
        }
        let mut ops = vec![];
        loop {
            process(ignored, &mut src, &mut start, &mut errs);
            match src.as_bytes().first().copied() {
                Some(b'.') => {
                    src = &src[1..];
                    start += 1;
                    process(ignored, &mut src, &mut start, &mut errs);
                    let loc = start;
                    let name = process(|src, start| ident(false, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                        let g = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: "an identifier",
                            found: g.0,
                            loc: (start, g.1).into()
                        });
                        ""
                    }, |x| x.0);
                    ops.push(PostfixType::Attribute(name, loc));
                    continue
                },
                Some(b'(') => {
                    let open = start;
                    src = &src[1..];
                    start += 1;
                    let mut args = vec![];
                    process(ignored, &mut src, &mut start, &mut errs);
                    let cparen;
                    loop {
                        if src.starts_with(')') {
                            cparen = (start, 1).into();
                            src = &src[1..];
                            start += 1;
                            break
                        }
                        if src.is_empty() {
                            cparen = start.into();
                            errs.push(CobaltError::UnmatchedDelimiter {
                                expected: ')',
                                found: ParserFound::Eof,
                                start: (open, 1).into(),
                                end: start.into()
                            });
                            break
                        }
                        args.push(process(|src, start| expr(2, src, start), &mut src, &mut start, &mut errs).map_or_else(|| {
                            errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                            Box::new(NullAST::new(start.into())) as _
                        }, |x| x.0));
                        process(ignored, &mut src, &mut start, &mut errs);
                        if !src.starts_with([',', ')']) {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: r#""," between function arguments"#,
                                found: got.0,
                                loc: (start, got.1).into()
                            });
                            let idx = src.find([',', ')']).unwrap_or(src.len());
                            src = &src[idx..];
                            start += idx;
                        }
                        if src.starts_with(',') {
                            src = &src[1..];
                            start += 1;
                        }
                        process(ignored, &mut src, &mut start, &mut errs);
                    }
                    ops.push(PostfixType::Call(args, cparen));
                    continue
                },
                Some(b'[') => {
                    let open = start;
                    src = &src[1..];
                    start += 1;
                    process(ignored, &mut src, &mut start, &mut errs);
                    let sub = process(|src, start| expr(2, src, start), &mut src, &mut start, &mut errs).map_or_else(|| Box::new(NullAST::new(start.into())) as _, |x| x.0);
                    process(ignored, &mut src, &mut start, &mut errs);
                    if src.starts_with(']') {
                        src = &src[1..];
                        start += 1;
                    }
                    else {
                        let got = got(src);
                        errs.push(CobaltError::UnmatchedDelimiter {
                            expected: ']',
                            found: got.0,
                            start: (open, 1).into(),
                            end: (start, got.1).into()
                        });
                    }
                    ops.push(PostfixType::Subscript(sub, start));
                    continue
                },
                Some(b'c') if src.starts_with("const") => {
                    let src_ = src;
                    let start_ = start;
                    src = &src[5..];
                    start += 5;
                    process(ignored, &mut src, &mut start, &mut errs);
                    match src.as_bytes().first().copied() {
                        Some(c @ (b'&' | b'*')) => {
                            ops.push(PostfixType::Operator((match c {
                                b'&' => "const&",
                                b'*' => "const*",
                                _ => unreachable!()
                            }).to_string(), start));
                            src = &src[1..];
                            start += 1;
                        },
                        _ => {
                            src = src_;
                            start = start_;
                            break
                        }
                    }
                    continue
                },
                Some(b'm') if src.starts_with("mut") => {
                    let src_ = src;
                    let start_ = start;
                    src = &src[3..];
                    start += 3;
                    process(ignored, &mut src, &mut start, &mut errs);
                    match src.as_bytes().first().copied() {
                        Some(c @ (b'&' | b'*')) => {
                            ops.push(PostfixType::Operator((match c {
                                b'&' => "mut&",
                                b'*' => "mut*",
                                _ => unreachable!()
                            }).to_string(), start));
                            src = &src[1..];
                            start += 1;
                        },
                        _ => {
                            src = src_;
                            start = start_;
                            break
                        }
                    }
                    continue
                },
                _ => {}
            }
            if src.as_bytes().get(1).map_or(false, |c| !b".;,)+-*/%:&|^<>=?!".contains(c)) {break}
            match src.as_bytes().first().copied() {
                Some(c @ (b'!' | b'?')) => {
                    ops.push(PostfixType::Operator((match c {
                        b'!' => "!",
                        b'?' => "?",
                        _ => unreachable!()
                    }).to_string(), start));
                    src = &src[1..];
                    start += 1;
                },
                Some(c @ (b'&' | b'*')) => {
                    ops.push(PostfixType::Operator((match c {
                        b'&' => "const&",
                        b'*' => "const*",
                        _ => unreachable!()
                    }).to_string(), start));
                    src = &src[1..];
                    start += 1;
                },
                _ => break
            }
        }
        let ast = ops.into_iter().fold(ast, |ast, op| match op {
            PostfixType::Operator(op, loc) => Box::new(PostfixAST::new((loc, op.len()).into(), op, ast)) as _,
            PostfixType::Subscript(sub, loc) => Box::new(SubAST::new(merge_spans(ast.loc(), loc.into()), ast, sub)) as _,
            PostfixType::Call(args, cparen) => Box::new(CallAST::new(cparen, ast, args)) as _,
            PostfixType::Attribute(name, loc) => Box::new(DotAST::new(ast, (name.to_string(), (loc, name.len()).into())))
        });
        Some((ast, (begin..start).into(), src, errs))
    }  
    fn prefix(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut ops = vec![];
        let mut errs = vec![];
        loop {
            if src.starts_with("++") || src.starts_with("--") {
                ops.push((start, src[..2].to_string()));
                src = &src[2..];
                start += 2;
            }
            else if src.starts_with(['+', '-', '~', '*', '&', '!']) {
                ops.push((start, src[..1].to_string()));
                src = &src[1..];
                start += 1;
            }
            else {break}
            process(ignored, &mut src, &mut start, &mut errs);
        }
        let ast = process(postfix, &mut src, &mut start, &mut errs)?.0;
        let ast = ops.into_iter().rfold(ast, |ast, (loc, op)| Box::new(PrefixAST::new((loc, op.len()).into(), op, ast)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    // TODO: Macros would be more DRY, maybe switch to them?
    fn mul_div(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(prefix, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with(['*', '/', '%']) {
            let loc = start;
            let op = src[..1].to_string();
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(prefix, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, op, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, op, rhs)| Box::new(BinOpAST::new((loc, 1).into(), op, lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn add_sub(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(mul_div, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with(['+', '-']) {
            let loc = start;
            let add = src.starts_with('+');
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(mul_div, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, add, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, add, rhs)| Box::new(BinOpAST::new((loc, 1).into(), if add {"+"} else {"-"}.to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn shift(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(add_sub, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with("<<") || src.starts_with(">>") {
            let loc = start;
            let ls = src.starts_with("<<");
            src = &src[2..];
            start += 2;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(add_sub, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, ls, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, ls, rhs)| Box::new(BinOpAST::new((loc, 2).into(), if ls {"<<"} else {">>"}.to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn cmp(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(shift, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with('>') || src.starts_with('<') {
            let loc = start;
            let gt = src.starts_with('>');
            src = &src[1..];
            start += 1;
            let eq = src.starts_with('=');
            if eq {
                src = &src[1..];
                start += 1;
            }
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(shift, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, gt, eq, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, gt, eq, rhs)| Box::new(BinOpAST::new((loc, 1 + eq as usize).into(), format!("{}{}", if gt {">"} else {"<"}, if eq {"="} else {""}), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn eq(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(cmp, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with("==") || src.starts_with("!=") {
            let loc = start;
            let inv = src.starts_with('!');
            src = &src[2..];
            start += 2;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(cmp, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, inv, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, inv, rhs)| Box::new(BinOpAST::new((loc, 2).into(), if inv {"!="} else {"=="}.to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn bit_and(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(eq, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with('&') {
            let loc = start;
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(eq, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, rhs)| Box::new(BinOpAST::new((loc, 1).into(), "&".to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn bit_xor(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(bit_and, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with('^') {
            let loc = start;
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(bit_and, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, rhs)| Box::new(BinOpAST::new((loc, 1).into(), "^".to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn bit_or(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(bit_xor, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with('|') {
            let loc = start;
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(bit_xor, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, rhs)| Box::new(BinOpAST::new((loc, 1).into(), "|".to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn casts(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(bit_or, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with(':') {
            if src.as_bytes()[1] == b':' {break}
            let loc = start;
            let bit = src.as_bytes().get(1) == Some(&b'?');
            src = &src[(1 + bit as usize)..];
            start += 1 + bit as usize;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(bit_or, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, bit, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, bit, rhs)|
            if bit {Box::new(BitCastAST::new((loc, 2).into(), lhs, rhs)) as _}
            else {Box::new(CastAST::new((loc, 1).into(), lhs, rhs)) as _}
        );
        Some((ast, (begin..start).into(), src, errs))
    }
    fn log_and(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(casts, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with("&?") {
            let loc = start;
            src = &src[2..];
            start += 2;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(casts, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, rhs)| Box::new(BinOpAST::new((loc, 2).into(), "&?".to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn log_or(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let first = process(log_and, &mut src, &mut start, &mut errs)?.0;
        let mut rest = vec![];
        process(ignored, &mut src, &mut start, &mut errs);
        while src.starts_with("|?") {
            let loc = start;
            src = &src[2..];
            start += 2;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(log_and, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            rest.push((loc, rhs));
        }
        let ast = rest.into_iter().fold(first, |lhs, (loc, rhs)| Box::new(BinOpAST::new((loc, 2).into(), "|?".to_string(), lhs, rhs)) as _);
        Some((ast, (begin..start).into(), src, errs))
    }
    fn assigns(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        // rtl associativity is pain
        // it's not as simple as just going through backwards, because we don't know the end
        // no, we have to take the AST node, along with optional operator data, with the invariant
        // that only the last element in `vals` has None in the second value, and that all others
        // have operator data
        let begin = start;
        let mut errs = vec![];
        let mut vals = vec![(process(log_or, &mut src, &mut start, &mut errs)?.0, None)];
        process(ignored, &mut src, &mut start, &mut errs);
        // match one of "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="
        while let Some(len) = (src.starts_with('=')).then_some(1).or_else(|| (src.as_bytes().get(1) == Some(&b'=')).then(||
            src.starts_with(['+', '-', '*', '/', '%', '&', '|', '^']).then_some(2).or_else(||
            (src.starts_with("<<") || src.starts_with(">>")).then_some(3))
        ).flatten()) {
            let loc = start;
            let op = src[..len].to_string();
            vals.last_mut().unwrap().1 = Some(((loc, len), op));
            src = &src[len..];
            start += len;
            process(ignored, &mut src, &mut start, &mut errs);
            let rhs = process(log_and, &mut src, &mut start, &mut errs).map_or_else(|| {
                errs.push(CobaltError::ExpectedExpr {loc: start.into()});
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            vals.push((rhs, None));
        }
        let last = vals.pop().unwrap().0;
        let ast = vals.into_iter().map(|(a, d)| (a, d.unwrap())).rfold(last, |rhs, (lhs, (loc, op))| Box::new(BinOpAST::new(loc.into(), op, lhs, rhs)));
        Some((ast, (begin..start).into(), src, errs))
    }
    fn compound(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        let mut ast = process(assigns, &mut src, &mut start, &mut errs)?.0;
        process(ignored, &mut src, &mut start, &mut errs);
        if src.starts_with(';') {
            let mut asts = vec![ast];
            while src.starts_with(';') {
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                asts.push(process(assigns, &mut src, &mut start, &mut errs).map_or_else(|| Box::new(NullAST::new(start.into())) as _, |x| x.0));
                process(ignored, &mut src, &mut start, &mut errs);
            }
            ast = Box::new(GroupAST::new(asts));
        }
        Some((ast, (begin..start).into(), src, errs))
    }
    fn cflow(mut next: impl FnMut(&str, usize) -> ParserReturn<Box<dyn AST>> + Copy, mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
        let begin = start;
        let mut errs = vec![];
        if process(|src, start| start_match("if", src, start), &mut src, &mut start, &mut errs).is_some() {
            process(ignored, &mut src, &mut start, &mut errs);
            let cond = match src.as_bytes().first() {
                Some(b'(') | Some(b'{') => process(atom, &mut src, &mut start, &mut errs).unwrap().0,
                _ => {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: r#""(" or "{" for `if` condition"#,
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    Box::new(NullAST::new(start.into())) as _
                }
            };
            process(ignored, &mut src, &mut start, &mut errs);
            let if_true = process(next, &mut src, &mut start, &mut errs).map_or_else(|| {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#"`if` body"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            let if_false = process(|src, start| start_match("else", src, start), &mut src, &mut start, &mut errs).map(|_| {
                process(next, &mut src, &mut start, &mut errs).map_or_else(|| {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: r#"`else` body"#,
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    Box::new(NullAST::new(start.into())) as _
                }, |x| x.0)
            });
            let ast = Box::new(IfAST::new((begin..start).into(), cond, if_true, if_false));
            Some((ast, (begin..start).into(), src, errs))
        }
        else if process(|src, start| start_match("while", src, start), &mut src, &mut start, &mut errs).is_some() {
            process(ignored, &mut src, &mut start, &mut errs);
            let cond = match src.as_bytes().first() {
                Some(b'(') | Some(b'{') => process(atom, &mut src, &mut start, &mut errs).unwrap().0,
                _ => {
                    let got = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: r#""(" or "{" for `while` condition"#,
                        found: got.0,
                        loc: (start, got.1).into()
                    });
                    Box::new(NullAST::new(start.into())) as _
                }
            };
            process(ignored, &mut src, &mut start, &mut errs);
            let body = process(next, &mut src, &mut start, &mut errs).map_or_else(|| {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#"`while` body"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
                Box::new(NullAST::new(start.into())) as _
            }, |x| x.0);
            let ast = Box::new(WhileAST::new((begin..start).into(), cond, body));
            Some((ast, (begin..start).into(), src, errs))
        }
        else {next(src, start)}
    }
    cflow([log_or, assigns, compound][std::cmp::min(mode, 2) as usize], src, start)
}
/// Parse a CompoundDottedNameSegment (CDNS)
/// A CDNS can be:
/// - an identifier
///   - `import x`;
/// - a glob (\*)
///   - `import *`;
/// - a brace-delimited, comma-separated list of CDNS-lists
///   - `import {a, b}`;
fn cdns(mut src: &str, mut start: usize) -> ParserReturn<CompoundDottedNameSegment> {
    use CompoundDottedNameSegment::*;
    if let Some((found, span, rem, errs)) = ident(false, src, start) {return Some((Identifier(found.to_string(), span), span, rem, errs))}
    match *src.as_bytes().first()? {
        b'*' => Some((Glob((start, 1).into()), (start, 1).into(), &src[1..], vec![])),
        b'{' => {
            let begin = start;
            let mut errs = vec![];
            let mut pat = vec![];
            src = &src[1..];
            start += 1;
            process(ignored, &mut src, &mut start, &mut errs);
            if let Some(rem) = src.strip_prefix('}') {return Some((Group(vec![]), (begin..(start + 1)).into(), rem, errs))}
            {
                let mut name = vec![process(cdns, &mut src, &mut start, &mut errs)?.0];
                process(ignored, &mut src, &mut start, &mut errs);
                loop {
                    if !src.starts_with('.') {break}
                    src = &src[1..];
                    start += 1;
                    process(ignored, &mut src, &mut start, &mut errs);
                    if let Some((seg, _)) = process(cdns, &mut src, &mut start, &mut errs) {name.push(seg)}
                    process(ignored, &mut src, &mut start, &mut errs);
                    if src.starts_with(['.', '}', ',']) {continue}
                    // simple error recovery: look for a '.' (the next element) or ';' (the end)
                    let idx = src.find(['.', '}', ',']).unwrap_or(src.len());
                    let g = got(src);
                    errs.push(CobaltError::ExpectedFound {
                        ex: r#""." between name elements"#,
                        found: g.0,
                        loc: (start, g.1).into()
                    });
                    start += idx;
                    src = &src[idx..];
                    process(ignored, &mut src, &mut start, &mut errs);
                }
                pat.push(name);
            }
            loop {
                if !src.starts_with(',') {break}
                src = &src[1..];
                start += 1;
                process(ignored, &mut src, &mut start, &mut errs);
                {
                    let mut name = vec![process(cdns, &mut src, &mut start, &mut errs)?.0];
                    process(ignored, &mut src, &mut start, &mut errs);
                    loop {
                        if !src.starts_with('.') {break}
                        src = &src[1..];
                        start += 1;
                        process(ignored, &mut src, &mut start, &mut errs);
                        if let Some((seg, _)) = process(cdns, &mut src, &mut start, &mut errs) {name.push(seg)}
                        process(ignored, &mut src, &mut start, &mut errs);
                        if src.starts_with(['.', '}', ',']) {continue}
                        // simple error recovery: look for a '.' (the next element) or ';' (the end)
                        let idx = src.find(['.', '}', ',']).unwrap_or(src.len());
                        let g = got(src);
                        errs.push(CobaltError::ExpectedFound {
                            ex: r#""." between name elements"#,
                            found: g.0,
                            loc: (start, g.1).into()
                        });
                        start += idx;
                        src = &src[idx..];
                        process(ignored, &mut src, &mut start, &mut errs);
                    }
                    pat.push(name);
                }
                process(ignored, &mut src, &mut start, &mut errs);
                if src.starts_with(['}', ',']) {continue}
                // simple error recovery: look for a ',' (the next element) or '}' (the end)
                let idx = src.find(['}', ',']).unwrap_or(src.len());
                let g = got(src);
                start += idx;
                src = &src[idx..];
                errs.push(CobaltError::ExpectedFound {
                    ex: r#""," between import alternatives"#,
                    found: g.0,
                    loc: (start, g.1).into()
                });
                process(ignored, &mut src, &mut start, &mut errs);
            }
            if src.starts_with('}') {
                src = &src[1..];
                start += 1;
            }
            else {
                let g = got(src);
                errs.push(CobaltError::UnmatchedDelimiter {
                    expected: '}',
                    found: g.0,
                    start: (begin, 1).into(),
                    end: (start, g.1).into()
                });
            }
            Some((Group(pat), (begin..start).into(), src, errs))
        },
        _ => None
    }
}
/// Parse a CompoundDottedName (CDN)
/// A CDN is a CDNS-list with an optional leading period to specify an import from absolute scope
/// A CDNS-list is a period-separated sequence of CDNSs
fn cdn(mut src: &str, mut start: usize) -> ParserReturn<CompoundDottedName> {
    let old = start;
    let global = if src.starts_with('.') {src = &src[1..]; start += 1; true} else {false};
    let mut errs = vec![];
    process(ignored, &mut src, &mut start, &mut errs);
    let mut name = vec![process(cdns, &mut src, &mut start, &mut errs)?.0];
    process(ignored, &mut src, &mut start, &mut errs);
    loop {
        if !src.starts_with('.') {break}
        src = &src[1..];
        start += 1;
        process(ignored, &mut src, &mut start, &mut errs);
        if let Some((seg, _)) = process(cdns, &mut src, &mut start, &mut errs) {name.push(seg)}
        process(ignored, &mut src, &mut start, &mut errs);
        if src.starts_with(['.', ';']) {continue}
        // simple error recovery: look for a '.' (the next element) or ';' (the end)
        let idx = src.find(['.', ';']);
        let g = got(src);
        if let Some(mut idx) = idx {
            if src.as_bytes()[idx] == b';' {
                idx -= 1;
                while !src.is_char_boundary(idx) {idx -= 1} // src.floor_char_boundary isn't stable
            }
            start += idx;
            src = &src[idx..];
        }
        else {
            start += src.len();
            src = "";
        }
        errs.push(CobaltError::ExpectedFound {
            ex: r#""." between name elements"#,
            found: g.0,
            loc: (start, g.1).into()
        });
        process(ignored, &mut src, &mut start, &mut errs);
    }
    Some((CompoundDottedName::new(name, global), (old..start).into(), src, errs))
}
/// Parse an import statement
/// An import has the form `import <CDN>`
fn import<'a>(anns: &[(&'a str, Option<&'a str>, SourceSpan)], src: &'a str, mut start: usize) -> ParserReturn<'a, Box<dyn AST>> {
    let old = start;
    start_match("import", src, start).and_then(|(_, start_span, mut src, mut errs)| {
        start += start_span.len();
        let name = process(cdn, &mut src, &mut start, &mut errs)?.0;
        let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
        Some((Box::new(ImportAST::new((old, 6).into(), name, anns)) as Box<dyn AST>, (old..start).into(), src, errs))
    })
}
/// Parse a single top-level item
/// That could be:
/// - a global declaration
/// - a module
/// - an import
fn top_level(mut src: &str, mut start: usize) -> ParserReturn<Box<dyn AST>> {
    let old = start;
    let mut errs = vec![];
    let anns: Vec<_> = std::iter::from_fn(|| process(annotation, &mut src, &mut start, &mut errs)).map(|x| x.0).collect();
    match *src.as_bytes().first()? {
        b'i' => {
            let out = process(|src, start| import(&anns, src, start), &mut src, &mut start, &mut errs)?.0;
            process(ignored, &mut src, &mut start, &mut errs);
            if src.starts_with(';') {
                src = &src[1..];
                start += 1;
            }
            else {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#"";" after declaration"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
            }
            Some((out, (old..start).into(), src, errs))
        },
        b'm' if src.starts_with("module") => {
            let begin = start;
            let (_, start_span, mut src, mut errs) = start_match("module", src, start)?;
            start += start_span.len();
            let name = process(global_id, &mut src, &mut start, &mut errs).map_or_else(|| {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#"module name"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
                DottedName::local((String::new(), unreachable_span()))
            }, |x| x.0);
            process(ignored, &mut src, &mut start, &mut errs);
            loop {
                match src.as_bytes().first() {
                    Some(b'=') => {
                        src = &src[1..];
                        start += 1;
                        process(ignored, &mut src, &mut start, &mut errs);
                        let import = process(cdn, &mut src, &mut start, &mut errs).unwrap().0;
                        let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
                        break Some((Box::new(ModuleAST::new((begin, 6).into(), name, vec![Box::new(ImportAST::new(start_span, import, vec![]))], anns)) as Box<dyn AST>, (old..start).into(), src, errs))
                    },
                    Some(b'{') => {
                        src = &src[1..];
                        start += 1;
                        let asts = process(top_levels, &mut src, &mut start, &mut errs).unwrap().0;
                        if src.as_bytes().first() == Some(&b'}') {
                            src = &src[1..];
                            start += 1;
                        }
                        else {
                            let got = got(src);
                            errs.push(CobaltError::ExpectedFound {
                                ex: r#""}" after module body"#,
                                found: got.0,
                                loc: (start, got.1).into()
                            })
                        }
                        let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
                        break Some((Box::new(ModuleAST::new((begin, 6).into(), name, asts, anns)), (old..start).into(), src, errs))
                    },
                    Some(b';') => {
                        src = &src[1..];
                        start += 1;
                        let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
                        break Some((Box::new(ModuleAST::new((begin, 6).into(), name, vec![], anns)), (old..start).into(), src, errs))
                    },
                    _ => {
                        let got = got(&src[1..]);
                        errs.push(CobaltError::ExpectedFound {
                            ex: r#""=", "{", or ";" after module name"#,
                            found: got.0,
                            loc: (start, got.1).into()
                        });
                        let idx = src.find([';', '=', '{']).unwrap_or(src.len());
                        src = &src[idx..];
                        start += idx;
                        if src.is_empty() {
                            let anns = anns.iter().copied().map(|(ann, arg, loc)| (ann.to_string(), arg.map(ToString::to_string), loc)).collect();
                            break Some((Box::new(ModuleAST::new((begin, 6).into(), name, vec![], anns)), (old..start).into(), src, errs))
                        }
                    }
                }
            }
        },
        _ => {
            let out = process(|src, start| declarations(DeclLoc::Global, Some(anns), src, start), &mut src, &mut start, &mut errs)?.0;
            process(ignored, &mut src, &mut start, &mut errs);
            if src.starts_with(';') {
                src = &src[1..];
                start += 1;
            }
            else {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: r#"";" after declaration"#,
                    found: got.0,
                    loc: (start, got.1).into()
                });
            }
            Some((out, (old..start).into(), src, errs))
        }
    }
}
/// Parse a Vec of top-level items
fn top_levels(mut src: &str, mut start: usize) -> ParserReturn<Vec<Box<dyn AST>>> {
    let mut errs = vec![];
    let old = start;
    Some((std::iter::from_fn(|| {
        process(ignored, &mut src, &mut start, &mut errs);
        if src.starts_with(';') {
            src = &src[1..];
            start += 1;
            return Some(None)
        }
        let ast = process(top_level, &mut src, &mut start, &mut errs)?.0;
        Some(Some(ast))
    }).flatten().collect(), (old..start).into(), src, errs))
}
/// Top-level parser entry point
/// Delegates to `top_levels`, recovering if unexpected characters are found
pub fn parse_tl(mut src: &str) -> (TopLevelAST, Vec<CobaltError>) {
    let mut asts = vec![];
    let mut errs = vec![];
    let mut start = 0;
    loop {
        let (mut asts_, _) = process(top_levels, &mut src, &mut start, &mut errs).unwrap();
        if asts_.is_empty() {break}
        else {
            if !asts.is_empty() {
                let got = got(src);
                errs.push(CobaltError::ExpectedFound {
                    ex: "top-level declaration",
                    found: got.0,
                    loc: (start, got.1).into()
                });
            }
            asts.append(&mut asts_);
        }
    }
    (TopLevelAST::new(asts), errs)
}
/// Alternate parser entry point
/// Calls `expr`, but hides all of the parsing details
/// `mode` can be 0, 1, or 2
/// if `mode >= 1`, assignments are parsed
/// if `mode >= 2`, componud expressions (expressions separated by `;`) are parsed
/// Setting `mode` to a number greater than 2 does nothing
pub fn parse_expr(mut src: &str, mode: u8) -> (Box<dyn AST>, Vec<CobaltError>) {
    let mut start = 0;
    let mut errs = vec![];
    process(ignored, &mut src, &mut start, &mut errs);
    let ast = process(move |src, start| expr(mode, src, start), &mut src, &mut start, &mut errs).map_or_else(|| Box::new(NullAST::new(0.into())) as _, |x| x.0);
    process(ignored, &mut src, &mut start, &mut errs);
    if !src.is_empty() {
        let g = got(src);
        errs.push(CobaltError::ExpectedFound {
            ex: "end of expression",
            found: g.0,
            loc: (start, g.1).into()
        });
    }
    (ast, errs)
}
