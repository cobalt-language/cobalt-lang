use crate::*;
#[allow(unreachable_code)]
fn parse_paths(toks: &[Token], is_nested: bool) -> (CompoundDottedName, usize, Vec<Error>) {
    let mut idx = 1;
    let mut errs = vec![];
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (CompoundDottedName::new(vec![], true), true),
        Identifier(str) => (CompoundDottedName::new(vec![CompoundDottedNameSegment::Identifier(str.clone())], false), false),
        x => return (CompoundDottedName::local(CompoundDottedNameSegment::Identifier(String::new())), 2, vec![Error::new(toks[0].loc, 210, format!("unexpected token {:?} in identifier", x))])
    };
    while idx < toks.len() {
        match &toks[idx].data {
            Special(';') => break,
            Special(',') | Special('}') if is_nested => break,
            Special('.') => {
                if lwp {
                    errs.push(Error::new(toks[idx].loc, 211, "identifier cannot contain consecutive periods".to_string()).note(Note::new(toks[idx].loc, "Did you accidentally type two?".to_string())))
                }
                lwp = true;
                idx += 1;
            }
            Identifier(s) => {
                if !lwp {
                    if let Some(CompoundDottedNameSegment::Glob(ref x)) = name.ids.last() {
                        name.ids.push(CompoundDottedNameSegment::Glob(x.to_owned() + s));
                    }
                    else {
                        errs.push(Error::new(toks[idx].loc, 212, "identifier cannot contain consecutive identifiers".to_string()).note(Note::new(toks[idx].loc, "Did you forget a period?".to_string())))
                    }
                }
                name.ids.push(CompoundDottedNameSegment::Identifier(s.clone()));
                idx += 1;
            }
            Operator(ref x) if x == "*" => {
                if lwp {
                    name.ids.push(CompoundDottedNameSegment::Glob('*'.to_string()));
                }
                else {
                    match name.ids.pop() {
                        Some(CompoundDottedNameSegment::Identifier(x)) |
                        Some(CompoundDottedNameSegment::Glob(x)) => name.ids.push(CompoundDottedNameSegment::Glob(x + "*")),
                        Some(CompoundDottedNameSegment::Group(_)) => errs.push(Error::new(toks[idx].loc, 212, "identifier cannot contain consecutive identifiers".to_string()).note(Note::new(toks[idx].loc, "Did you forget a period?".to_string()))),
                        None => unreachable!("if the last element was not a period, then there is at least one element in name.ids")
                    }
                }
                idx += 1;
            },
            x => {
                errs.push(Error::new(toks[idx].loc, 210, format!("unexpected token {:?} in identifier", x)));
                break;
            }
        }
    }
    (name, idx + 1, errs)
}
fn parse_path(toks: &[Token], terminators: &'static str) -> (DottedName, usize, Vec<Error>) {
    let mut idx = 1;
    let mut errs = vec![];
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (DottedName::new(vec![], true), true),
        Identifier(str) => (DottedName::new(vec![str.clone()], false), false),
        x => return (DottedName::local(String::new()), 2, vec![Error::new(toks[0].loc, 210, format!("unexpected token {:?} in identifier", x))])
    };
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Keyword(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
            Special('.') => {
                if lwp {
                    errs.push(Error::new(toks[idx].loc, 211, "identifier cannot contain consecutive periods".to_string()).note(Note::new(toks[idx].loc, "Did you accidentally type two?".to_string())))
                }
                lwp = true;
                idx += 1;
            }
            Identifier(str) => {
                if !lwp {
                    errs.push(Error::new(toks[idx].loc, 212, "identifier cannot contain consecutive identifiers".to_string()).note(Note::new(toks[idx].loc, "Did you forget a period?".to_string())))
                }
                name.ids.push(str.clone());
                idx += 1;
            }
            x => {
                errs.push(Error::new(toks[idx].loc, 210, format!("unexpected token {:?} in identifier", x)));
                break;
            }
        }
    }
    (name, idx + 1, errs)
}
fn parse_tl(mut toks: &[Token], flags: &Flags) -> (Vec<Box<dyn AST>>, usize, Vec<Error>) {
    let mut outs: Vec<Box<dyn AST>> = vec![];
    let mut errs = vec![];
    let mut i = 0;
    while toks.len() != 0 {
        let val = &toks[0];
        match val.data {
            Special(';') => {i += 1; toks = &toks[1..];},
            Special('}') => break,
            Keyword(ref x) => match x.as_str() {
                "module" => {
                    let (name, idx, mut es) = parse_path(&toks[1..], "=;{");
                    i += idx;
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.len() == 0 {
                        errs.push(Error::new(val.loc, 202, "expected module body, got EOF".to_string()));
                        break;
                    }
                    match &toks[0].data {
                        Special('{') => {
                            let (vals, idx, mut e) = parse_tl(&toks[1..], flags);
                            if idx == toks.len() {
                                if toks[idx - 1].data != Special('}') {
                                    errs.push(Error::new(toks[0].loc, 220, "unmatched opening brace of module body".to_string()))
                                }
                                else {
                                    outs.push(Box::new(ModuleAST::new(toks[0].loc, name, vals)));
                                }
                                break;
                            }
                            outs.push(Box::new(ModuleAST::new(toks[0].loc, name, vals)));
                            errs.append(&mut e);
                            toks = &toks[idx..];
                            i += idx;
                        },
                        Operator(s) if s == "=" => {
                            let (oname, idx, mut es) = parse_path(toks, ";");
                            i += idx;
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            if toks.last().map(|x| &x.data) == Some(&Special(';')) {
                                errs.push(Error::new(val.loc, 202, "expected semicolon after module assignment".to_string()));
                                break;
                            }
                            let mut cname: CompoundDottedName = oname.into();
                            cname.ids.push(CompoundDottedNameSegment::Glob('*'.to_string()));
                            outs.push(Box::new(ModuleAST::new(toks[0].loc, name, vec![Box::new(ImportAST::new(toks[0].loc, cname))])));
                        },
                        Special(';') => {
                            outs.push(Box::new(ModuleAST::new(toks[0].loc, name, vec![])));
                        },
                        x => unreachable!("unexpected value after module: {:?}", x)
                    }
                },
                "import" => {
                    let (name, idx, mut es) = parse_paths(&toks[1..], false);
                    outs.push(Box::new(ImportAST::new(toks[0].loc, name)));
                    errs.append(&mut es);
                    i += idx + 1;
                    toks = &toks[(idx + 1)..];
                },
                "fn" => {},
                "cr" => {},
                "let" => {},
                "mut" => {},
                _ => {
                    errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)));
                    i += 1;
                    toks = &toks[1..];
                }
            },
            _ => {
                errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)));
                i += 1;
                toks = &toks[1..];
            }
        }
    };
    (outs, i + 1, errs)
}
pub fn parse(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Error>) {
    if toks.len() == 0 {
        return (Box::new(TopLevelAST::new(Location::new("<empty>", 0, 0, 0), vec![])), vec![])
    }
    let start = unsafe {toks.get_unchecked(0)}.loc; // already bounds checked
    let (mut out, mut len, mut errs) = parse_tl(toks, flags);
    while len < toks.len() {
        errs.push(Error::new(toks[len - 1].loc, 220, "unmatched closing brace".to_string()));
        toks = &toks[len..];
        let (mut o, l, mut e) = parse_tl(toks, flags);
        out.append(&mut o);
        len = l;
        errs.append(&mut e);
    }
    return (Box::new(TopLevelAST::new(start, out)), vec![]);
}
