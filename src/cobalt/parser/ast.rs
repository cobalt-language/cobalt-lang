use crate::*;
fn parse_type(toks: &[Token], terminators: &'static str, flags: &Flags) -> (ParsedType, usize, Vec<Error>) {
    let mut idx = 1;
    if toks.len() == 0 {
        return (ParsedType::Error, 0, vec![Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 291, "expected a type".to_string())]); // parse_type always has code before it
    }
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (DottedName::new(vec![], true), true),
        Identifier(s) => (DottedName::new(vec![s.clone()], false), false),
        x => return (ParsedType::Error, 2, vec![Error::new(toks[0].loc.clone(), 291, "expected a type".to_string()).note(Note::new(toks[0].loc.clone(), format!("got {:?}", x)))])
    };
    let mut errs = vec![];
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
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
            Special('&') | Special('*') | Special('^') | Special('[') => break,
            x => {
                errs.push(Error::new(toks[idx].loc.clone(), 210, format!("unexpected token {:?} in type", x)));
                if !name.global && name.ids.len() == 1 {
                    match name.ids[0].as_str() {
                        "isize" => return (ParsedType::ISize, idx + 1, errs),
                        x if x.as_bytes()[0] == 0x69 && x.as_bytes().iter().all(|&x| x >= 0x30 && x <= 0x39) => return (x[1..].parse().ok().map(ParsedType::Int).or(Some(ParsedType::Error)).unwrap(), idx + 1, errs),
                        "usize" => return (ParsedType::USize, idx + 1, errs),
                        x if x.as_bytes()[0] == 0x75 && x.as_bytes().iter().all(|&x| x >= 0x30 && x <= 0x39) => return (x[1..].parse().ok().map(ParsedType::UInt).or(Some(ParsedType::Error)).unwrap(), idx + 1, errs),
                        "f16" => return (ParsedType::F16, idx + 1, errs),
                        "f32" => return (ParsedType::F32, idx + 1, errs),
                        "f64" => return (ParsedType::F64, idx + 1, errs),
                        "f128" => return (ParsedType::F128, idx + 1, errs),
                        "null" => return (ParsedType::Null, idx + 1, errs),
                        _ => {}
                    }
                }
                return (ParsedType::Other(name), idx + 1, errs);
            }
        }
    } 
    let mut out = if !name.global && name.ids.len() == 1 {
        match name.ids[0].as_str() {
            "isize" => ParsedType::ISize,
            x if x.as_bytes()[0] == 0x69 && x.as_bytes().iter().all(|&x| x >= 0x30 && x <= 0x39) => {
                let val = x[1..].parse();
                match val {
                    Ok(x) => ParsedType::UInt(x),
                    Err(x) => {
                        errs.push(Error::new(toks[0].loc.clone(), 290, format!("error when parsing integral type: {}", x)));
                        return (ParsedType::Error, idx + 1, errs)
                    }
                }
            },
            "usize" => ParsedType::USize,
            x if x.as_bytes()[0] == 0x75 && x.as_bytes().iter().all(|&x| x >= 0x30 && x <= 0x39) => {
                let val = x[1..].parse();
                match val {
                    Ok(x) => ParsedType::Int(x),
                    Err(x) => {
                        errs.push(Error::new(toks[0].loc.clone(), 290, format!("error when parsing integral type: {}", x)));
                        return (ParsedType::Error, idx + 1, errs)
                    }
                }
            },
            "f16" => ParsedType::F16,
            "f32" => ParsedType::F32,
            "f64" => ParsedType::F64,
            "f128" => ParsedType::F128,
            "null" => ParsedType::Null,
            _ => ParsedType::Other(name)
        }
    }
    else {ParsedType::Other(name)};
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
            Operator(x) => match x.as_str() {
                "&" => {out = ParsedType::Reference(Box::new(out)); idx += 1;},
                "*" => {out = ParsedType::Pointer(Box::new(out)); idx += 1;},
                "^" => {out = ParsedType::Borrow(Box::new(out)); idx += 1;},
                "&&" => {out = ParsedType::Reference(Box::new(ParsedType::Reference(Box::new(out)))); idx += 1;},
                "**" => {out = ParsedType::Pointer(Box::new(ParsedType::Pointer(Box::new(out)))); idx += 1;},
                "^^" => {out = ParsedType::Borrow(Box::new(ParsedType::Borrow(Box::new(out)))); idx += 1;},
                _ => {
                    errs.push(Error::new(toks[idx].loc, 220, format!("unexpected token {:?} in type", toks[idx].data)));
                    break;
                }
            },
            Special('[') => {
                if idx + 1 == toks.len() {errs.push(Error::new(toks[idx].loc.clone(), 252, "unmatched '['".to_string()));}
                else {
                    if toks[idx + 1].data == Special(']') {
                        out = ParsedType::UnsizedArray(Box::new(out))
                    }
                    else {
                        let (ast, i, mut es) = parse_expr(&toks[(idx + 1)..], "]", flags);
                        idx += i;
                        errs.append(&mut es);
                        out = ParsedType::SizedArray(Box::new(out), ast)
                    }
                }
                idx += 1;
            },
            _ => {
                errs.push(Error::new(toks[idx].loc, 220, format!("unexpected token {:?} in type name", toks[idx].data)));
                break;
            }
        }
    }
    (out, idx + 1, errs)
}
#[allow(unreachable_code)]
fn parse_paths(toks: &[Token], is_nested: bool) -> (CompoundDottedName, usize, Vec<Error>) {
    let mut idx = 1;
    let mut errs = vec![];
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (CompoundDottedName::new(vec![], true), true),
        Identifier(str) => (CompoundDottedName::new(vec![CompoundDottedNameSegment::Identifier(str.clone())], false), false),
        x => return (CompoundDottedName::local(CompoundDottedNameSegment::Identifier(String::new())), 2, vec![Error::new(toks[0].loc.clone(), 210, format!("unexpected token {:?} in identifier", x))])
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
        Identifier(s) => (DottedName::new(vec![s.clone()], false), false),
        x => return (DottedName::local(String::new()), 2, vec![Error::new(toks[0].loc.clone(), 210, format!("unexpected token {:?} in identifier", x))])
    };
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
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
                errs.push(Error::new(toks[idx].loc.clone(), 210, format!("unexpected token {:?} in identifier", x)));
                break;
            }
        }
    }
    (name, idx + 1, errs)
}
#[allow(unused_variables)]
fn parse_expr(toks: &[Token], terminators: &'static str, flags: &Flags) -> (Box<dyn AST>, usize, Vec<Error>) {
    let mut i = 0;
    let mut errs = vec![];
    while i < toks.len() {
        match &toks[i].data {
            Special(c) if terminators.contains(*c) => break,
            Special('(') => {
                let start = toks[i].loc.clone();
                let mut depth = 1;
                i += 1;
                while i < toks.len() && depth > 0 {
                    match &toks[i].data {
                        Special('(') => depth += 1,
                        Special(')') => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                if i == toks.len() && depth > 0 {
                    errs.push(Error::new(start, 250, "unmatched '('".to_string()));
                }
            },
            Special('[') => {
                let start = toks[i].loc.clone();
                let mut depth = 1;
                i += 1;
                while i < toks.len() && depth > 0 {
                    match &toks[i].data {
                        Special('[') => depth += 1,
                        Special(']') => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                if i == toks.len() && depth > 0 {
                    errs.push(Error::new(start, 252, "unmatched '['".to_string()));
                }
            },
            Special('{') => {
                let start = toks[i].loc.clone();
                let mut depth = 1;
                i += 1;
                while i < toks.len() && depth > 0 {
                    match &toks[i].data {
                        Special('{') => depth += 1,
                        Special('}') => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                if i == toks.len() && depth > 0 {
                    errs.push(Error::new(start, 254, "unmatched '{'".to_string()));
                }
            }
            Special(')') => {errs.push(Error::new(toks[i].loc.clone(), 251, "unmatched ')'".to_string())); break;},
            Special(']') => {errs.push(Error::new(toks[i].loc.clone(), 253, "unmatched ']'".to_string())); break;},
            Special('}') => {errs.push(Error::new(toks[i].loc.clone(), 255, "unmatched '}'".to_string())); break;},
            _ => i += 1
        }
    }
    (Box::new(NullAST::new(toks[0].loc.clone())), i + 1, errs)
}
fn parse_tl(mut toks: &[Token], flags: &Flags) -> (Vec<Box<dyn AST>>, usize, Vec<Error>) {
    let mut outs: Vec<Box<dyn AST>> = vec![];
    let mut errs = vec![];
    let mut i = 0;
    'main: while toks.len() != 0 {
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
                                    errs.push(Error::new(toks[0].loc, 244, "unmatched '{' of module body".to_string()))
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
                "fn" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], "(=;");
                    toks = &toks[idx..];
                    i = idx;
                    errs.append(&mut es);
                    if toks.len() == 0 {
                        errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 234, "expected parameters or assignment after function definition".to_string()));
                        break;
                    }
                    match &toks[0].data {
                        Special('(') => {
                            let mut params = vec![];
                            let mut defaults = None;
                            loop {
                                if toks.len() < 2 {
                                    errs.push(Error::new(toks[0].loc.clone(), 238, "unexpected end of parameter list".to_string()));
                                    break 'main;
                                }
                                if toks[1].data == Special(')') {
                                    toks = &toks[2..];
                                    i += 2;
                                    break;
                                }
                                let is_mut = if toks[1].data == Keyword("mut".to_string()) {
                                    toks = &toks[2..];
                                    i += 2;
                                    true
                                }
                                else {
                                    toks = &toks[1..];
                                    i += 1;
                                    false
                                };
                                let id_start = toks[0].loc.clone();
                                let (mut name, idx, mut es) = parse_path(toks, ":,)");
                                toks = &toks[(idx - 1)..];
                                i += idx - 1;
                                errs.append(&mut es);
                                if name.global || name.ids.len() > 1 {
                                    errs.push(Error::new(id_start, 239, "function parameters cannot be global variables".to_string()));
                                }
                                let name = name.ids.pop().unwrap_or_else(String::new);
                                let ty = if toks.len() > 0 && toks[0].data == Special(':') {
                                    let (ty, idx, mut es) = parse_type(&toks[1..], ",)=", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    ty
                                }
                                else {
                                    errs.push(Error::new(toks[0].loc.clone(), 240, "function parameters must have explicit types".to_string()));
                                    ParsedType::Error
                                };
                                let default = if toks.len() > 0 && toks[0].data == Operator("=".to_string()) {
                                    if defaults == None {defaults = Some(toks[0].loc.clone());}
                                    let (val, idx, mut es) = parse_expr(&toks[1..], ",)", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    Some(val)
                                }
                                else {
                                    if defaults.is_some() {
                                        errs.push(Error::new(toks[0].loc.clone(), 241, "all parameters after the first default parameter must be defaults".to_string()).note(Note::new(defaults.unwrap(), "first default defined here".to_string())));
                                    }
                                    None
                                };
                                params.push((name, is_mut, ty, default));
                                if toks.len() == 0 {
                                    errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 238, "unexpected end of parameter list".to_string()));
                                    break 'main;
                                }
                                match &toks[0].data {
                                    Special(')') => {
                                        toks = &toks[1..];
                                        i += 1;
                                        break;
                                    },
                                    Special(',') => {},
                                    x => errs.push(Error::new(toks[0].loc.clone(), 242, format!("expected ',' or ')' after parameter, got {x:?}")))
                                }
                            }
                            if toks.len() == 0 {
                                errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 238, "expected function return type".to_string()));
                                break;
                            }
                            match &toks[0].data {
                                Special(';') => {
                                    errs.push(Error::new(toks[0].loc.clone(), 243, "function declaration requires an explicit return type".to_string()));
                                    outs.push(Box::new(FnDefAST::new(start, name, ParsedType::Error, params, Box::new(NullAST::new(toks[0].loc.clone())))));
                                    toks = &toks[1..];
                                    i += 1;
                                    continue;
                                },
                                Special(':') => {
                                    let (ty, idx, mut es) = parse_type(&toks[1..], "=;", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    if toks.len() == 0 {
                                        let last = unsafe {(*toks.as_ptr().offset(-1)).loc.clone()};
                                        errs.push(Error::new(last.clone(), 244, "expected function body or semicolon".to_string()));
                                        outs.push(Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(last)))));
                                        break;
                                    }
                                    match &toks[0].data {
                                        Special(';') => outs.push(Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(toks[0].loc.clone()))))),
                                        Special('{') => {
                                            errs.push(Error::new(toks[0].loc.clone(), 245, "functions are defined with an '='".to_string()).note(Note::new(toks[0].loc.clone(), "try inserting an '='".to_string())));
                                            let (ast, idx, mut es) = parse_expr(toks, ";", flags);
                                            toks = &toks[idx..];
                                            i += idx;
                                            errs.append(&mut es);
                                            outs.push(Box::new(FnDefAST::new(start, name, ty, params, ast)));
                                        },
                                        Operator(x) if x == "=" => {
                                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                            toks = &toks[(idx + 1)..];
                                            i += idx + 1;
                                            errs.append(&mut es);
                                            outs.push(Box::new(FnDefAST::new(start, name, ty, params, ast)));
                                        },
                                        x => errs.push(Error::new(toks[0].loc.clone(), 244, format!("expected function body or semicolon, got {x:?}")))
                                    }
                                },
                                Operator(x) if x == "=" => {
                                    let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                    toks = &toks[(idx + 1)..];
                                    i += idx + 1;
                                    errs.append(&mut es);
                                    outs.push(Box::new(FnDefAST::new(start, name, ParsedType::Error, params, ast)));
                                },
                                x => {}
                            }
                        },
                        Special(';') => errs.push(Error::new(toks[0].loc.clone(), 235, "function declaration must have parameters and return type".to_string())),
                        Operator(x) if x == "=" => errs.push(Error::new(toks[0].loc.clone(), 237, "functions cannot be assigned".to_string())),
                        _ => errs.push(Error::new(toks[0].loc.clone(), 236, format!("expected function parameters, got {:?}", toks[0].data)))
                    }
                },
                "cr" => {},
                "let" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], ":=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    if toks.len() == 0 {
                        errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, "expected type specification or value after variable definition".to_string()));
                        break;
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let cast_loc = toks[0].loc.clone();
                            let (t, idx, mut es) = parse_type(&toks[1..], "=;", flags);
                            toks = &toks[(idx + 1)..];
                            i += idx + 1;
                            errs.append(&mut es);
                            if toks.len() == 0 {
                                errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 232, "expected value after typed variable definition".to_string()));
                                break;
                            }
                            let ast = if toks[0].data == Special(':') {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[(idx + 1)..];
                                i += idx + 1;
                                errs.append(&mut es);
                                if toks.len() == 0 {
                                    errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, "expected semicolon after variable definition".to_string()));
                                    break;
                                }
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            outs.push(Box::new(VarDefAST::new(start, name, Box::new(CastAST::new(cast_loc, ast, t)))));
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.len() == 0 {
                                errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, "expected semicolon after variable definition".to_string()));
                                break;
                            }
                            outs.push(Box::new(VarDefAST::new(start, name, ast)));
                        },
                        Special(';') => errs.push(Error::new(toks[0].loc.clone(), 233, "variable definition must have a type specification and/or value".to_string())),
                        _ => errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, "expected type specification or value after variable definition".to_string()).note(Note::new(toks[0].loc, format!("got {:?}", toks[0].data))))
                    }
                },
                "mut" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], ":=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    if toks.len() == 0 {
                        errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, "expected type specification or value after variable definition".to_string()));
                        break;
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let cast_loc = toks[0].loc.clone();
                            let (t, idx, mut es) = parse_type(&toks[1..], "=;", flags);
                            toks = &toks[(idx + 1)..];
                            i += idx + 1;
                            errs.append(&mut es);
                            if toks.len() == 0 {
                                errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 232, "expected value after typed variable definition".to_string()));
                                break;
                            }
                            let ast = if toks[0].data == Special(':') {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[(idx + 1)..];
                                i += idx + 1;
                                errs.append(&mut es);
                                if toks.len() == 0 {
                                    errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, "expected semicolon after variable definition".to_string()));
                                    break;
                                }
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            outs.push(Box::new(MutDefAST::new(start, name, Box::new(CastAST::new(cast_loc, ast, t)))));
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.len() == 0 {
                                errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, "expected semicolon after variable definition".to_string()));
                                break;
                            }
                            outs.push(Box::new(MutDefAST::new(start, name, ast)));
                        },
                        Special(';') => errs.push(Error::new(toks[0].loc.clone(), 233, "variable definition must have a type specification and/or value".to_string())),
                        _ => errs.push(Error::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, "expected type specification or value after variable definition".to_string()).note(Note::new(toks[0].loc, format!("got {:?}", toks[0].data))))
                    }
                },
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
        errs.push(Error::new(toks[len - 1].loc, 255, "unmatched '}'".to_string()));
        toks = &toks[len..];
        let (mut o, l, mut e) = parse_tl(toks, flags);
        out.append(&mut o);
        len = l;
        errs.append(&mut e);
    }
    return (Box::new(TopLevelAST::new(start, out)), errs);
}
