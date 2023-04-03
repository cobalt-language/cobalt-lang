#![allow(unreachable_code)]
use crate::*;
use crate::parser::ops::*;
use TokenData::*;
fn null(toks: &[Token]) -> Box<dyn AST> {Box::new(NullAST::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}))}
fn parse_paths(toks: &[Token], is_nested: bool) -> (CompoundDottedName, usize, Vec<Diagnostic>) {
    let mut idx = 1;
    let mut errs = vec![];
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (CompoundDottedName::new(vec![], true), true),
        Identifier(str) => (CompoundDottedName::new(vec![CompoundDottedNameSegment::Identifier(str.clone(), toks[0].loc.clone())], false), false),
        x => return (CompoundDottedName::local(CompoundDottedNameSegment::Identifier(String::new(), toks[0].loc.clone())), 2, vec![Diagnostic::error(toks[0].loc.clone(), 210, Some(format!("expected identifier, '{{', '*', or '.', got {x:#}")))])
    };
    'main: while idx < toks.len() {
        match &toks[idx].data {
            Special(';') => break,
            Special(',') | Special('}') if is_nested => break,
            Special('.') => {
                if lwp {errs.push(Diagnostic::error(toks[idx].loc.clone(), 211, None))}
                lwp = true;
                idx += 1;
            },
            Special('{') => {
                if !lwp {errs.push(Diagnostic::error(toks[idx].loc.clone(), 212, None))}
                let mut groups = vec![];
                lwp = false;
                idx += 1;
                loop {
                    let (sub, i, mut es) = parse_paths(&toks[idx..], true);
                    errs.append(&mut es);
                    if sub.global {errs.push(Diagnostic::error(toks[idx].loc.clone(), 215, None))}
                    groups.push(sub.ids);
                    idx += i - 1;
                    match &toks[idx].data {
                        Special(',') => idx += 1,
                        Special('}') => {
                            idx += 1;
                            break;
                        },
                        _ => {
                            errs.push(Diagnostic::error(toks[idx].loc.clone(), 216, None));
                            name.ids.push(CompoundDottedNameSegment::Group(groups));
                            break 'main;
                        }
                    }
                }
                name.ids.push(CompoundDottedNameSegment::Group(groups));
            },
            Identifier(s) => {
                if !lwp {errs.push(Diagnostic::error(toks[idx].loc.clone(), 212, None))}
                lwp = false;
                name.ids.push(CompoundDottedNameSegment::Identifier(s.clone(), toks[idx].loc.clone()));
                idx += 1;
            },
            Operator(ref x) if x == "*" => {
                if !lwp {errs.push(Diagnostic::error(toks[idx].loc.clone(), 212, None))}
                name.ids.push(CompoundDottedNameSegment::Glob(toks[idx].loc.clone()));
                lwp = false;
                idx += 1;
            },
            _ => break
        }
    }
    if lwp {errs.push(Diagnostic::error(toks[idx].loc.clone(), 214, None))}
    (name, idx + 1, errs)
}
fn parse_path(toks: &[Token], terminators: &'static str) -> (DottedName, usize, Vec<Diagnostic>) {
    let mut idx = 1;
    let mut errs = vec![];
    if toks.is_empty() {return (DottedName::local((String::new(), (0, 0..0))), 0, vec![])}
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (DottedName::new(vec![], true), true),
        Identifier(s) => (DottedName::new(vec![(s.clone(), toks[0].loc.clone())], false), false),
        x => return (DottedName::local((String::new(), toks[0].loc.clone())), 2, vec![Diagnostic::error(toks[0].loc.clone(), 210, Some(format!("expected identifier or '.', got {x:#}")))])
    };
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
            Special('.') => {
                if lwp {
                    errs.push(Diagnostic::error(toks[idx].loc.clone(), 211, None))
                }
                lwp = true;
                idx += 1;
            }
            Identifier(str) => {
                if !lwp {
                    errs.push(Diagnostic::error(toks[idx].loc.clone(), 212, None))
                }
                lwp = false;
                name.ids.push((str.clone(), toks[idx].loc.clone()));
                idx += 1;
            }
            _ => break
        }
    }
    (name, idx + 1, errs)
}
fn parse_literals(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if toks.is_empty() {return (Box::new(NullAST::new((0, 0..0))), vec![])}
    match &toks[0].data {
        Int(x) => {
            if toks.len() == 1 {return (Box::new(IntLiteralAST::new(toks[0].loc.clone(), *x, None)), vec![])}
            let mut errs = vec![];
            let suf = if let Identifier(s) = &toks[1].data {Some(s)} else {
                errs.push(Diagnostic::error(toks[1].loc.clone(), 270, Some(format!("unexpected {:#} after integer literal", toks[1].data))));
                None
            };
            errs.extend(toks.iter().skip(2).map(|tok| Diagnostic::error(tok.loc.clone(), 270, Some(format!("unexpected {:#} after integer literal", tok.data)))));
            (Box::new(IntLiteralAST::new(toks[0].loc.clone(), *x, suf.map(|suf| (suf.clone(), toks[1].loc.clone())))), errs)
        },
        Float(x) => {
            if toks.len() == 1 {return (Box::new(FloatLiteralAST::new(toks[0].loc.clone(), *x, None)), vec![])}
            let mut errs = vec![];
            let suf = if let Identifier(s) = &toks[1].data {Some(s)} else {
                errs.push(Diagnostic::error(toks[1].loc.clone(), 270, Some(format!("unexpected {:#} after floating-point literal", toks[1].data))));
                None
            };
            errs.extend(toks.iter().skip(2).map(|tok| Diagnostic::error(tok.loc.clone(), 270, Some(format!("unexpected {:#} after floating-point literal", tok.data)))));
            (Box::new(FloatLiteralAST::new(toks[0].loc.clone(), *x, suf.map(|suf| (suf.clone(), toks[1].loc.clone())))), errs)
        },
        Char(x) => {
            if toks.len() == 1 {return (Box::new(CharLiteralAST::new(toks[0].loc.clone(), *x, None)), vec![])}
            let mut errs = vec![];
            let suf = if let Identifier(s) = &toks[1].data {Some(s)} else {
                errs.push(Diagnostic::error(toks[1].loc.clone(), 270, Some(format!("unexpected {:#} after integer literal", toks[1].data))));
                None
            };
            errs.extend(toks.iter().skip(2).map(|tok| Diagnostic::error(tok.loc.clone(), 270, Some(format!("unexpected {:#} after character literal", tok.data)))));
            (Box::new(CharLiteralAST::new(toks[0].loc.clone(), *x, suf.map(|suf| (suf.clone(), toks[1].loc.clone())))), errs)
        },
        Str(x) => {
            if toks.len() == 1 {return (Box::new(StringLiteralAST::new(toks[0].loc.clone(), x.clone(), None)), vec![])}
            let mut errs = vec![];
            let suf = if let Identifier(s) = &toks[1].data {Some(s)} else {
                errs.push(Diagnostic::error(toks[1].loc.clone(), 270, Some(format!("unexpected {:#} after integer literal", toks[1].data))));
                None
            };
            errs.extend(toks.iter().skip(2).map(|tok| Diagnostic::error(tok.loc.clone(), 270, Some(format!("unexpected {:#} after string literal", tok.data)))));
            (Box::new(StringLiteralAST::new(toks[0].loc.clone(), x.clone(), suf.map(|suf| (suf.clone(), toks[1].loc.clone())))), errs)
        },
        Identifier(x) if x == "null" => (Box::new(NullAST::new(toks[0].loc.clone())), toks.iter().skip(1).map(|tok| Diagnostic::error(tok.loc.clone(), 273, Some(format!("unexpected {:#} after null", tok.data)))).collect()),
        Identifier(name) => (Box::new(VarGetAST::new(toks[0].loc.clone(), name.clone(), false)), toks.iter().skip(1).map(|tok| Diagnostic::error(tok.loc.clone(), 273, Some(format!("got {:#}", tok.data)))).collect()),
        Special('.') =>
            if let Some(Token {loc, data: Identifier(name)}) = toks.get(1) {(Box::new(VarGetAST::new(loc.clone(), name.clone(), true)), toks.iter().skip(2).map(|tok| Diagnostic::error(tok.loc.clone(), 273, Some(format!("got {:#}", tok.data)))).collect())}
            else {(Box::new(NullAST::new(toks[0].loc.clone())), toks.iter().map(|tok| Diagnostic::error(tok.loc.clone(), 273, Some(format!("got {:#}", tok.data)))).collect())}
        Macro(name, None) => (Box::new(IntrinsicAST::new(toks[0].loc.clone(), name.clone(), vec![])), toks.iter().skip(1).map(|tok| Diagnostic::error(tok.loc.clone(), 272, Some(format!("unexpected {:#} after intrinsic", tok.data)))).collect()),
        Macro(name, Some((_, atoks))) => {
            let mut atoks = atoks.as_slice();
            let mut errs = toks.iter().skip(1).map(|tok| Diagnostic::error(tok.loc.clone(), 272, Some(format!("unexpected {:#} after intrinsic", tok.data)))).collect::<Vec<_>>();
            let mut args = vec![];
            while !atoks.is_empty() {
                let (ast, i, mut es) = parse_expr(atoks, ",", flags);
                errs.append(&mut es);
                args.push(ast);
                if i > atoks.len() {break}
                atoks = &atoks[i..];
            }
            (Box::new(IntrinsicAST::new(toks[0].loc.clone(), name.clone(), args)), errs)
        },
        _ => (Box::new(NullAST::new(toks[0].loc.clone())), toks.iter().map(|tok| Diagnostic::error(tok.loc.clone(), 273, Some(format!("got {:#}", tok.data)))).collect())
    }
}
fn parse_groups(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    match toks.get(0).map(|x| &x.data) {
        Some(Special('(')) => {
            if toks.last().unwrap().data == Special(')') {toks = &toks[..(toks.len() - 1)];}
            toks = &toks[1..];
            if toks.is_empty() {return (Box::new(NullAST::new(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()})), vec![])}
            let (ast, _, errs) = parse_expr(toks, "", flags);
            (ast, errs)
        },
        Some(Special('{')) => {
            let mut start = toks[0].loc.clone();
            start.1.end = toks.last().unwrap().loc.1.end;
            if toks.last().unwrap().data == Special('}') {toks = &toks[..(toks.len() - 1)];}
            let mut errs = vec![];
            toks = &toks[1..];
            let len = toks.len();
            let mut idx = 0;
            let mut asts: Vec<Box<dyn AST>> = vec![];
            while idx <= len {
                if idx + 1 > len {
                    asts.push(Box::new(NullAST::new(toks[idx - 1].loc.clone())));
                    idx += 1;
                }
                else {
                    let (ast, i, mut es) = parse_stmts(&toks[idx..], ";", flags);
                    errs.append(&mut es);
                    asts.push(ast);
                    idx += i;
                    if idx >= 1 && matches!(toks.get(idx - 1).map(|x| &x.data), Some(Statement(_))) {idx -= 1;}
                }
            }
            (Box::new(BlockAST::new(start, asts)), errs)
        },
        Some(Special('[')) => {
            let start = toks[0].loc.clone();
            let end = toks.last().unwrap().loc.clone();
            if toks.last().unwrap().data == Special(']') {toks = &toks[..(toks.len() - 1)];}
            let mut errs = vec![];
            toks = &toks[1..];
            let len = toks.len();
            let mut idx = 0;
            let mut asts: Vec<Box<dyn AST>> = vec![];
            while idx <= len {
                let (ast, i, mut es) = parse_expr(&toks[idx..], ",", flags);
                errs.append(&mut es);
                asts.push(ast);
                idx += i;
            }
            (Box::new(ArrayLiteralAST::new(start, end, asts)), errs)
        },
        Some(_) => parse_literals(toks, flags),
        None => (null(toks), vec![])
    }
}
fn parse_calls(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    match toks.last().map(|x| &x.data) {
        Some(Special(')')) => {
            let mut depth = 1;
            let mut idx = toks.len() - 1;
            while idx > 0 && depth > 0 {
                idx -= 1;
                match &toks[idx].data {
                    Special(')') => depth += 1,
                    Special('(') => depth -= 1,
                    _ => {}
                }
            }
            if idx == 0 || depth > 0 {parse_groups(toks, flags)}
            else {
                let (target, ts) = toks.split_at(idx);
                toks = &ts[1..];
                let mut args = vec![];
                let mut errs = vec![];
                while !toks.is_empty() && toks[0].data != Special(')') {
                    let (ast, idx, mut es) = parse_expr(toks, ",)", flags);
                    errs.append(&mut es);
                    args.push(ast);
                    toks = &toks[idx..];
                }
                let (target, _, mut es) = parse_expr(target, "", flags);
                errs.append(&mut es);
                (Box::new(CallAST::new(target.loc(), unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, target, args)), errs)
            }
        },
        Some(Special(']')) => {
            let mut depth = 1;
            let mut idx = toks.len() - 1;
            while idx > 0 && depth > 0 {
                idx -= 1;
                match &toks[idx].data {
                    Special(']') => depth += 1,
                    Special('[') => depth -= 1,
                    _ => {}
                }
            }
            if idx == 0 || depth > 0 {parse_groups(toks, flags)}
            else {
                let (target, ts) = toks.split_at(idx);
                toks = &ts[1..(ts.len() - 1)];
                let (idx, _, mut errs) = if toks.is_empty() {(Box::new(NullAST::new((ts[0].loc.0, ts[0].loc.1.end..ts[0].loc.1.end))) as Box<dyn AST>, 0usize, vec![])} else {parse_expr(toks, "", flags)};
                let (target, _, mut es) = parse_expr(target, "", flags);
                errs.append(&mut es);
                (Box::new(SubAST::new((target.loc().0, target.loc().1.start..ts.last().unwrap().loc.1.end), target, idx)), errs)
            }
        },
        Some(_) => parse_groups(toks, flags),
        None => (null(toks), vec![]) // technically unreachable
    }
}
fn parse_flow(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if let Some(Token {data: Keyword(ref s), ref loc}) = toks.get(0) {
        match s.as_str() {
            "if" => {
                let mut errs = vec![];
                let loc = loc.clone();
                toks = &toks[1..];
                let cond;
                match toks.get(0).map(|x| &x.data) {
                    None => return (null(toks), vec![Diagnostic::error(loc, 261, None)]),
                    Some(Special('(')) => {
                        let mut depth = 1;
                        let mut idx = 2;
                        {
                            let mut toks = toks;
                            while 'cond: {
                                toks = &toks[1..];
                                idx += 1;
                                match toks.get(0).map(|x| &x.data) {
                                    None => break 'cond false,
                                    Some(Special('(')) => depth += 1,
                                    Some(Special(')')) => depth -= 1,
                                    _ => {}
                                }
                                depth > 0
                            } {}
                        }
                        let (c, ts) = toks.split_at(idx - 1);
                        toks = ts;
                        let (c, mut es) = parse_groups(c, flags);
                        errs.append(&mut es);
                        cond = c;
                    },
                    Some(Special('{')) => {
                        let mut depth = 1;
                        let mut idx = 2;
                        {
                            let mut toks = toks;
                            while 'cond: {
                                toks = &toks[1..];
                                idx += 1;
                                match toks.get(0).map(|x| &x.data) {
                                    None => break 'cond false,
                                    Some(Special('{')) => depth += 1,
                                    Some(Special('}')) => depth -= 1,
                                    _ => {}
                                }
                                depth > 0
                            } {}
                        }
                        let (c, ts) = toks.split_at(idx - 1);
                        toks = ts;
                        let (c, mut es) = parse_groups(c, flags);
                        errs.append(&mut es);
                        cond = c;
                    },
                    Some(x) => {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 380, Some(format!("expected '(' or '{{', got {x:#}"))));
                        cond = Box::new(NullAST::new(toks[0].loc.clone()));
                    }
                }
                let (if_true, cont) = {
                    let mut i = 0;
                    while i < toks.len() {
                        match &toks[i].data {
                            Special(';') => break,
                            Keyword(x) if x == "else" => break,
                            Statement(k) if (k != "const" && k != "mut") || !matches!(toks.get(i + 1).and_then(|x| if let Operator(ref x) = x.data {Some(x.as_str())} else {None}).unwrap_or(""), "&" | "*" | "^") => {errs.push(Diagnostic::error(toks[i].loc.clone(), 280, None)); break},
                            Special('(') => {
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
                            },
                            Special('[') => {
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
                            },
                            Special('{') => {
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
                            }
                            Special(')') => break,
                            Special(']') => break,
                            Special('}') => break,
                            _ => i += 1
                        }
                    }
                    let (ast, mut es) = parse_splits(&toks[..i], flags);
                    errs.append(&mut es);
                    let cont = toks.get(i).map(|x| &x.data) == Some(&Keyword("else".to_string()));
                    toks = &toks[i..];
                    (ast, cont)
                };
                (Box::new(IfAST::new(loc, cond, if_true, if cont {
                    let (ast, _, mut es) = parse_expr(&toks[1..], ";", flags);
                    errs.append(&mut es);
                    Some(ast)
                } else {None})), errs)
            },
            "else" => {
                let (ast, mut errs) = parse_calls(&toks[1..], flags);
                errs.insert(0, Diagnostic::error(loc.clone(), 263, None));
                (ast, errs)
            },
            "while" => {
                let mut errs = vec![];
                let loc = loc.clone();
                toks = &toks[1..];
                let cond;
                match toks.get(0).map(|x| &x.data) {
                    None => return (null(toks), vec![Diagnostic::error(loc, 262, None)]),
                    Some(Special('(')) => {
                        let mut depth = 1;
                        let mut idx = 2;
                        {
                            let mut toks = toks;
                            while 'cond: {
                                toks = &toks[1..];
                                idx += 1;
                                match toks.get(0).map(|x| &x.data) {
                                    None => break 'cond false,
                                    Some(Special('(')) => depth += 1,
                                    Some(Special(')')) => depth -= 1,
                                    _ => {}
                                }
                                depth > 0
                            } {}
                        }
                        let (c, ts) = toks.split_at(idx - 1);
                        toks = ts;
                        let (c, mut es) = parse_groups(c, flags);
                        errs.append(&mut es);
                        cond = c;
                    },
                    Some(Special('{')) => {
                        let mut depth = 1;
                        let mut idx = 2;
                        {
                            let mut toks = toks;
                            while 'cond: {
                                toks = &toks[1..];
                                idx += 1;
                                match toks.get(0).map(|x| &x.data) {
                                    None => break 'cond false,
                                    Some(Special('{')) => depth += 1,
                                    Some(Special('}')) => depth -= 1,
                                    _ => {}
                                }
                                depth > 0
                            } {}
                        }
                        let (c, ts) = toks.split_at(idx - 1);
                        toks = ts;
                        let (c, mut es) = parse_groups(c, flags);
                        errs.append(&mut es);
                        cond = c;
                    },
                    Some(x) => {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 380, Some(format!("expected '(' or '{{', got {x:#}"))));
                        cond = Box::new(NullAST::new(toks[0].loc.clone()));
                    }
                }
                let (body, _, mut es) = parse_expr(toks, "", flags);
                errs.append(&mut es);
                (Box::new(WhileAST::new(loc, cond, body)), errs)
            },
            _ => parse_calls(toks, flags)
        }
    }
    else {parse_calls(toks, flags)}
}
fn parse_statement(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    let mut errs = vec![];
    let start_idx = toks.iter().position(|x| !matches!(&x.data, Macro(..))).unwrap_or(toks.len());
    let val = toks.get(start_idx);
    if val.is_none() {
        return (null(toks), vec![]);
    }
    let val = val.unwrap();
    let ast = 'main: {
        match val.data {
            Statement(ref x) => match x.as_str() {
                "module" => {errs.push(Diagnostic::error(toks[0].loc.clone(), 275, None)); null(toks)},
                "import" => {
                    let annotations = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    let (name, idx, mut es) = parse_paths(&toks[1..], false);
                    let loc = toks[0].loc.clone();
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    Box::new(ImportAST::new(loc, name, annotations))
                },
                "fn" => {
                    let annotations = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    toks = &toks[start_idx..];
                    let start = toks[0].loc.clone();
                    let (mut name, idx, mut es) = parse_path(&toks[1..], "(=;");
                    if name.global || name.ids.len() > 1 {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 276, None));
                        name.global = false;
                        name.ids = name.ids.pop().map_or(vec![], |x| vec![x]);
                    }
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 234, None));
                        break 'main null(toks) as Box<dyn AST>;
                    }
                    match &toks[0].data {
                        Special('(') => {
                            let mut params = vec![];
                            let mut defaults = None;
                            loop {
                                if toks.len() < 2 {
                                    errs.push(Diagnostic::error(toks[0].loc.clone(), 238, None));
                                    break 'main null(toks) as Box<dyn AST>;
                                }
                                if toks[1].data == Special(')') {
                                    toks = &toks[2..];
                                    break;
                                }
                                let param_type = if let Statement(ref x) = toks[1].data {
                                    match x.as_str() {
                                        "mut" => {
                                            toks = &toks[2..];
                                            ParamType::Mutable
                                        },
                                        "const" => {
                                            toks = &toks[2..];
                                            ParamType::Constant
                                        },
                                        _ => {
                                            toks = &toks[1..];
                                            ParamType::Normal
                                        }
                                    }
                                }
                                else {
                                    toks = &toks[1..];
                                    ParamType::Normal
                                };
                                let id_start = toks[0].loc.clone();
                                let (mut name, idx, mut es) = parse_path(toks, ":,)");
                                toks = &toks[(idx - 1)..];
                                errs.append(&mut es);
                                if name.global || name.ids.len() > 1 {
                                    errs.push(Diagnostic::error(id_start, 239, None));
                                }
                                let name = name.ids.pop().map_or_else(String::new, |(name, _)| name);
                                let ty = if !toks.is_empty() && toks[0].data == Special(':') {
                                    let (ty, idx, mut es) = parse_expr(&toks[1..], ",)=", flags);
                                    toks = &toks[idx..];
                                    errs.append(&mut es);
                                    ty
                                }
                                else {
                                    let loc = unsafe {(*toks.as_ptr().offset(-1)).loc.clone()};
                                    errs.push(Diagnostic::error(loc.clone(), 242, None));
                                    Box::new(ErrorTypeAST::new(loc))
                                };
                                let default = if !toks.is_empty() && toks[0].data == Operator("=".to_string()) {
                                    if defaults.is_none() {defaults = Some(toks[0].loc.clone());}
                                    let (val, idx, mut es) = parse_expr(&toks[1..], ",)", flags);
                                    toks = &toks[idx..];
                                    errs.append(&mut es);
                                    Some(val)
                                }
                                else {
                                    if defaults.is_some() {
                                        errs.push(Diagnostic::error(toks[0].loc.clone(), 246, None));
                                    }
                                    None
                                };
                                params.push((name, param_type, ty, default));
                                if toks.is_empty() {
                                    errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 238, None));
                                    break 'main null(toks);
                                }
                                match &toks[0].data {
                                    Special(')') => {
                                        toks = &toks[1..];
                                        break;
                                    },
                                    Special(',') => {},
                                    x => errs.push(Diagnostic::error(toks[0].loc.clone(), 241, Some(format!("expected ',' or ')', got {x:#}"))))
                                }
                            }
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 240, None));
                                break 'main null(toks);
                            }
                            match &toks[0].data {
                                Special(';') => {
                                    let loc = toks[0].loc.clone();
                                    errs.push(Diagnostic::error(loc.clone(), 243, None));
                                    toks = &toks[1..];
                                    Box::new(FnDefAST::new(start, name, Box::new(NullAST::new(loc.clone())), params, Box::new(NullAST::new(loc)), annotations))
                                },
                                Special(':') => {
                                    let (ty, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                                    toks = &toks[idx..];
                                    errs.append(&mut es);
                                    if toks.is_empty() {
                                        let last = unsafe {(*toks.as_ptr().offset(-1)).loc.clone()};
                                        errs.push(Diagnostic::error(last.clone(), 244, None));
                                        break 'main Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(last)), annotations));
                                    }
                                    match &toks[0].data {
                                        Special(';') => break 'main Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(toks[0].loc.clone())), annotations)),
                                        Special('{') => {
                                            errs.push(Diagnostic::error(toks[0].loc.clone(), 245, None));
                                            let (ast, idx, mut es) = parse_expr(toks, ";", flags);
                                            toks = &toks[idx..];
                                            errs.append(&mut es);
                                            Box::new(FnDefAST::new(start, name, ty, params, ast, annotations)) as Box<dyn AST>
                                        },
                                        Operator(x) if x == "=" => {
                                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                            toks = &toks[(idx + 1)..];
                                            errs.append(&mut es);
                                            Box::new(FnDefAST::new(start, name, ty, params, ast, annotations)) as Box<dyn AST>
                                        },
                                        x => {errs.push(Diagnostic::error(toks[0].loc.clone(), 244, Some(format!("expected '=' or ';', got {x:#}")))); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>}
                                    }
                                },
                                Operator(x) if x == "=" => {
                                    let loc = toks[0].loc.clone();
                                    let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                    toks = &toks[(idx + 1)..];
                                    errs.append(&mut es);
                                    Box::new(FnDefAST::new(start, name, Box::new(NullAST::new(loc)), params, ast, annotations))
                                },
                                x => {errs.push(Diagnostic::error(toks[0].loc.clone(), 244, Some(format!("expected ':' or '=', got {x:#}")))); null(toks)}
                            }
                        },
                        Special(';') => {errs.push(Diagnostic::error(toks[0].loc.clone(), 235, None)); null(toks)},
                        x => {errs.push(Diagnostic::error(toks[0].loc.clone(), 236, Some(format!("expected function parameters, got {x:#}")))); null(toks)}
                    }
                },
                "let" => {
                    let annotations = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    toks = &toks[start_idx..];
                    let start = toks[0].loc.clone();
                    let (mut name, idx, mut es) = parse_path(&toks[1..], ":=");
                    if name.global || name.ids.len() > 1 {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 277, None));
                        name.global = false;
                        name.ids = name.ids.pop().map_or(vec![], |x| vec![x]);
                    }
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break 'main null(toks);
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                errs.append(&mut es);
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            Box::new(VarDefAST::new(start, name, ast, Some(t), annotations, false)) as Box<dyn AST>
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            Box::new(VarDefAST::new(start, name, ast, None, annotations, false)) as Box<dyn AST>
                        },
                        Special(';') => {errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>},
                        _ => {errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data)))); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>}
                    }
                },
                "mut" => {
                    let annotations = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    toks = &toks[start_idx..];
                    let start = toks[0].loc.clone();
                    let (mut name, idx, mut es) = parse_path(&toks[1..], ":=");
                    if name.global || name.ids.len() > 1 {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 277, None));
                        name.global = false;
                        name.ids = name.ids.pop().map_or(vec![], |x| vec![x]);
                    }
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break 'main null(toks);
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                errs.append(&mut es);
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            Box::new(MutDefAST::new(start, name, ast, Some(t), annotations, false)) as Box<dyn AST>
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            Box::new(MutDefAST::new(start, name, ast, None, annotations, false)) as Box<dyn AST>
                        },
                        Special(';') => {errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>},
                        _ => {errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data)))); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>}
                    }
                },
                "const" => {
                    let annotations = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    toks = &toks[start_idx..];
                    let start = toks[0].loc.clone();
                    let (mut name, idx, mut es) = parse_path(&toks[1..], ":=");
                    if name.global || name.ids.len() > 1 {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 277, None));
                        name.global = false;
                        name.ids = name.ids.pop().map_or(vec![], |x| vec![x]);
                    }
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break 'main null(toks);
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                errs.append(&mut es);
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            Box::new(ConstDefAST::new(start, name, ast, Some(t), annotations)) as Box<dyn AST>
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            Box::new(ConstDefAST::new(start, name, ast, None, annotations)) as Box<dyn AST>
                        },
                        Special(';') => {errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>},
                        _ => {errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data)))); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>}
                    }
                },
                "type" => {
                    let anns = toks.iter().take(start_idx).filter_map(|x| if let Macro(name, args) = &x.data {Some((name.clone(), args.as_ref().map(|(x, _)| x.clone()), x.loc.clone()))} else {None}).collect::<Vec<_>>();
                    toks = &toks[start_idx..];
                    let start = toks[0].loc.clone();
                    let (mut name, idx, mut es) = parse_path(&toks[1..], "=");
                    if name.global || name.ids.len() > 1 {
                        errs.push(Diagnostic::error(toks[0].loc.clone(), 277, None));
                        name.global = false;
                        name.ids = name.ids.pop().map_or(vec![], |x| vec![x]);
                    }
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break 'main null(toks);
                    }
                    match &toks[0].data {
                        Operator(x) if x == "=" => {
                            let (ty, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                break 'main null(toks);
                            }
                            Box::new(TypeDefAST::new(start, name, ty, anns)) as Box<dyn AST>
                        },
                        Special(';') => {errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>},
                        _ => {errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data)))); Box::new(NullAST::new(toks[0].loc.clone())) as Box<dyn AST>}
                    }
                },
                _ => {
                    let (ast, idx, mut es) = parse_expr(toks, ";", flags);
                    errs.append(&mut es);
                    toks = &toks[(idx - 1)..];
                    ast
                }
            },
            _ => {
                let (ast, idx, mut es) = parse_expr(toks, ";", flags);
                errs.append(&mut es);
                toks = &toks[(idx - 1)..];
                ast
            }
        }
    };
    errs.extend(toks.iter().map(|x| Diagnostic::error(x.loc.clone(), 203, Some(format!("expected ';', got {:#}", x.data)))));
    (ast, errs)
}
fn parse_dots(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if toks.len() < 3 {return parse_flow(toks, flags);}
    if let (toks, [Token {loc: _, data: Special('.')}, Token {loc: loc2, data: Identifier(id)}]) = toks.split_at(toks.len() - 2) {
        let (ast, errs) = parse_postfix(toks, flags);
        (Box::new(DotAST::new(ast, (id.clone(), loc2.clone()))), errs)
    }
    else {
        parse_flow(toks, flags)
    }
}
fn parse_postfix(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if let Some((tok, toks)) = toks.split_last() {
        if let Operator(op) = &tok.data {
            return if COBALT_POST_OPS.contains(&op.as_str()) {
                let (ast, errs) = parse_postfix(toks, flags);
                (Box::new(PostfixAST::new(tok.loc.clone(), op.clone(), ast)), errs)
            }
            else if op == "*" || op == "&" {
                match toks.last().map(|x| &x.data) {
                    Some(Statement(ref s)) => match s.as_str() {
                        "mut" => {
                            let mut loc = toks.last().unwrap().loc.clone();
                            loc.1.end = tok.loc.1.end;
                            let toks = &toks[..(toks.len() - 1)];
                            let (ast, errs) = parse_postfix(toks, flags);
                            (Box::new(PostfixAST::new(loc, "mut".to_string() + op, ast)), errs)
                        },
                        "const" => {
                            let mut loc = toks.last().unwrap().loc.clone();
                            loc.1.end = tok.loc.1.end;
                            let toks = &toks[..(toks.len() - 1)];
                            let (ast, errs) = parse_postfix(toks, flags);
                            (Box::new(PostfixAST::new(loc, "const".to_string() + op, ast)), errs)
                        },
                        _ => {
                            let (ast, errs) = parse_postfix(toks, flags);
                            (Box::new(PostfixAST::new(tok.loc.clone(), "const".to_string() + op, ast)), errs)
                        }
                    },
                    _ => {
                        let (ast, errs) = parse_postfix(toks, flags);
                        (Box::new(PostfixAST::new(tok.loc.clone(), "const".to_string() + op, ast)), errs)
                    }
                }
            }
            else {
                let (ast, mut errs) = parse_postfix(toks, flags);
                errs.insert(0, Diagnostic::error(tok.loc.clone(), 260, Some(format!("{op} is not a postfix operator"))));
                (ast, errs)
            };
        }
    }
    parse_dots(toks, flags)
}
fn parse_prefix(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if let Some((tok, toks)) = toks.split_first() {
        if let Operator(op) = &tok.data {
            return if COBALT_PRE_OPS.contains(&op.as_str()) {
                let (ast, errs) = parse_prefix(toks, flags);
                (Box::new(PrefixAST::new(tok.loc.clone(), op.clone(), ast)), errs)
            }
            else {
                let (ast, mut errs) = parse_prefix(toks, flags);
                errs.insert(0, Diagnostic::error(tok.loc.clone(), 265, Some(format!("{} is not a prefix operator", op))));
                (ast, errs)
            }
        };
    }
    parse_postfix(toks, flags)
}
fn parse_casts(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    let mut errs = vec![];
    {
        let mut it = toks.iter().rev();
        let mut idx = toks.len();
        'main: while let Some(tok) = it.next() {
            idx -= 1;
            match &tok.data {
                Special(')') => {
                    let mut depth = 1;
                    while depth > 0 {
                        match it.next().map(|x| &x.data) {
                            Some(Special(')')) => depth += 1,
                            Some(Special('(')) => depth -= 1,
                            None => break 'main,
                            _ => {}
                        }
                        idx -= 1;
                    }
                },
                Special(']') => {
                    let mut depth = 1;
                    while depth > 0 {
                        match it.next().map(|x| &x.data) {
                            Some(Special(']')) => depth += 1,
                            Some(Special('[')) => depth -= 1,
                            None => break 'main,
                            _ => {}
                        }
                        idx -= 1;
                    }
                },
                Special('}') => {
                    let mut depth = 1;
                    while depth > 0 {
                        match it.next().map(|x| &x.data) {
                            Some(Special('}')) => depth += 1,
                            Some(Special('{')) => depth -= 1,
                            None => break 'main,
                            _ => {}
                        }
                        idx -= 1;
                    }
                },
                Special('(') => break 'main,
                Special('[') => break 'main,
                Special('{') => break 'main,
                Special(':')  => {
                    match toks.get(idx + 1).map(|x| &x.data) {
                        Some(Operator(x)) if x == "?" => {
                            let (lhs, mut es) = parse_casts(&toks[..idx], flags);
                            errs.append(&mut es);
                            let (rhs, _, mut es) = parse_expr(&toks[(idx + 2)..], "", flags);
                            errs.append(&mut es);
                            return (Box::new(BitCastAST::new(tok.loc.clone(), lhs, rhs)), errs);
                        },
                        _ => {
                            let (lhs, mut es) = parse_casts(&toks[..idx], flags);
                            errs.append(&mut es);
                            let (rhs, _, mut es) = parse_expr(&toks[(idx + 1)..], "", flags);
                            errs.append(&mut es);
                            return (Box::new(CastAST::new(tok.loc.clone(), lhs, rhs)), errs);
                        }
                    }
                },
                _ => if idx == 0 {break}
            }
        }
    }
    parse_prefix(toks, flags)
}
fn parse_binary<'a, F: Clone + for<'r> FnMut(&'r parser::ops::OpType) -> bool>(toks: &[Token], ops_arg: &[OpType], mut ops_it: std::slice::SplitInclusive<'a, OpType, F>, flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if ops_arg.is_empty() {return (Box::new(NullAST::new(toks[0].loc.clone())), vec![])}
    let (op_ty, ops) = ops_arg.split_last().unwrap();
    let mut errs = vec![];
    match op_ty {
        Ltr => {
            let mut it = toks.iter();
            let mut idx = 0;
            'main: while let Some(tok) = it.next() {
                match &tok.data {
                    Special('(') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special('(')) => depth += 1,
                                Some(Special(')')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx += 1;
                        }
                        idx += 1;
                    },
                    Special('[') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special('[')) => depth += 1,
                                Some(Special(']')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx += 1;
                        }
                        idx += 1;
                    },
                    Special('{') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special('{')) => depth += 1,
                                Some(Special('}')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx += 1;
                        }
                        idx += 1;
                    },
                    Special(')') => break 'main,
                    Special(']') => break 'main,
                    Special('}') => break 'main,
                    Operator(x) if ops.iter().any(|y| if let Op(op) = y {op == x} else {false}) && idx != 0 && idx != toks.len() - 1 && !matches!(&toks[idx - 1].data, Statement(_)) => {
                        let (rhs, mut es) = parse_binary(&toks[(idx + 1)..], ops_arg, ops_it.clone(), flags);
                        errs.append(&mut es);
                        let (lhs, mut es) = if let Some(op) = ops_it.next() {parse_binary(&toks[..idx], op, ops_it, flags)}
                        else {parse_casts(&toks[..idx], flags)};
                        errs.append(&mut es);
                        return (Box::new(BinOpAST::new(tok.loc.clone(), x.clone(), lhs, rhs)), errs);
                    },
                    _ => {idx += 1; if idx == toks.len() {break}}
                }
            }
        },
        Rtl => {
            let mut it = toks.iter().rev();
            let mut idx = toks.len() - 1;
            'main: while let Some(tok) = it.next() {
                match &tok.data {
                    Special(')') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special(')')) => depth += 1,
                                Some(Special('(')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx -= 1;
                        }
                    },
                    Special(']') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special(']')) => depth += 1,
                                Some(Special('[')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx -= 1;
                        }
                    },
                    Special('}') => {
                        let mut depth = 1;
                        while depth > 0 {
                            match it.next().map(|x| &x.data) {
                                Some(Special('}')) => depth += 1,
                                Some(Special('{')) => depth -= 1,
                                None => break 'main,
                                _ => {}
                            }
                            idx -= 1;
                        }
                    },
                    Special('(') => break 'main,
                    Special('[') => break 'main,
                    Special('{') => break 'main,
                    Operator(x) if ops.iter().any(|y| if let Op(op) = y {op == x} else {false}) && idx != 0 && idx != toks.len() - 1 && !matches!(&toks[idx - 1].data, Statement(_)) => {
                        let (lhs, mut es) = parse_binary(&toks[..idx], ops_arg, ops_it.clone(), flags);
                        errs.append(&mut es);
                        let (rhs, mut es) = if let Some(op) = ops_it.next() {parse_binary(&toks[(idx + 1)..], op, ops_it, flags)}
                        else {parse_casts(&toks[(idx + 1)..], flags)};
                        errs.append(&mut es);
                        return (Box::new(BinOpAST::new(tok.loc.clone(), x.clone(), lhs, rhs)), errs);
                    },
                    _ => if idx == 0 {break} else {idx -= 1}
                }
            }
        },
        Op(_) => panic!("ops.split_inclusive should end in Ltr or Rtl")
    }
    if let Some(op) = ops_it.next() {
        let (ast, mut es) = parse_binary(toks, op, ops_it, flags);
        errs.append(&mut es);
        (ast, errs)
    }
    else {
        let (ast, mut es) = parse_casts(toks, flags);
        errs.append(&mut es);
        (ast, errs)
    }
}
fn parse_tuple(mut toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    let mut errs = vec![];
    let mut comma = false;
    let mut vals = vec![];
    let mut it = toks.iter();
    let mut idx = 0;
    'main: while let Some(tok) = it.next() {
        match &tok.data {
            Special('(') => {
                let mut depth = 1;
                while depth > 0 {
                    match it.next().map(|x| &x.data) {
                        Some(Special('(')) => depth += 1,
                        Some(Special(')')) => depth -= 1,
                        None => break 'main,
                        _ => {}
                    }
                    idx += 1;
                }
                idx += 1;
            },
            Special('[') => {
                let mut depth = 1;
                while depth > 0 {
                    match it.next().map(|x| &x.data) {
                        Some(Special('[')) => depth += 1,
                        Some(Special(']')) => depth -= 1,
                        None => break 'main,
                        _ => {}
                    }
                    idx += 1;
                }
                idx += 1;
            },
            Special('{') => {
                let mut depth = 1;
                while depth > 0 {
                    match it.next().map(|x| &x.data) {
                        Some(Special('{')) => depth += 1,
                        Some(Special('}')) => depth -= 1,
                        None => break 'main,
                        _ => {}
                    }
                    idx += 1;
                }
                idx += 1;
            },
            Special(')') => break 'main,
            Special(']') => break 'main,
            Special('}') => break 'main,
            Special(',') => {
                comma = true;
                let (p, n) = toks.split_at(idx);
                vals.push(p);
                toks = &n[1..];
                idx = 0;
            },
            _ => {idx += 1; if idx == toks.len() {break}}
        }
    }
    if toks.len() > 0 {vals.push(toks);}
    let vals = vals.iter().map(|toks| if vals.len() == 1 {
        let mut it = COBALT_BIN_OPS.split_inclusive(|&x| x == Ltr || x == Rtl);
        let (ast, mut es) = parse_binary(toks, it.next().unwrap(), it, flags);
        errs.append(&mut es);
        ast
    } else {
        let (ast, _, mut es) = parse_expr_nosplit(toks, "", flags);
        errs.append(&mut es);
        ast
    }).collect();
    (if comma {Box::new(TupleLiteralAST::new(vals))} else {
        assert_eq!(vals.len(), 1);
        vals.into_iter().next().unwrap()
    }, errs)
}
fn parse_splits(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if toks.is_empty() {return (null(toks), vec![Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 290, None)])}
    let mut start = toks[0].loc.clone();
    start.1.end = toks.last().unwrap().loc.1.end;
    let len = toks.len();
    let mut idx = 0;
    let mut asts: Vec<Box<dyn AST>> = vec![];
    let mut errs = vec![];
    while idx <= len {
        if idx + 1 > len {
            asts.push(Box::new(NullAST::new(toks[idx - 1].loc.clone())));
            idx += 1;
        }
        else {
            let (ast, i, mut es) = parse_expr_nosplit(&toks[idx..], ";", flags);
            errs.append(&mut es);
            asts.push(ast);
            idx += i;
        }
    }
    match asts.len() {
        0 => (Box::new(NullAST::new(start)), errs),
        1 => (asts.into_iter().next().unwrap(), errs),
        _ => (Box::new(GroupAST::new(start, asts)), errs)
    }
}
fn parse_stmts(toks: &[Token], terminators: &'static str, flags: &Flags) -> (Box<dyn AST>, usize, Vec<Diagnostic>) {
    let mut i = 0;
    let mut errs = vec![];
    let mut anns_only = true;
    while i < toks.len() {
        let ii = i;
        match &toks[i].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(c) if c.len() == 1 && terminators.contains(c) => break,
            Statement(_) if anns_only => i += 1,
            Statement(k) if (k != "const" && k != "mut") && !matches!(toks.get(i + 1).and_then(|x| if let Operator(ref x) = x.data {Some(x.as_str())} else {None}).unwrap_or(""), "&" | "*" | "^") => {errs.push(Diagnostic::error(toks[i].loc.clone(), 280, Some(format!("expected ';', got {k:?}")))); break},
            Special('(') => {
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
            },
            Special('[') => {
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
            },
            Special('{') => {
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
            },
            Special(')') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 251, None)); break;},
            Special(']') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 253, None)); break;},
            Special('}') => break,
            _ => i += 1
        }
        if anns_only {anns_only = matches!(toks[ii].data, Macro(..))};
    }
    let (ast, mut es) = parse_statement(&toks[..i], flags);
    errs.append(&mut es);
    (ast, i + 1, errs)
}
fn parse_expr_nosplit(toks: &[Token], terminators: &'static str, flags: &Flags) -> (Box<dyn AST>, usize, Vec<Diagnostic>) {
    let mut i = 0;
    let mut errs = vec![];
    while i < toks.len() {
        match &toks[i].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(c) if c.len() == 1 && terminators.contains(c) => break,
            Statement(_) if i == 0 => i += 1,
            Statement(k) if (k != "const" && k != "mut") || !matches!(toks.get(i + 1).and_then(|x| if let Operator(ref x) = x.data {Some(x.as_str())} else {None}).unwrap_or(""), "&" | "*" | "^") => {errs.push(Diagnostic::error(toks[i].loc.clone(), 280, Some(format!("expected ';', got {k:?}")))); break},
            Special('(') => {
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
            },
            Special('[') => {
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
            },
            Special('{') => {
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
            },
            Special(')') => break,
            Special(']') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 253, None)); break;},
            Special('}') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 255, None)); break;},
            _ => i += 1
        }
    }
    let (ast, mut es) = parse_tuple(&toks[..i], flags);
    errs.append(&mut es);
    (ast, i + 1, errs)
}
fn parse_expr(toks: &[Token], terminators: &'static str, flags: &Flags) -> (Box<dyn AST>, usize, Vec<Diagnostic>) {
    let mut i = 0;
    let mut errs = vec![];
    while i < toks.len() {
        match &toks[i].data {
            Special(c) if terminators.contains(*c) => break,
            Operator(c) if c.len() == 1 && terminators.contains(c) => break,
            Statement(_) if i == 0 => i += 1,
            Statement(k) if (k != "const" && k != "mut") || !matches!(toks.get(i + 1).and_then(|x| if let Operator(ref x) = x.data {Some(x.as_str())} else {None}).unwrap_or(""), "&" | "*" | "^") => {errs.push(Diagnostic::error(toks[i].loc.clone(), 280, Some(format!("expected ';', got {k:?}")))); break},
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
                    errs.push(Diagnostic::error(start, 250, None));
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
                    errs.push(Diagnostic::error(start, 252, None));
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
                    errs.push(Diagnostic::error(start, 254, None));
                }
            },
            Special(')') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 251, None)); break;},
            Special(']') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 253, None)); break;},
            Special('}') => {errs.push(Diagnostic::error(toks[i].loc.clone(), 255, None)); break;},
            _ => i += 1
        }
    }
    let (ast, mut es) = parse_splits(&toks[..i], flags);
    errs.append(&mut es);
    (ast, i + 1, errs)
}
fn parse_tl(mut toks: &[Token], flags: &Flags, is_tl: bool) -> (Vec<Box<dyn AST>>, Option<usize>, Vec<Diagnostic>) {
    let mut outs: Vec<Box<dyn AST>> = vec![];
    let mut errs = vec![];
    let mut i = 0;
    let mut annotations = vec![];
    'main: while !toks.is_empty() {
        let val = &toks[0];
        match &val.data {
            Macro(name, params) => {i += 1; annotations.push((name.clone(), params.as_ref().map(|(x, _)| x.clone()), toks[0].loc.clone())); toks = &toks[1..];}
            Special(';') => {
                if !annotations.is_empty() {
                    errs.push(Diagnostic::error(val.loc.clone(), 281, None));
                    annotations = vec![];
                }
                i += 1; 
                toks = &toks[1..];
            },
            Special('}') => if is_tl {
                errs.push(Diagnostic::error(val.loc.clone(), 255, None));
                i += 1;
                toks = &toks[1..];
            } else {break 'main},
            Statement(ref x) => match x.as_str() {
                "module" => {
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    let (name, idx, mut es) = parse_path(&toks[1..], "=;{");
                    i += idx;
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(val.loc.clone(), 201, None));
                        break;
                    }
                    match &toks[0].data {
                        Special('{') => {
                            let (vals, idx, mut e) = parse_tl(&toks[1..], flags, false);
                            if let Some(idx) = idx {
                                outs.push(Box::new(ModuleAST::new(toks[0].loc.clone(), name, vals, anns)));
                                errs.append(&mut e);
                                toks = &toks[(idx + 1)..];
                                i += idx + 1;
                            }
                            else {
                                errs.push(Diagnostic::error(toks[0].loc.clone(), 254, None));
                                toks = &[];
                                break;
                            }
                        },
                        Operator(s) if s == "=" => {
                            let (oname, idx, mut es) = parse_path(toks, ";");
                            i += idx;
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            if toks.last().map(|x| &x.data) == Some(&Special(';')) {
                                errs.push(Diagnostic::error(val.loc.clone(), 202, None));
                                break;
                            }
                            let mut cname: CompoundDottedName = oname.into();
                            cname.ids.push(CompoundDottedNameSegment::Glob(toks[0].loc.clone()));
                            outs.push(Box::new(ModuleAST::new(toks[0].loc.clone(), name, vec![Box::new(ImportAST::new(toks[0].loc.clone(), cname, vec![]))], anns)));
                        },
                        Special(';') => {
                            outs.push(Box::new(ModuleAST::new(toks[0].loc.clone(), name, vec![], anns)));
                        },
                        x => unreachable!("unexpected value after module: {:#}", x)
                    }
                },
                "import" => {
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    let (name, idx, mut es) = parse_paths(&toks[1..], false);
                    outs.push(Box::new(ImportAST::new(toks[0].loc.clone(), name, anns)));
                    errs.append(&mut es);
                    i += idx + 1;
                    toks = &toks[(idx + 1)..];
                },
                "fn" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], "(=;");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 234, None));
                        break;
                    }
                    match &toks[0].data {
                        Special('(') => {
                            let mut params = vec![];
                            let mut defaults = None;
                            loop {
                                if toks.len() < 2 {
                                    errs.push(Diagnostic::error(toks[0].loc.clone(), 238, None));
                                    break 'main;
                                }
                                if toks[1].data == Special(')') {
                                    toks = &toks[2..];
                                    i += 2;
                                    break;
                                }
                                let param_type = if let Statement(ref x) = toks[1].data {
                                    match x.as_str() {
                                        "mut" => {
                                            toks = &toks[2..];
                                            i += 2;
                                            ParamType::Mutable
                                        },
                                        "const" => {
                                            toks = &toks[2..];
                                            i += 2;
                                            ParamType::Constant
                                        },
                                        _ => {
                                            toks = &toks[1..];
                                            i += 1;
                                            ParamType::Normal
                                        }
                                    }
                                }
                                else {
                                    toks = &toks[1..];
                                    i += 1;
                                    ParamType::Normal
                                };
                                let id_start = toks[0].loc.clone();
                                let (mut name, idx, mut es) = parse_path(toks, ":,)");
                                toks = &toks[(idx - 1)..];
                                i += idx - 1;
                                errs.append(&mut es);
                                if name.global || name.ids.len() > 1 {
                                    errs.push(Diagnostic::error(id_start, 239, None));
                                }
                                let name = name.ids.pop().map_or_else(String::new, |(name, _)| name);
                                let ty = if !toks.is_empty() && toks[0].data == Special(':') {
                                    let (ty, idx, mut es) = parse_expr(&toks[1..], ",)=", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    ty
                                }
                                else {
                                    let loc = unsafe {(*toks.as_ptr().offset(-1)).loc.clone()};
                                    errs.push(Diagnostic::error(loc.clone(), 242, None));
                                    Box::new(ErrorTypeAST::new(loc))
                                };
                                let default = if !toks.is_empty() && toks[0].data == Operator("=".to_string()) {
                                    if defaults.is_none() {defaults = Some(toks[0].loc.clone());}
                                    let (val, idx, mut es) = parse_expr(&toks[1..], ",)", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    Some(val)
                                }
                                else {
                                    if defaults.is_some() {
                                        errs.push(Diagnostic::error(toks[0].loc.clone(), 246, None));
                                    }
                                    None
                                };
                                params.push((name, param_type, ty, default));
                                if toks.is_empty() {
                                    errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 238, None));
                                    break 'main;
                                }
                                match &toks[0].data {
                                    Special(')') => {
                                        toks = &toks[1..];
                                        i += 1;
                                        break;
                                    },
                                    Special(',') => {},
                                    x => errs.push(Diagnostic::error(toks[0].loc.clone(), 242, Some(format!("expected ',' or ')' after parameter, got {x:#}"))))
                                }
                            }
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 238, None));
                                break;
                            }
                            match &toks[0].data {
                                Special(';') => {
                                    let loc = toks[0].loc.clone();
                                    errs.push(Diagnostic::error(loc.clone(), 243, None));
                                    outs.push(Box::new(FnDefAST::new(start, name, Box::new(NullAST::new(loc.clone())), params, Box::new(NullAST::new(loc)), anns)));
                                    toks = &toks[1..];
                                    i += 1;
                                },
                                Special(':') => {
                                    let (ty, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                                    toks = &toks[idx..];
                                    i += idx;
                                    errs.append(&mut es);
                                    if toks.is_empty() {
                                        let last = unsafe {(*toks.as_ptr().offset(-1)).loc.clone()};
                                        errs.push(Diagnostic::error(last.clone(), 244, None));
                                        outs.push(Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(last)), anns)));
                                        break;
                                    }
                                    match &toks[0].data {
                                        Special(';') => outs.push(Box::new(FnDefAST::new(start, name, ty, params, Box::new(NullAST::new(toks[0].loc.clone())), anns))),
                                        Special('{') => {
                                            errs.push(Diagnostic::error(toks[0].loc.clone(), 245, None));
                                            let (ast, idx, mut es) = parse_expr(toks, ";", flags);
                                            toks = &toks[idx..];
                                            i += idx;
                                            errs.append(&mut es);
                                            outs.push(Box::new(FnDefAST::new(start, name, ty, params, ast, anns)));
                                        },
                                        Operator(x) if x == "=" => {
                                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                            toks = &toks[(idx + 1)..];
                                            i += idx + 1;
                                            errs.append(&mut es);
                                            outs.push(Box::new(FnDefAST::new(start, name, ty, params, ast, anns)));
                                        },
                                        x => errs.push(Diagnostic::error(toks[0].loc.clone(), 244, Some(format!("expected '=' or ';', got {x:#}"))))
                                    }
                                },
                                Operator(x) if x == "=" => {
                                    let loc = toks[0].loc.clone();
                                    let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                    toks = &toks[(idx + 1)..];
                                    i += idx + 1;
                                    errs.append(&mut es);
                                    outs.push(Box::new(FnDefAST::new(start, name, Box::new(NullAST::new(loc)), params, ast, anns)));
                                },
                                x => errs.push(Diagnostic::error(toks[0].loc.clone(), 244, Some(format!("expected ':' or '=', got {x:#}"))))
                            }
                        },
                        Special(';') => errs.push(Diagnostic::error(toks[0].loc.clone(), 235, None)),
                        x => errs.push(Diagnostic::error(toks[0].loc.clone(), 234, Some(format!("expected '(' or '=', got {x:#}"))))
                    }
                },
                "let" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], ":=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break;
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 232, None));
                                break;
                            }
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                i += idx;
                                errs.append(&mut es);
                                if toks.is_empty() {
                                    errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                    break;
                                }
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            outs.push(Box::new(VarDefAST::new(start, name, ast, Some(t), anns, true)));
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                break;
                            }
                            outs.push(Box::new(VarDefAST::new(start, name, ast, None, anns, true)));
                        },
                        Special(';') => errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)),
                        _ => errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data))))
                    }
                },
                "mut" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], ":=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break;
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 232, None));
                                break;
                            }
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                i += idx;
                                errs.append(&mut es);
                                if toks.is_empty() {
                                    errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                    break;
                                }
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            outs.push(Box::new(MutDefAST::new(start, name, ast, Some(t), anns, true)));
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                break;
                            }
                            outs.push(Box::new(MutDefAST::new(start, name, ast, None, anns, true)));
                        },
                        Special(';') => errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)),
                        _ => errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data))))
                    }
                },
                "const" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], ":=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break;
                    }
                    match &toks[0].data {
                        Special(':') => {
                            let (t, idx, mut es) = parse_expr(&toks[1..], "=;", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 232, None));
                                break;
                            }
                            let ast = if toks[0].data == Operator("=".to_string()) {
                                let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                                toks = &toks[idx..];
                                i += idx;
                                errs.append(&mut es);
                                if toks.is_empty() {
                                    errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                    break;
                                }
                                ast
                            }
                            else {Box::new(NullAST::new(toks[0].loc.clone()))};
                            outs.push(Box::new(ConstDefAST::new(start, name, ast, Some(t), anns)));
                        },
                        Operator(x) if x == "=" => {
                            let (ast, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                break;
                            }
                            outs.push(Box::new(ConstDefAST::new(start, name, ast, None, anns)));
                        },
                        Special(';') => errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)),
                        _ => errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data))))
                    }
                },
                "type" => {
                    let start = toks[0].loc.clone();
                    let (name, idx, mut es) = parse_path(&toks[1..], "=");
                    toks = &toks[idx..];
                    i += idx;
                    errs.append(&mut es);
                    let mut anns = vec![];
                    std::mem::swap(&mut annotations, &mut anns);
                    if toks.is_empty() {
                        errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 230, None));
                        break;
                    }
                    match &toks[0].data {
                        Operator(x) if x == "=" => {
                            let (ty, idx, mut es) = parse_expr(&toks[1..], ";", flags);
                            toks = &toks[idx..];
                            i += idx;
                            errs.append(&mut es);
                            if toks.is_empty() {
                                errs.push(Diagnostic::error(unsafe {(*toks.as_ptr().offset(-1)).loc.clone()}, 231, None));
                                break;
                            }
                            outs.push(Box::new(TypeDefAST::new(start, name, ty, anns)));
                        },
                        Special(';') => errs.push(Diagnostic::error(toks[0].loc.clone(), 233, None)),
                        _ => errs.push(Diagnostic::error(toks[0].loc.clone(), 230, Some(format!("got {:#}", toks[0].data))))
                    }
                    
                },
                x => {
                    errs.push(Diagnostic::error(val.loc.clone(), 200, Some(format!("unexpected {x:#}"))));
                    i += 1;
                    toks = &toks[1..];
                }
            },
            x => {
                errs.push(Diagnostic::error(val.loc.clone(), 200, Some(format!("unexpected {x:#}"))));
                i += 1;
                toks = &toks[1..];
            }
        }
    }
    (outs, if toks.is_empty() {None} else {Some(i + 1)}, errs)
}
pub fn parse(toks: &[Token], flags: &Flags) -> (Box<dyn AST>, Vec<Diagnostic>) {
    if toks.is_empty() {
        return (Box::new(TopLevelAST::new((0, 0..0), vec![])), vec![])
    }
    let start = toks[0].loc.clone(); // already bounds checked
    let (out, _, errs) = parse_tl(toks, flags, true);
    (Box::new(TopLevelAST::new(start, out)), errs)
}
