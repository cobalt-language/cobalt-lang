use crate::{*, ast::scope::ImportAST};
fn parse_paths(toks: &[Token], flags: Flags) -> (CompoundDottedName, Vec<Error>) {
    (CompoundDottedName{ids: vec![], global: false}, vec![])
}
fn parse_path(toks: &[Token], terminators: &'static str) -> (DottedName, usize, Vec<Error>) {
    let mut idx = 1;
    let mut errs = vec![];
    let (mut name, mut lwp) = match &toks[0].data {
        Special('.') => (DottedName::new(vec![], true), true),
        Identifier(str) => (DottedName::new(vec![str.clone()], false), false),
        x => return (DottedName::local(String::from("")), 1, vec![Error::new(toks[0].loc, 210, format!("unexpected token {:?} in identifier", x))])
    };
    while idx < toks.len() {
        match &toks[idx].data {
            Special(c) if terminators.contains(*c) => break,
            Keyword(s) if s.len() == 1 && terminators.contains(unsafe {s.get_unchecked(0..1)}) => break,
            Special('.') => {
                if lwp {
                    errs.push(Error::new(toks[idx].loc, 211, String::from("identifier cannot contain consecutive periods")).note(Note{loc: toks[idx].loc, message: String::from("Did you accidentally type two?")}))
                }
                lwp = true;
                idx += 1;
            }
            Identifier(str) => {
                if !lwp {
                    errs.push(Error::new(toks[idx].loc, 212, String::from("identifier cannot contain consecutive identifiers")).note(Note{loc: toks[idx].loc, message: String::from("Did you forget a period?")}))
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
    (name, idx, errs)
}
#[allow(dead_code, unused_variables, unused_mut)]
fn parse_tl(mut toks: &[Token], flags: &Flags) -> (Vec<Box<dyn AST>>, usize, Vec<Error>) {
    let mut outs: Vec<Box<dyn AST>> = vec![];
    let mut errs = vec![];
    let mut i = 0;
    while toks.len() != 0 {
        let val = &toks[0];
        match val.data {
            Special(';') => {},
            Special('}') => break,
            Keyword(ref x) => match x.as_str() {
                "module" => {
                    let (name, idx, mut es) = parse_path(toks, "=;{");
                    i += idx;
                    toks = &toks[idx..];
                    errs.append(&mut es);
                    if toks.len() == 0 {
                        errs.push(Error::new(val.loc, 202, String::from("expected module body, got EOF")));
                        break;
                    }
                    match &toks[0].data {
                        Special('{') => {
                            let (vals, idx, mut e) = parse_tl(&toks, flags);
                            if idx == toks.len() {
                                if toks[idx - 1].data != Special('}') {
                                    errs.push(Error::new(toks[0].loc, 220, String::from("unmatched opening brace of module body")))
                                }
                                else {
                                    outs.push(Box::new(ast::scope::ModuleAST::new(toks[0].loc, name, vals)));
                                }
                                break;
                            }
                            outs.push(Box::new(ast::scope::ModuleAST::new(toks[0].loc, name, vals)));
                            errs.append(&mut e);
                            toks = &toks[idx..];
                            i += idx;
                        },
                        Operator(s) if s == "=" => {
                            let (oname, idx, mut es) = parse_path(toks, ";");
                            i += idx;
                            toks = &toks[idx..];
                            errs.append(&mut es);
                            if toks.len() == 0 {
                                errs.push(Error::new(val.loc, 202, String::from("expected semicolon after module assignment")));
                                break;
                            }
                            let mut cname: CompoundDottedName = oname.into();
                            cname.ids.push(CompoundDottedNameSegment::Glob(String::from("*")));
                            outs.push(Box::new(ModuleAST::new(toks[0].loc, name, vec![Box::new(ImportAST::new(toks[0].loc, cname))])));
                        },
                        Special(';') => {
                            outs.push(Box::new(ast::scope::ModuleAST::new(toks[0].loc, name, vec![])));
                        },
                        _ => unreachable!("")
                    }
                },
                "import" => {},
                "fn" => {},
                "cr" => {},
                "let" => {},
                "mut" => {},
                _ => errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)))
            },
            _ => errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)))
        }
    };
    (outs, i, errs)
}
#[allow(unused_variables)]
pub fn parse(toks: &[Token], flags: Flags) -> (Box<dyn AST>, Vec<Error>) {
    panic!("parser has not been implemented")
}
