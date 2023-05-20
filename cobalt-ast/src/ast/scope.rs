use crate::*;
use glob::Pattern;
#[derive(Debug, Clone)]
pub struct ModuleAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub vals: Vec<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>
}
impl AST for ModuleAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = Vec::<CobaltError>::new();
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc, 427, Some(format!("error at byte {}: {}", err.pos, err.msg))).into())
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc, 426, None).into());
                    }
                },
                "export" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc, 428, None).note(vs, "previously defined here".to_string()).into());
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(x) => errs.push(Diagnostic::error(loc, 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))).into())
                        }
                    }
                },
                "private" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc, 428, None).note(vs, "previously defined here".to_string()).into());
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(x) => errs.push(Diagnostic::error(loc, 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))).into())
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc, 410, Some(format!("unknown annotation {x:?} for variable definition"))).into())
            }
        }
        if target_match == 0 {return (Value::null(), errs)}
        ctx.map_vars(|mut v| {
            match v.lookup_mod(&self.name) {
                Ok((m, i, _)) => Box::new(VarMap {parent: Some(v), symbols: m, imports: i}),
                Err(UndefVariable::NotAModule(x)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))).into());
                    Box::new(VarMap::new(Some(v)))
                },
                Err(UndefVariable::DoesNotExist(x)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))).into());
                    Box::new(VarMap::new(Some(v)))
                }
            }
        });
        let old_scope = ctx.push_scope(&self.name);
        let old_vis = if let Some((v, _)) = vis_spec {ctx.export.replace(v)} else {false};
        if ctx.flags.prepass {
            self.vals.iter().for_each(|val| val.varfwd_prepass(ctx));
            let mut again = true;
            while again {
                again = false;
                self.vals.iter().for_each(|val| val.constinit_prepass(ctx, &mut again));
            }
            self.vals.iter().for_each(|val| val.fwddef_prepass(ctx));
        }
        errs.extend(self.vals.iter().flat_map(|val| val.codegen(ctx).1));
        ctx.restore_scope(old_scope);
        if vis_spec.is_some() {ctx.export.set(old_vis)}
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms, ctx.mangle(&self.name))));
        (Value::null(), errs)
    }
    fn to_code(&self) -> String {
        let mut out = format!("module {} {{", self.name);
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            out += ";";
            if count > 1 {out += " ";}
            count -= 1;
        }
        out + "}"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "module: {}", self.name)?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
impl ModuleAST {
    pub fn new(loc: SourceSpan, name: DottedName, vals: Vec<Box<dyn AST>>, annotations: Vec<(String, Option<String>, SourceSpan)>) -> Self {ModuleAST {loc, name, vals, annotations}}
}
#[derive(Debug, Clone)]
pub struct ImportAST {
    loc: SourceSpan,
    pub name: CompoundDottedName,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>
}
impl ImportAST {
    pub fn new(loc: SourceSpan, name: CompoundDottedName, annotations: Vec<(String, Option<String>, SourceSpan)>) -> Self {ImportAST {loc, name, annotations}}
}
impl AST for ImportAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc, 427, Some(format!("error at byte {}: {}", err.pos, err.msg))).into())
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc, 426, None).into());
                    }
                },
                "export" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc, 428, None).note(vs, "previously defined here".to_string()).into());
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(x) => errs.push(Diagnostic::error(loc, 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))).into())
                        }
                    }
                },
                "private" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc, 428, None).note(vs, "previously defined here".to_string()).into());
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(x) => errs.push(Diagnostic::error(loc, 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))).into())
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc, 410, Some(format!("unknown annotation {x:?} for variable definition"))).into())
            }
        }
        if target_match == 0 {return (Value::null(), errs)}
        ctx.with_vars(|v| {
            let vec = v.verify(&self.name);
            errs.extend(vec.into_iter().map(|l| Diagnostic::warning(l, 90, None)).map(CobaltError::from));
            v.imports.push((self.name.clone(), vis_spec.map_or(ctx.export.get(), |(v, _)| v)))
        });
        (Value::null(), errs)
    }
    fn to_code(&self) -> String {
        format!("import {}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "import: {}", self.name)
    }
}
