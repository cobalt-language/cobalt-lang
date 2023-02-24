use crate::*;
use glob::Pattern;
pub struct ModuleAST {
    loc: Location,
    pub name: DottedName,
    pub vals: Vec<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, Location)>
}
impl AST for ModuleAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().get(0) == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = if negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()) {1} else {0}},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
            }
        }
        if target_match == 0 {return (Value::null(), errs)}
        ctx.map_vars(|mut v| {
            match v.lookup_mod(&self.name) {
                Ok((m, i)) => Box::new(VarMap {parent: Some(v), symbols: m, imports: i}),
                Err(UndefVariable::NotAModule(x)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                    Box::new(VarMap::new(Some(v)))
                },
                Err(UndefVariable::DoesNotExist(x)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                    Box::new(VarMap::new(Some(v)))
                }
            }
        });
        let old_scope = ctx.push_scope(&self.name);
        errs.extend(self.vals.iter().flat_map(|val| val.codegen(ctx).1));
        ctx.restore_scope(old_scope);
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms)));
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "module: {}", self.name)?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
}
impl ModuleAST {
    pub fn new(loc: Location, name: DottedName, vals: Vec<Box<dyn AST>>, annotations: Vec<(String, Option<String>, Location)>) -> Self {ModuleAST {loc, name, vals, annotations}}
}
pub struct ImportAST {
    loc: Location,
    pub name: CompoundDottedName
}
impl AST for ImportAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        ctx.with_vars(|v| v.imports.push(self.name.clone()));
        (Value::null(), vec![])
    }
    fn to_code(&self) -> String {
        format!("import {}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "import: {}", self.name)
    }
}
impl ImportAST {
    pub fn new(loc: Location, name: CompoundDottedName) -> Self {ImportAST {loc, name}}
}
