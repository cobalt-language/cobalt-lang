use crate::*;
pub struct ModuleAST {
    loc: Location,
    pub name: DottedName,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for ModuleAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        ctx.map_vars(|mut v| {
            match v.lookup_mod(&self.name) {
                Ok(m) => Box::new(VarMap {parent: Some(v), symbols: m}),
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
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), v.symbols));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms)));
        (Variable::null(None), errs)
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
    pub fn new(loc: Location, name: DottedName, vals: Vec<Box<dyn AST>>) -> Self {ModuleAST {loc, name, vals}}
}
pub struct ImportAST {
    loc: Location,
    pub name: CompoundDottedName
}
impl AST for ImportAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {todo!("code generation has not been implemented for imports")}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {todo!("code generation has not been implemented for imports")}
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
