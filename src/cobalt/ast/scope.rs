use crate::*;
pub struct ModuleAST {
    loc: Location,
    pub name: DottedName,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for ModuleAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
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
