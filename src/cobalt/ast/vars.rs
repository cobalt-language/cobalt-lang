use crate::*;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&'ctx self, ctx: &'ctx CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        format!("let {} = {}", self.name, self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "vardef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>) -> Self {VarDefAST {loc, name, val}}
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>
}
impl AST for MutDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        format!("mut {} = {}", self.name, self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mutdef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>) -> Self {MutDefAST {loc, name, val}}
}
pub struct VarGetAST {
    loc: Location,
    pub name: DottedName
}
impl VarGetAST {
    pub fn new(loc: Location, name: DottedName) -> Self {VarGetAST {loc, name}}
}
impl AST for VarGetAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        format!("{}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "varget: {}", self.name)
    }
}
