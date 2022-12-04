use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct ModuleAST {
    loc: Location,
    pub name: DottedName,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for ModuleAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl ImportAST {
    pub fn new(loc: Location, name: CompoundDottedName) -> Self {ImportAST {loc, name}}
}