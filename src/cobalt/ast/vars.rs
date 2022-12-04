use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>) -> Self {MutDefAST {loc, name, val}}
}
pub struct VarGetAST {
    loc: Location,
    pub name: DottedName
}
impl AST for VarGetAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}