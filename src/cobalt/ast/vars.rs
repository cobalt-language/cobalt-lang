use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct VarDefAST<'loc> {
    loc: Location<'loc>,
    pub name: DottedName,
    pub val: Box<dyn AST<'loc>>
}
impl<'loc> AST<'loc> for VarDefAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl<'loc> VarDefAST<'loc> {
    pub fn new(loc: Location<'loc>, name: DottedName, val: Box<dyn AST<'loc>>) -> Self {VarDefAST {loc, name, val}}
}
pub struct MutDefAST<'loc> {
    loc: Location<'loc>,
    pub name: DottedName,
    pub val: Box<dyn AST<'loc>>
}
impl<'loc> AST<'loc> for MutDefAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl<'loc> MutDefAST<'loc> {
    pub fn new(loc: Location<'loc>, name: DottedName, val: Box<dyn AST<'loc>>) -> Self {MutDefAST {loc, name, val}}
}
pub struct VarGetAST<'loc> {
    loc: Location<'loc>,
    pub name: DottedName
}
impl<'loc> AST<'loc> for VarGetAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}