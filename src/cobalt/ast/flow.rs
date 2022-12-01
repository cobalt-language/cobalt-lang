use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct BlockAST<'loc> {
    loc: Location<'loc>,
    pub vals: Vec<Box<dyn AST<'loc>>>
}
impl<'loc> AST<'loc> for BlockAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl<'loc> BlockAST<'loc> {
    pub fn new(loc: Location<'loc>, vals: Vec<Box<dyn AST<'loc>>>) -> Self {BlockAST {loc, vals}}
}
pub struct GroupAST<'loc> {
    loc: Location<'loc>,
    pub vals: Vec<Box<dyn AST<'loc>>>
}
impl<'loc> AST<'loc> for GroupAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl<'loc> GroupAST<'loc> {
    pub fn new(loc: Location<'loc>, vals: Vec<Box<dyn AST<'loc>>>) -> Self {GroupAST {loc, vals}}
}
pub struct TopLevelAST<'loc> {
    loc: Location<'loc>,
    pub vals: Vec<Box<dyn AST<'loc>>>
}
impl<'loc> AST<'loc> for TopLevelAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl<'loc> TopLevelAST<'loc> {
    pub fn new(loc: Location<'loc>, vals: Vec<Box<dyn AST<'loc>>>) -> Self {TopLevelAST {loc, vals}}
}