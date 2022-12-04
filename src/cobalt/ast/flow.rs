use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct BlockAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for BlockAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl BlockAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {BlockAST {loc, vals}}
}
pub struct GroupAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for GroupAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl GroupAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {GroupAST {loc, vals}}
}
pub struct TopLevelAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for TopLevelAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
impl TopLevelAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {TopLevelAST {loc, vals}}
}