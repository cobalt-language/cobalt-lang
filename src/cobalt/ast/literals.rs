use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
struct IntLiteralAST<'loc> {
    loc: Location<'loc>,
    pub val: i128,
    pub suffix: Option<String>
}
impl<'loc> IntLiteralAST<'loc> {
    pub fn new(loc: Location<'loc>, val: i128, suffix: Option<String>) -> Self {IntLiteralAST {loc, val, suffix}}
}
impl<'loc> AST<'loc> for IntLiteralAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
struct FloatLiteralAST<'loc> {
    loc: Location<'loc>,
    pub val: f64,
    pub suffix: Option<String>
}
impl<'loc> FloatLiteralAST<'loc> {
    pub fn new(loc: Location<'loc>, val: f64, suffix: Option<String>) -> Self {FloatLiteralAST {loc, val, suffix}}
}
impl<'loc> AST<'loc> for FloatLiteralAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
struct CharLiteralAST<'loc> {
    loc: Location<'loc>,
    pub val: char,
    pub suffix: Option<String>
}
impl<'loc> CharLiteralAST<'loc> {
    pub fn new(loc: Location<'loc>, val: char, suffix: Option<String>) -> Self {CharLiteralAST {loc, val, suffix}}
}
impl<'loc> AST<'loc> for CharLiteralAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}
struct StringLiteralAST<'loc> {
    loc: Location<'loc>,
    pub val: String,
    pub suffix: Option<String>
}
impl<'loc> StringLiteralAST<'loc> {
    pub fn new(loc: Location<'loc>, val: String, suffix: Option<String>) -> Self {StringLiteralAST {loc, val, suffix}}
}
impl<'loc> AST<'loc> for StringLiteralAST<'loc> {
    fn loc(&self) -> Location<'loc> {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
}