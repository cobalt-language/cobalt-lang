use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
pub struct CastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: Option<ParsedType>
}
impl CastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: Option<ParsedType>) -> Self {
        CastAST {loc, val, target}
    }
}
impl AST for CastAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        match &self.target {
            Some(x) => format!("{}: {}", self.val.to_code(), x),
            None => format!("{}: <error-type>", self.val.to_code())
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        match &self.target {
            Some(x) => writeln!(f, "cast: {}", x),
            None => writeln!(f, "cast: <error-type>")
        }?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct NullAST {
    loc: Location
}
impl NullAST {
    pub fn new(loc: Location) -> Self {
        NullAST {loc}
    }
}
impl AST for NullAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        "null".to_string()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "null")
    }
}
