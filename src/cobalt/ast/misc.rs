use crate::*;
pub struct CastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: ParsedType
}
impl CastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: ParsedType) -> Self {CastAST {loc, val, target}}
}
impl AST for CastAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        format!("{}: {}", self.val.to_code(), self.target)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "cast: {}", self.target)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct NullAST {
    loc: Location
}
impl NullAST {
    pub fn new(loc: Location) -> Self {NullAST {loc}}
}
impl AST for NullAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        "null".to_string()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "null")
    }
}
