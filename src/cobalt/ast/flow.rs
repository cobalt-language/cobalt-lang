use crate::*;
pub struct BlockAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for BlockAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        let mut out = '{'.to_string();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if count != 1 {out += "; ";}
            count -= 1;
        }
        out + "}"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "block")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
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
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        let mut out = '('.to_string();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if count != 1 {out += "; ";}
            count -= 1;
        }
        out + ")"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "group")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
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
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        let mut out = String::new();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            out += if count != 1 {"; "} else {";"};
            count -= 1;
        }
        out
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "{}", self.loc.file)?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
}
impl TopLevelAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {TopLevelAST {loc, vals}}
}
