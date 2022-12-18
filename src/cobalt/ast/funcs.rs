use crate::*;
pub struct FnDefAST {
    loc: Location,
    pub name: DottedName,
    pub ret: ParsedType,
    pub params: Vec<(String, bool, ParsedType, Option<Box<dyn AST>>)>, // parameter, mutable, type, default
    pub body: Box<dyn AST>
}
impl FnDefAST {
    pub fn new(loc: Location, name: DottedName, ret: ParsedType, params: Vec<(String, bool, ParsedType, Option<Box<dyn AST>>)>, body: Box<dyn AST>) -> Self {FnDefAST {loc, name, ret, params, body}}
}
impl AST for FnDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> Type {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {panic!("code generation has not been implemented")}
    fn to_code(&self) -> String {
        let mut out = format!("fn {}(", self.name);
        let mut len = self.params.len();
        for (param, is_mut, ty, default) in self.params.iter() {
            if *is_mut {out += "mut "}
            out += format!("{}: {}", param, ty).as_str();
            if let Some(val) = default {
                out += format!(" = {}", val.to_code()).as_str();
            }
            if len > 1 {
                out += ", ";
            }
            len -= 1;
        }
        out + format!("): {} = {}", self.ret, self.body.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "function: {}(", self.name)?;
        let mut len = self.params.len(); 
        for (param, is_mut, ty, default) in self.params.iter() {
            if *is_mut {write!(f, "mut ")?;}
            write!(f, "{}: {}", param, ty)?;
            if let Some(val) = default {
                write!(f, " = {}", val.to_code())?;
            }
            if len > 1 {
                write!(f, ", ")?;
            }
            len -= 1;
        }
        writeln!(f, "): {}", self.ret)?;
        print_ast_child(f, pre, &*self.body, true)
    }
}
