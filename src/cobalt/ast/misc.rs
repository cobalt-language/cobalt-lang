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
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.target.into_type(ctx).0.unwrap_or(Type::Null)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (val, mut errs) = self.val.codegen(ctx);
        let (t, mut es) = self.target.into_type(ctx);
        errs.append(&mut es);
        let t = match t {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name)) => {
                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime) => {
                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                Type::Null
            }
        };
        let err = format!("cannot convert value of type {} to {t}", val.data_type);
        if let Some(val) = types::utils::expl_convert(val, t, ctx) {(val, errs)}
        else {
            errs.push(Error::new(self.loc.clone(), 311, err));
            (Variable::error(), errs)
        }
    }
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
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {(Variable::metaval(InterData::Null, Type::Null), vec![])}
    fn to_code(&self) -> String {
        "null".to_string()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "null")
    }
}
