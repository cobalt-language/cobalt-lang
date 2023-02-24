use crate::*;
pub struct CastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: ParsedType,
    pub target_loc: Location
}
impl CastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: ParsedType, target_loc: Location) -> Self {CastAST {loc, val, target, target_loc}}
}
impl AST for CastAST {
    fn loc(&self) -> Location {(self.target_loc.0, self.val.loc().1.start..self.target_loc.1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.target.into_type(ctx).0.unwrap_or(Type::Error)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let (t, mut es) = self.target.into_type(ctx);
        errs.append(&mut es);
        let t = match t {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name, loc)) => {
                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                Type::Error
            },
            Err(IntoTypeError::NotCompileTime(loc)) => {
                errs.push(Diagnostic::error(loc, 324, None));
                Type::Error
            },
            Err(IntoTypeError::NotAModule(name, loc)) => {
                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                Type::Error
            },
            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                Type::Error
            }
        };
        let l = format!("source type is {}", val.data_type);
        let r = format!("target type is {t}");
        let err = format!("cannot convert value of type {} to {t}", val.data_type);
        if let Some(val) = types::utils::expl_convert(val, t, ctx) {(val, errs)}
        else {
            errs.push(Diagnostic::error(self.loc.clone(), 312, Some(err)).note(self.val.loc(), l).note(self.target_loc.clone(), r));
            (Value::error(), errs)
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
pub struct BitCastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: ParsedType,
    pub target_loc: Location
}
impl BitCastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: ParsedType, target_loc: Location) -> Self {BitCastAST {loc, val, target, target_loc}}
}
impl AST for BitCastAST {
    fn loc(&self) -> Location {(self.target_loc.0, self.val.loc().1.start..self.target_loc.1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.target.into_type(ctx).0.unwrap_or(Type::Error)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (mut val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let (t, mut es) = self.target.into_type(ctx);
        errs.append(&mut es);
        let t = match t {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name, loc)) => {
                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                Type::Error
            },
            Err(IntoTypeError::NotCompileTime(loc)) => {
                errs.push(Diagnostic::error(loc, 324, None));
                Type::Error
            },
            Err(IntoTypeError::NotAModule(name, loc)) => {
                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                Type::Error
            },
            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                Type::Error
            }
        };
        match (t.size(), val.data_type.size()) {
            (SizeType::Static(d), SizeType::Static(s)) => {
                if d != s {
                    errs.push(Diagnostic::error(self.loc.clone(), 317, None).note(self.val.loc(), format!("source type is {}, which has a size of {}", val.data_type, val.data_type.size())).note(self.target_loc.clone(), format!("target type is {t}, which has a size of {}", t.size())));
                    return (Value::error(), errs)
                }
            },
            _ => {
                errs.push(Diagnostic::error(self.loc.clone(), 316, None).note(self.val.loc(), format!("source type is {}", val.data_type)).note(self.target_loc.clone(), format!("target type is {t}")));
                return (Value::error(), errs)
            }
        }
        if let (Some(llt), Some(ctval)) = (t.llvm_type(ctx), val.comp_val) {
            val.comp_val = Some(ctx.builder.build_bitcast(ctval, llt, ""));
            val.data_type = t;
            (val, errs)
        }
        else {(Value::error(), errs)}
    }
    fn to_code(&self) -> String {
        format!("{}:? {}", self.val.to_code(), self.target)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "bitcast: {}", self.target)?;
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
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {(Value::null(), vec![])}
    fn to_code(&self) -> String {
        "null".to_string()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "null")
    }
}
