use crate::*;
pub struct CastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: Box<dyn AST>
}
impl CastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: Box<dyn AST>) -> Self {CastAST {loc, val, target}}
}
impl AST for CastAST {
    fn loc(&self) -> Location {(self.target.loc().0, self.val.loc().1.start..self.target.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {if let Some(InterData::Type(ty)) = types::utils::impl_convert((0, 0..0), (self.target.const_codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(|t| t.inter_val) {*ty} else {Type::Error}}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let oic = ctx.is_const.replace(true);
        let t = types::utils::impl_convert(self.target.loc(), (self.target.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        ctx.is_const.set(oic);
        (types::utils::expl_convert(self.loc.clone(), (val, Some(self.val.loc())), (t, Some(self.target.loc())), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        }), errs)
    }
    fn to_code(&self) -> String {
        format!("{}: {}", self.val.to_code(), self.target.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "cast")?;
        print_ast_child(f, pre, &*self.val, false)?;
        print_ast_child(f, pre, &*self.target, true)
    }
}
pub struct BitCastAST {
    loc: Location,
    pub val: Box<dyn AST>,
    pub target: Box<dyn AST>
}
impl BitCastAST {
    pub fn new(loc: Location, val: Box<dyn AST>, target: Box<dyn AST>) -> Self {BitCastAST {loc, val, target}}
}
impl AST for BitCastAST {
    fn loc(&self) -> Location {(self.target.loc().0, self.val.loc().1.start..self.target.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {if let Some(InterData::Type(ty)) = types::utils::impl_convert((0, 0..0), (self.target.const_codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(|t| t.inter_val) {*ty} else {Type::Error}}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (mut val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let oic = ctx.is_const.replace(true);
        let t = types::utils::impl_convert(self.target.loc(), (self.target.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        ctx.is_const.set(oic);
        match (t.size(), val.data_type.size()) {
            (SizeType::Static(d), SizeType::Static(s)) => {
                if d != s {
                    errs.push(Diagnostic::error(self.loc.clone(), 317, None).note(self.val.loc(), format!("source type is {}, which has a size of {}", val.data_type, val.data_type.size())).note(self.target.loc(), format!("target type is {t}, which has a size of {}", t.size())));
                    return (Value::error(), errs)
                }
            },
            _ => {
                errs.push(Diagnostic::error(self.loc.clone(), 316, None).note(self.val.loc(), format!("source type is {}", val.data_type)).note(self.target.loc(), format!("target type is {t}")));
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
        format!("{}:? {}", self.val.to_code(), self.target.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "bitcast")?;
        print_ast_child(f, pre, &*self.val, false)?;
        print_ast_child(f, pre, &*self.target, true)
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
