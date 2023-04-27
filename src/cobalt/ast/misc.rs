use crate::*;
#[derive(Debug, Clone)]
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
    fn expl_type<'ctx>(&self, _: &CompCtx<'ctx>) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {if let Some(InterData::Type(ty)) = types::utils::impl_convert(Default::default(), (self.target.const_codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(|t| t.inter_val) {*ty} else {Type::Error}}
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
#[derive(Debug, Clone)]
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
    fn expl_type<'ctx>(&self, _: &CompCtx<'ctx>) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {if let Some(InterData::Type(ty)) = types::utils::impl_convert(Default::default(), (self.target.const_codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(|t| t.inter_val) {*ty} else {Type::Error}}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (mut val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let oic = ctx.is_const.replace(true);
        let t = types::utils::impl_convert(self.target.loc(), (self.target.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        ctx.is_const.set(oic);
        match (t.size(ctx), val.data_type.size(ctx)) {
            (SizeType::Static(d), SizeType::Static(s)) => {
                if d != s {
                    errs.push(Diagnostic::error(self.loc.clone(), 317, None).note(self.val.loc(), format!("source type is {}, which has a size of {}", val.data_type, val.data_type.size(ctx))).note(self.target.loc(), format!("target type is {t}, which has a size of {}", t.size(ctx))));
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
#[derive(Debug, Clone)]
pub struct NullAST {
    loc: Location
}
impl NullAST {
    pub fn new(loc: Location) -> Self {NullAST {loc}}
}
impl AST for NullAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::Null}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {(Value::null(), vec![])}
    fn to_code(&self) -> String {"null".to_string()}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {writeln!(f, "null")}
}
#[derive(Debug, Clone)]
pub struct ErrorTypeAST {
    loc: Location
}
impl ErrorTypeAST {
    pub fn new(loc: Location) -> Self {ErrorTypeAST {loc}}
}
impl AST for ErrorTypeAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::TypeData}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {(Value::make_type(Type::Error), vec![])}
    fn to_code(&self) -> String {"<error type>".to_string()}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {writeln!(f, "error type")}
}
#[derive(Debug, Clone)]
pub struct TypeLiteralAST {
    loc: Location
}
impl TypeLiteralAST {
    pub fn new(loc: Location) -> Self {TypeLiteralAST {loc}}
}
impl AST for TypeLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::TypeData}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {(Value::make_type(Type::TypeData), vec![])}
    fn to_code(&self) -> String {"type".to_string()}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {writeln!(f, "type (literal)")}
}
#[derive(Debug, Clone)]
pub struct ParenAST {
    pub loc: Location,
    pub base: Box<dyn AST>
}
impl ParenAST {
    pub fn new(loc: Location, base: Box<dyn AST>) -> Self {ParenAST {loc, base}}
}
impl AST for ParenAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {self.base.is_const()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.base.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {self.base.codegen(ctx)}
    fn to_code(&self) -> String {format!("({})", self.base.to_code())}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {self.base.print_impl(f, pre)}
}
