use cobalt_llvm::inkwell::values::BasicValue;

use crate::*;
#[derive(Debug, Clone)]
pub struct CastAST {
    loc: SourceSpan,
    pub val: Box<dyn AST>,
    pub target: Box<dyn AST>
}
impl CastAST {
    pub fn new(loc: SourceSpan, val: Box<dyn AST>, target: Box<dyn AST>) -> Self {CastAST {loc, val, target}}
}
impl AST for CastAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.val.loc(), self.target.loc())}
    fn nodes(&self) -> usize {self.val.nodes() + self.target.nodes() + 1}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let (val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let oic = ctx.is_const.replace(true);
        let t = ops::impl_convert(self.target.loc(), (self.target.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        ctx.is_const.set(oic);
        (ops::expl_convert(self.loc, (val, Some(self.val.loc())), (t, Some(self.target.loc())), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        }), errs)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "cast")?;
        print_ast_child(f, pre, &*self.val, false, file)?;
        print_ast_child(f, pre, &*self.target, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct BitCastAST {
    loc: SourceSpan,
    pub val: Box<dyn AST>,
    pub target: Box<dyn AST>
}
impl BitCastAST {
    pub fn new(loc: SourceSpan, val: Box<dyn AST>, target: Box<dyn AST>) -> Self {BitCastAST {loc, val, target}}
}
impl AST for BitCastAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.val.loc(), self.target.loc())}
    fn nodes(&self) -> usize {self.val.nodes() + self.target.nodes() + 1}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let (mut val, mut errs) = self.val.codegen(ctx);
        if val.data_type == Type::Error {return (Value::error(), errs)}
        let oic = ctx.is_const.replace(true);
        let t = ops::impl_convert(self.target.loc(), (self.target.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        ctx.is_const.set(oic);
        let decayed = ops::decay(val.data_type.clone());
        val = ops::impl_convert(unreachable_span(), (val, None), (decayed, None), ctx).unwrap();
        match (t.size(ctx), val.data_type.size(ctx)) {
            (SizeType::Static(d), SizeType::Static(s)) => {
                if d != s {
                    errs.push(CobaltError::DifferentBitCastSizes {
                        loc: self.loc,
                        from_ty: val.data_type.to_string(),
                        from_sz: s,
                        from_loc: self.val.loc(),
                        to_ty: t.to_string(),
                        to_sz: d,
                        to_loc: self.target.loc()
                    });
                    return (Value::error(), errs)
                }
            }
            _ => {
                errs.push(CobaltError::UnsizedBitCast {
                    loc: self.loc,
                    from_ty: val.data_type.to_string(),
                    from_loc: self.val.loc(),
                    to_ty: t.to_string(),
                    to_loc: self.target.loc()
                });
                return (Value::error(), errs)
            }
        }
        if let (Some(llt), Some(ctval)) = (t.llvm_type(ctx), val.comp_val) {
            let bc = ctx.builder.build_bitcast(ctval, llt, "");
            ops::mark_move(&val, bc.as_instruction_value().map(From::from).unwrap_or(cfg::Location::current(ctx).unwrap()), ctx, self.loc());
            val.comp_val = Some(bc);
            val.data_type = t;
            (val, errs)
        }
        else {(Value::error(), errs)}
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "bitcast")?;
        print_ast_child(f, pre, &*self.val, false, file)?;
        print_ast_child(f, pre, &*self.target, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct NullAST {
    loc: SourceSpan
}
impl NullAST {
    pub fn new(loc: SourceSpan) -> Self {NullAST {loc}}
}
impl AST for NullAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {(Value::null(), vec![])}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {writeln!(f, "null")}
}
#[derive(Debug, Clone)]
pub struct ErrorAST {
    loc: SourceSpan
}
impl ErrorAST {
    pub fn new(loc: SourceSpan) -> Self {ErrorAST {loc}}
}
impl AST for ErrorAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {(Value::error(), vec![])}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {writeln!(f, "error")}
}
#[derive(Debug, Clone)]
pub struct ErrorTypeAST {
    loc: SourceSpan
}
impl ErrorTypeAST {
    pub fn new(loc: SourceSpan) -> Self {ErrorTypeAST {loc}}
}
impl AST for ErrorTypeAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {(Value::make_type(Type::Error), vec![])}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {writeln!(f, "error type")}
}
#[derive(Debug, Clone)]
pub struct TypeLiteralAST {
    loc: SourceSpan
}
impl TypeLiteralAST {
    pub fn new(loc: SourceSpan) -> Self {TypeLiteralAST {loc}}
}
impl AST for TypeLiteralAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {(Value::make_type(Type::TypeData), vec![])}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {writeln!(f, "type (literal)")}
}
#[derive(Debug, Clone)]
pub struct ParenAST {
    pub loc: SourceSpan,
    pub base: Box<dyn AST>
}
impl ParenAST {
    pub fn new(loc: SourceSpan, base: Box<dyn AST>) -> Self {ParenAST {loc, base}}
}
impl AST for ParenAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn nodes(&self) -> usize {self.base.nodes() + 1}
    fn is_const(&self) -> bool {self.base.is_const()}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {self.base.codegen(ctx)}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {self.base.print_impl(f, pre, file)}
}
