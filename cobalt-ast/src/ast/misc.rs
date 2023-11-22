use crate::*;
use cobalt_llvm::inkwell::values::BasicValue;

#[derive(Debug, Clone)]
pub struct CastAST<'src> {
    pub val: BoxedAST<'src>,
    pub target: BoxedAST<'src>,
}
impl<'src> CastAST<'src> {
    // TODO: remove unused parameter
    // _loc is kept for API stability
    pub fn new(_loc: SourceSpan, val: BoxedAST<'src>, target: BoxedAST<'src>) -> Self {
        CastAST { val, target }
    }
}
impl<'src> AST<'src> for CastAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.val.loc(), self.target.loc())
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + self.target.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let val = self.val.codegen(ctx, errs);
        if val.data_type == types::Error::new() {
            return Value::error();
        }
        let oic = ctx.is_const.replace(true);
        let t = self
            .target
            .const_codegen(ctx, errs)
            .into_type(ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                types::Error::new()
            });
        ctx.is_const.set(oic);
        val.expl_convert((t, Some(self.target.loc())), ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                Value::error().with_loc(self.val.loc())
            })
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "cast")?;
        print_ast_child(f, pre, &*self.val, false, file)?;
        print_ast_child(f, pre, &*self.target, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct BitCastAST<'src> {
    loc: SourceSpan,
    pub val: BoxedAST<'src>,
    pub target: BoxedAST<'src>,
}
impl<'src> BitCastAST<'src> {
    pub fn new(loc: SourceSpan, val: BoxedAST<'src>, target: BoxedAST<'src>) -> Self {
        BitCastAST { loc, val, target }
    }
}
impl<'src> AST<'src> for BitCastAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.val.loc(), self.target.loc())
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + self.target.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let mut val = self.val.codegen(ctx, errs);
        if val.data_type == types::Error::new() {
            return Value::error();
        }
        let oic = ctx.is_const.replace(true);
        let t = self
            .target
            .const_codegen(ctx, errs)
            .into_type(ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                types::Error::new()
            });
        ctx.is_const.set(oic);
        let decayed = if let (types::IntLiteral::KIND, SizeType::Static(s)) =
            (val.data_type.kind(), t.size())
        {
            types::Int::signed(u16::try_from(s * 8).unwrap_or(64))
        } else {
            val.data_type.decay()
        };
        val = val.impl_convert((decayed, None), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error().with_loc(self.val.loc())
        });
        match (t.size(), val.data_type.size()) {
            (SizeType::Static(d), SizeType::Static(s)) => {
                if d != s {
                    errs.push(CobaltError::DifferentBitCastSizes {
                        loc: self.loc,
                        from_ty: val.data_type.to_string(),
                        from_sz: s,
                        from_loc: self.val.loc(),
                        to_ty: t.to_string(),
                        to_sz: d,
                        to_loc: self.target.loc(),
                    });
                    return Value::error();
                }
            }
            _ => {
                if t == types::Error::new() || val.data_type == types::Error::new() {
                    errs.push(CobaltError::UnsizedBitCast {
                        loc: self.loc,
                        from_ty: val.data_type.to_string(),
                        from_loc: self.val.loc(),
                        to_ty: t.to_string(),
                        to_loc: self.target.loc(),
                    });
                }
                return Value::error();
            }
        }
        if let (Some(llt), Some(ctval)) = (t.llvm_type(ctx), val.comp_val) {
            let cty = ctval.get_type();
            let bc = if cty == llt {
                ctval
            } else if llt.is_array_type()
                || llt.is_struct_type()
                || cty.is_array_type()
                || cty.is_struct_type()
            {
                ctx.builder.build_load(llt, val.addr(ctx).unwrap(), "")
            } else {
                ctx.builder.build_bitcast(ctval, llt, "")
            };
            cfg::mark_move(
                &val,
                bc.as_instruction_value()
                    .map(From::from)
                    .unwrap_or(cfg::Location::current(ctx).unwrap()),
                ctx,
                self.loc(),
            );
            val.comp_val = Some(bc);
        }
        val.data_type = t;
        val
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "bitcast")?;
        print_ast_child(f, pre, &*self.val, false, file)?;
        print_ast_child(f, pre, &*self.target, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct NullAST {
    loc: SourceSpan,
}
impl NullAST {
    pub fn new(loc: SourceSpan) -> Self {
        NullAST { loc }
    }
}
impl<'src> AST<'src> for NullAST {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        _errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        Value::null()
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "null")
    }
}
#[derive(Debug, Clone)]
pub struct ErrorAST {
    loc: SourceSpan,
}
impl ErrorAST {
    pub fn new(loc: SourceSpan) -> Self {
        ErrorAST { loc }
    }
}
impl<'src> AST<'src> for ErrorAST {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        _errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        Value::error()
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "error")
    }
}
#[derive(Debug, Clone)]
pub struct ErrorTypeAST {
    loc: SourceSpan,
}
impl ErrorTypeAST {
    pub fn new(loc: SourceSpan) -> Self {
        ErrorTypeAST { loc }
    }
}
impl<'src> AST<'src> for ErrorTypeAST {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        _errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        Value::make_type(types::Error::new())
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "error type")
    }
}
#[derive(Debug, Clone)]
pub struct TypeLiteralAST {
    loc: SourceSpan,
}
impl TypeLiteralAST {
    pub fn new(loc: SourceSpan) -> Self {
        TypeLiteralAST { loc }
    }
}
impl<'src> AST<'src> for TypeLiteralAST {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        _errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        Value::make_type(types::TypeData::new())
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "type (literal)")
    }
}
#[derive(Debug, Clone)]
pub struct ParenAST<'src> {
    pub loc: SourceSpan,
    pub base: BoxedAST<'src>,
}
impl<'src> ParenAST<'src> {
    pub fn new(loc: SourceSpan, base: BoxedAST<'src>) -> Self {
        ParenAST { loc, base }
    }
}
impl<'src> AST<'src> for ParenAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.base.nodes() + 1
    }
    fn is_const(&self) -> bool {
        self.base.is_const()
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        self.base.codegen(ctx, errs)
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        self.base.print_impl(f, pre, file)
    }
}
#[derive(Debug, Clone)]
pub struct SymbolAST<'src> {
    pub loc: SourceSpan,
    pub val: Cow<'src, [u8]>,
}
impl<'src> SymbolAST<'src> {
    pub fn new(loc: SourceSpan, val: Cow<'src, [u8]>) -> Self {
        SymbolAST { loc, val }
    }
}
impl<'src> AST<'src> for SymbolAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        _errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        Value::new(None, None, types::Symbol::new_ref(&self.val))
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "symbol: {:?}", bstr::BStr::new(&self.val))
    }
}
