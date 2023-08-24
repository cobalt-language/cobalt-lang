use cobalt_llvm::inkwell::values::BasicValue;

use crate::*;
#[derive(Debug, Clone)]
pub struct CastAST<'src> {
    loc: SourceSpan,
    pub val: BoxedAST<'src>,
    pub target: BoxedAST<'src>,
}
impl<'src> CastAST<'src> {
    pub fn new(loc: SourceSpan, val: BoxedAST<'src>, target: BoxedAST<'src>) -> Self {
        CastAST { loc, val, target }
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (val, mut errs) = self.val.codegen(ctx);
        if val.data_type == types::Error::new() {
            return (Value::error(), errs);
        }
        let oic = ctx.is_const.replace(true);
        let t = ops::impl_convert(
            self.target.loc(),
            (self.target.codegen_errs(ctx, &mut errs), None),
            (types::TypeData::new(), None),
            ctx,
        )
        .map_or_else(
            |e| {
                errs.push(e);
                types::Error::new()
            },
            |v| {
                if let Some(InterData::Type(t)) = v.inter_val {
                    *t
                } else {
                    types::Error::new()
                }
            },
        );
        ctx.is_const.set(oic);
        (
            ops::expl_convert(
                self.loc,
                (val, Some(self.val.loc())),
                (t, Some(self.target.loc())),
                ctx,
            )
            .unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            }),
            errs,
        )
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (mut val, mut errs) = self.val.codegen(ctx);
        if val.data_type == types::Error::new() {
            return (Value::error(), errs);
        }
        let oic = ctx.is_const.replace(true);
        let t = ops::impl_convert(
            self.target.loc(),
            (self.target.codegen_errs(ctx, &mut errs), None),
            (types::TypeData::new(), None),
            ctx,
        )
        .map_or_else(
            |e| {
                errs.push(e);
                types::Error::new()
            },
            |v| {
                if let Some(InterData::Type(t)) = v.inter_val {
                    *t
                } else {
                    types::Error::new()
                }
            },
        );
        ctx.is_const.set(oic);
        let decayed = val.data_type.decay();
        val = ops::impl_convert(unreachable_span(), (val, None), (decayed, None), ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
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
                    return (Value::error(), errs);
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
                return (Value::error(), errs);
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
            ops::mark_move(
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
        (val, errs)
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        (Value::null(), vec![])
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        (Value::error(), vec![])
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        (Value::make_type(types::Error::new()), vec![])
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        (Value::make_type(types::TypeData::new()), vec![])
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
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        self.base.codegen(ctx)
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
