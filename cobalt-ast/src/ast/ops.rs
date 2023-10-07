use crate::*;
#[derive(Debug, Clone)]
pub struct BinOpAST<'src> {
    loc: SourceSpan,
    pub op: &'static str,
    pub lhs: BoxedAST<'src>,
    pub rhs: BoxedAST<'src>,
}
impl<'src> BinOpAST<'src> {
    pub fn new(
        loc: SourceSpan,
        op: &'static str,
        lhs: BoxedAST<'src>,
        rhs: BoxedAST<'src>,
    ) -> Self {
        BinOpAST { loc, op, lhs, rhs }
    }
}
impl<'src> AST<'src> for BinOpAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.lhs.loc(), self.rhs.loc())
    }
    fn nodes(&self) -> usize {
        self.lhs.nodes() + self.rhs.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.op {
            "&?" => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let cond = lhs
                    .expl_convert((types::Int::bool(), None), ctx)
                    .unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error().with_loc(self.lhs.loc())
                    });
                if let Some(inkwell::values::BasicValueEnum::IntValue(val)) = cond.value(ctx) {
                    let bb = ctx.builder.get_insert_block().unwrap();
                    let f = bb.get_parent().unwrap();
                    let ab = ctx.context.append_basic_block(f, "active");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(val, ab, mb);
                    ctx.builder.position_at_end(ab);
                    let mut rhs = self.rhs.codegen_errs(ctx, &mut errs);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    if rhs.data_type.kind() == types::IntLiteral::KIND {
                        rhs.data_type = types::Int::unsigned(64);
                    }
                    let rdt = rhs.data_type;
                    let (comp_val, inter_val) = if let Ok(rhv) =
                        Value::metaval(InterData::Int(0), types::Int::bool())
                            .expl_convert((types::Int::bool(), None), ctx)
                    {
                        (
                            if let (Some(val), Some(ifv)) = (rhs.value(ctx), rhv.value(ctx)) {
                                let llt = val.get_type();
                                let phi = ctx.builder.build_phi(llt, "");
                                phi.add_incoming(&[(&val, ab), (&ifv, bb)]);
                                Some(phi.as_basic_value())
                            } else {
                                None
                            },
                            if let Some(InterData::Int(v)) = cond.inter_val {
                                if v == 0 {
                                    rhs.inter_val
                                } else {
                                    rhv.inter_val
                                }
                            } else {
                                None
                            },
                        )
                    } else {
                        match rhs.expl_convert((types::Int::bool(), None), ctx) {
                            Ok(rhv) => (
                                if let Some(val) = rhv.value(ctx) {
                                    let llt = ctx.context.bool_type();
                                    let phi = ctx.builder.build_phi(llt, "");
                                    phi.add_incoming(&[(&val, ab), (&llt.const_zero(), bb)]);
                                    Some(phi.as_basic_value())
                                } else {
                                    None
                                },
                                if let Some(InterData::Int(v)) = cond.inter_val {
                                    if v == 0 {
                                        Some(InterData::Int(0))
                                    } else {
                                        rhv.inter_val
                                    }
                                } else {
                                    None
                                },
                            ),
                            Err(err) => {
                                errs.push(err);
                                (None, None)
                            }
                        }
                    };
                    (Value::new(comp_val, inter_val, rdt), errs)
                } else {
                    (Value::null(), errs)
                }
            }
            "|?" => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let cond = lhs
                    .expl_convert((types::Int::bool(), None), ctx)
                    .unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error().with_loc(self.lhs.loc())
                    });
                if let Some(inkwell::values::BasicValueEnum::IntValue(val)) = cond.value(ctx) {
                    let bb = ctx.builder.get_insert_block().unwrap();
                    let f = bb.get_parent().unwrap();
                    let ab = ctx.context.append_basic_block(f, "active");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(val, mb, ab);
                    ctx.builder.position_at_end(ab);
                    let mut rhs = self.rhs.codegen_errs(ctx, &mut errs);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    if rhs.data_type.kind() == types::IntLiteral::KIND {
                        rhs.data_type = types::Int::signed(64);
                    }
                    let rdt = rhs.data_type;
                    let (comp_val, inter_val) = if let Ok(rhv) =
                        Value::metaval(InterData::Int(1), types::Int::bool())
                            .expl_convert((types::Int::bool(), None), ctx)
                    {
                        (
                            if let (Some(val), Some(ifv)) = (rhs.value(ctx), rhv.value(ctx)) {
                                let llt = val.get_type();
                                let phi = ctx.builder.build_phi(llt, "");
                                phi.add_incoming(&[(&val, ab), (&ifv, bb)]);
                                Some(phi.as_basic_value())
                            } else {
                                None
                            },
                            if let Some(InterData::Int(v)) = cond.inter_val {
                                if v != 0 {
                                    rhs.inter_val
                                } else {
                                    rhv.inter_val
                                }
                            } else {
                                None
                            },
                        )
                    } else {
                        match rhs.expl_convert((types::Int::bool(), None), ctx) {
                            Ok(rhv) => (
                                if let Some(val) = rhv.value(ctx) {
                                    let llt = ctx.context.bool_type();
                                    let phi = ctx.builder.build_phi(llt, "");
                                    phi.add_incoming(&[(&val, ab), (&llt.const_int(1, false), bb)]);
                                    Some(phi.as_basic_value())
                                } else {
                                    None
                                },
                                if let Some(InterData::Int(v)) = cond.inter_val {
                                    if v != 0 {
                                        Some(InterData::Int(1))
                                    } else {
                                        rhv.inter_val
                                    }
                                } else {
                                    None
                                },
                            ),
                            Err(err) => {
                                errs.push(err);
                                (None, None)
                            }
                        }
                    };
                    (Value::new(comp_val, inter_val, rdt), errs)
                } else {
                    (Value::null(), errs)
                }
            }
            x => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let rhs = self.rhs.codegen_errs(ctx, &mut errs);
                if lhs.data_type == types::Error::new() || rhs.data_type == types::Error::new() {
                    return (Value::error(), errs);
                }
                (
                    lhs.bin_op((x, self.loc), rhs, ctx).unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error().with_loc(merge_spans(self.lhs.loc(), self.rhs.loc()))
                    }),
                    errs,
                )
            }
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "binary op: {}", self.op)?;
        print_ast_child(f, pre, &*self.lhs, false, file)?;
        print_ast_child(f, pre, &*self.rhs, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct PostfixAST<'src> {
    loc: SourceSpan,
    pub op: &'static str,
    pub val: BoxedAST<'src>,
}
impl<'src> PostfixAST<'src> {
    pub fn new(loc: SourceSpan, op: &'static str, val: BoxedAST<'src>) -> Self {
        PostfixAST { loc, op, val }
    }
}
impl<'src> AST<'src> for PostfixAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.val.loc(), self.loc)
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == types::Error::new() {
            return (Value::error(), errs);
        }
        (
            v.post_op((self.op, self.loc), ctx).unwrap_or_else(|e| {
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
        writeln!(f, "postfix op: {}", self.op)?;
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct PrefixAST<'src> {
    loc: SourceSpan,
    pub op: &'static str,
    pub val: BoxedAST<'src>,
}
impl<'src> PrefixAST<'src> {
    pub fn new(loc: SourceSpan, op: &'static str, val: BoxedAST<'src>) -> Self {
        PrefixAST { loc, op, val }
    }
}
impl<'src> AST<'src> for PrefixAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.loc, self.val.loc())
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == types::Error::new() {
            return (Value::error(), errs);
        }
        (
            if self.op == "&" {
                if let Some(ty) = v.data_type.downcast::<types::Reference>() {
                    Value {
                        data_type: types::Pointer::new(ty.base()),
                        ..v
                    }
                } else {
                    Value::new(
                        v.addr(ctx).map(From::from),
                        None,
                        v.data_type.add_ref(v.frozen.is_none()),
                    )
                }
            } else {
                v.pre_op((self.op, self.loc), ctx).unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
                })
            },
            errs,
        )
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "prefix op: {}", self.op)?;
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct SubAST<'src> {
    loc: SourceSpan,
    pub target: BoxedAST<'src>,
    pub index: BoxedAST<'src>,
}
impl<'src> SubAST<'src> {
    pub fn new(loc: SourceSpan, target: BoxedAST<'src>, index: BoxedAST<'src>) -> Self {
        SubAST { loc, target, index }
    }
}
impl<'src> AST<'src> for SubAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.target.nodes() + self.index.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (target, mut errs) = self.target.codegen(ctx);
        let index = self.index.codegen_errs(ctx, &mut errs);
        if target.data_type == types::Error::new() || index.data_type == types::Error::new() {
            return (Value::error(), errs);
        }
        (
            target.subscript(index, ctx).unwrap_or_else(|e| {
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
        writeln!(f, "subscript")?;
        print_ast_child(f, pre, &*self.target, false, file)?;
        print_ast_child(f, pre, &*self.index, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct DotAST<'src> {
    pub obj: BoxedAST<'src>,
    pub name: (Cow<'src, str>, SourceSpan),
}
impl<'src> DotAST<'src> {
    pub fn new(obj: BoxedAST<'src>, name: (Cow<'src, str>, SourceSpan)) -> Self {
        DotAST { obj, name }
    }
}
impl<'src> AST<'src> for DotAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.obj.loc(), self.name.1)
    }
    fn nodes(&self) -> usize {
        self.obj.nodes() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let v = self
            .obj
            .codegen_errs(ctx, &mut errs)
            .attr(self.name.clone(), ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            });
        (v, errs)
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "attr: {}", self.name.0)?;
        print_ast_child(f, pre, &*self.obj, true, file)
    }
}
