use crate::*;
#[derive(Debug, Clone)]
pub struct IfAST<'src> {
    loc: SourceSpan,
    pub cond: BoxedAST<'src>,
    pub if_true: BoxedAST<'src>,
    pub if_false: BoxedAST<'src>,
}
impl<'src> IfAST<'src> {
    pub fn new(
        loc: SourceSpan,
        cond: BoxedAST<'src>,
        if_true: BoxedAST<'src>,
        if_false: BoxedAST<'src>,
    ) -> Self {
        IfAST {
            loc,
            cond,
            if_true,
            if_false,
        }
    }
}
impl<'src> AST<'src> for IfAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.cond.nodes() + self.if_true.nodes() + self.if_false.nodes() + 1
    }
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        if ctx.is_const.get() {
            return (Value::null(), vec![]);
        }
        let mut errs = vec![];
        let (cond, mut es) = self.cond.codegen(ctx);
        errs.append(&mut es);
        let cv = ops::expl_convert(
            self.cond.loc(),
            (cond, None),
            (Type::Int(1, false), None),
            ctx,
        )
        .unwrap_or_else(|e| {
            errs.push(e);
            Value::compiled(
                ctx.context.bool_type().const_int(0, false).into(),
                Type::Int(1, false),
            )
        });
        if let Some(inkwell::values::BasicValueEnum::IntValue(v)) = cv.value(ctx) {
            (
                {
                    if let Some(f) = ctx
                        .builder
                        .get_insert_block()
                        .and_then(|bb| bb.get_parent())
                    {
                        let itb = ctx.context.append_basic_block(f, "if_true");
                        let ifb = ctx.context.append_basic_block(f, "if_false");
                        let mb = ctx.context.append_basic_block(f, "merge");
                        ctx.builder.build_conditional_branch(v, itb, ifb);
                        ctx.builder.position_at_end(itb);
                        let if_true = self.if_true.codegen_errs(ctx, &mut errs);
                        ctx.builder.position_at_end(ifb);
                        let if_false = self.if_false.codegen_errs(ctx, &mut errs);
                        if let Some(ty) = ops::common(&if_true.data_type, &if_false.data_type, ctx)
                        {
                            ctx.builder.position_at_end(itb);
                            let if_true = ops::impl_convert(
                                self.if_true.loc(),
                                (if_true, None),
                                (ty.clone(), None),
                                ctx,
                            )
                            .unwrap_or_else(|e| {
                                errs.push(e);
                                Value::error()
                            });
                            ctx.builder.build_unconditional_branch(mb);
                            ctx.builder.position_at_end(ifb);
                            let if_false = ops::impl_convert(
                                self.if_false.loc(),
                                (if_false, None),
                                (ty.clone(), None),
                                ctx,
                            )
                            .unwrap_or_else(|e| {
                                errs.push(e);
                                Value::error()
                            });
                            ctx.builder.build_unconditional_branch(mb);
                            ctx.builder.position_at_end(mb);
                            Value::new(
                                if let Some(llt) = ty.llvm_type(ctx) {
                                    let phi = ctx.builder.build_phi(llt, "");
                                    if let Some(v) = if_true.value(ctx) {
                                        phi.add_incoming(&[(&v, itb)]);
                                    }
                                    if let Some(v) = if_false.value(ctx) {
                                        phi.add_incoming(&[(&v, ifb)]);
                                    }
                                    Some(phi.as_basic_value())
                                } else {
                                    None
                                },
                                if let Some(InterData::Int(v)) = cv.inter_val {
                                    if v == 0 {
                                        if_false.inter_val
                                    } else {
                                        if_true.inter_val
                                    }
                                } else {
                                    None
                                },
                                ty,
                            )
                        } else {
                            ctx.builder.position_at_end(itb);
                            ctx.builder.build_unconditional_branch(mb);
                            ctx.builder.position_at_end(ifb);
                            ctx.builder.build_unconditional_branch(mb);
                            ctx.builder.position_at_end(mb);
                            Value::null()
                        }
                    } else {
                        Value::error()
                    }
                },
                errs,
            )
        } else {
            (Value::error(), vec![])
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "if/else")?;
        print_ast_child(f, pre, &*self.cond, false, file)?;
        print_ast_child(f, pre, &*self.if_true, false, file)?;
        print_ast_child(f, pre, &*self.if_false, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct WhileAST<'src> {
    loc: SourceSpan,
    cond: BoxedAST<'src>,
    body: BoxedAST<'src>,
}
impl<'src> WhileAST<'src> {
    pub fn new(loc: SourceSpan, cond: BoxedAST<'src>, body: BoxedAST<'src>) -> Self {
        WhileAST { loc, cond, body }
    }
}
impl<'src> AST<'src> for WhileAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.cond.nodes() + self.body.nodes() + 1
    }
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        if ctx.is_const.get() {
            return (Value::null(), vec![]);
        }
        if let Some(f) = ctx
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_parent())
        {
            let cond = ctx.context.append_basic_block(f, "cond");
            let body = ctx.context.append_basic_block(f, "body");
            let exit = ctx.context.append_basic_block(f, "exit");
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(cond);
            let (c, mut errs) = self.cond.codegen(ctx);
            let val =
                ops::expl_convert(self.cond.loc(), (c, None), (Type::Int(1, false), None), ctx)
                    .unwrap_or_else(|e| {
                        errs.push(e);
                        Value::compiled(
                            ctx.context.bool_type().const_int(0, false).into(),
                            Type::Int(1, false),
                        )
                    })
                    .into_value(ctx)
                    .unwrap_or(ctx.context.bool_type().const_int(0, false).into());
            ctx.builder
                .build_conditional_branch(val.into_int_value(), body, exit);
            ctx.builder.position_at_end(body);
            let (_, mut es) = self.body.codegen(ctx);
            errs.append(&mut es);
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(exit);
            (Value::null(), errs)
        } else {
            (Value::error(), vec![])
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "while")?;
        print_ast_child(f, pre, &*self.cond, false, file)?;
        print_ast_child(f, pre, &*self.body, true, file)
    }
}
