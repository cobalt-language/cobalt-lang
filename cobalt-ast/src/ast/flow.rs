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
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        if ctx.is_const.get() {
            return Value::null();
        }
        let cond = self.cond.codegen(ctx, errs);
        let cv = cond
            .impl_convert((types::Int::bool(), None), ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                Value::compiled(
                    ctx.context.bool_type().const_int(0, false).into(),
                    types::Int::bool(),
                )
            });
        if let Some(inkwell::values::BasicValueEnum::IntValue(v)) = cv.value(ctx) {
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
                let if_true = self.if_true.codegen(ctx, errs);
                ctx.builder.position_at_end(ifb);
                let if_false = self.if_false.codegen(ctx, errs);
                if let Some(ty) = if_true.data_type.common(if_false.data_type, ctx) {
                    ctx.builder.position_at_end(itb);
                    let if_true = if_true.impl_convert((ty, None), ctx).unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error().with_loc(self.if_true.loc())
                    });
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(ifb);
                    let if_false = if_false.impl_convert((ty, None), ctx).unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error().with_loc(self.if_false.loc())
                    });
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    Value::new(
                        if let Some(llt) = ty.llvm_type(ctx) {
                            let phi = ctx.builder.build_phi(llt, "").unwrap();
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
        } else {
            Value::error()
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
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        if ctx.is_const.get() {
            return Value::null();
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
            let c = self.cond.codegen(ctx, errs);
            let val = c
                .expl_convert((types::Int::bool(), None), ctx)
                .unwrap_or_else(|e| {
                    errs.push(e);
                    Value::compiled(
                        ctx.context.bool_type().const_int(0, false).into(),
                        types::Int::bool(),
                    )
                })
                .value(ctx)
                .unwrap_or(ctx.context.bool_type().const_int(0, false).into());
            ctx.builder
                .build_conditional_branch(val.into_int_value(), body, exit);
            ctx.builder.position_at_end(body);
            self.body.codegen(ctx, errs);
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(exit);
            Value::null()
        } else {
            Value::error()
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
