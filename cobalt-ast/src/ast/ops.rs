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
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.op {
            "&?" => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let cond = ops::expl_convert(
                    self.lhs.loc(),
                    (lhs, None),
                    (Type::Int(1, false), None),
                    ctx,
                )
                .unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
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
                    if rhs.data_type == Type::IntLiteral {
                        rhs.data_type = Type::Int(64, false);
                    }
                    let rdt = rhs.data_type.clone();
                    let (comp_val, inter_val) = if let Ok(rhv) = ops::expl_convert(
                        self.rhs.loc(),
                        (Value::metaval(InterData::Int(0), Type::Int(1, false)), None),
                        (rhs.data_type.clone(), None),
                        ctx,
                    ) {
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
                        match ops::expl_convert(
                            self.rhs.loc(),
                            (rhs, None),
                            (Type::Int(1, false), None),
                            ctx,
                        ) {
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
                let cond = ops::expl_convert(
                    self.lhs.loc(),
                    (lhs, None),
                    (Type::Int(1, false), None),
                    ctx,
                )
                .unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
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
                    if rhs.data_type == Type::IntLiteral {
                        rhs.data_type = Type::Int(64, false);
                    }
                    let rdt = rhs.data_type.clone();
                    let (comp_val, inter_val) = if let Ok(rhv) = ops::expl_convert(
                        self.rhs.loc(),
                        (Value::metaval(InterData::Int(1), Type::Int(1, false)), None),
                        (rhs.data_type.clone(), None),
                        ctx,
                    ) {
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
                        match ops::expl_convert(
                            self.rhs.loc(),
                            (rhs, None),
                            (Type::Int(1, false), None),
                            ctx,
                        ) {
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
                if lhs.data_type == Type::Error || rhs.data_type == Type::Error {
                    return (Value::error(), errs);
                }
                (
                    ops::bin_op(
                        self.loc,
                        (lhs, self.lhs.loc()),
                        (rhs, self.rhs.loc()),
                        x,
                        ctx,
                        true,
                        true,
                    )
                    .unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error()
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
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {
            return (Value::error(), errs);
        }
        (
            ops::post_op(self.loc, (v, self.val.loc()), self.op, ctx, true).unwrap_or_else(|e| {
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
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {
            return (Value::error(), errs);
        }
        (
            ops::pre_op(self.loc, (v, self.val.loc()), self.op, ctx, true).unwrap_or_else(|e| {
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
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (target, mut errs) = self.target.codegen(ctx);
        let index = self.index.codegen_errs(ctx, &mut errs);
        if target.data_type == Type::Error || index.data_type == Type::Error {
            return (Value::error(), errs);
        }
        (
            ops::subscript((target, self.target.loc()), (index, self.index.loc()), ctx)
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
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let r = self.obj.codegen_errs(ctx, &mut errs);
        let v = match r {
            Value {
                data_type: Type::Module,
                inter_val: Some(InterData::Module(s, i, n)),
                ..
            } => {
                let (e, v) = ctx
                    .with_vars(|v| VarMap::lookup_in_mod((&s, &i), &self.name.0, v))
                    .map_or_else(
                        || {
                            (
                                Some(CobaltError::VariableDoesNotExist {
                                    name: self.name.0.clone(),
                                    module: n.into(),
                                    container: "module",
                                    loc: self.name.1,
                                }),
                                Value::error(),
                            )
                        },
                        |Symbol(x, d)| {
                            (
                                if !d.init {
                                    Some(CobaltError::UninitializedGlobal {
                                        name: self.name.0.clone(),
                                        loc: self.name.1,
                                    })
                                } else {
                                    None
                                },
                                x.clone(),
                            )
                        },
                    );
                errs.extend(e);
                v
            }
            Value {
                data_type: Type::TypeData,
                inter_val: Some(InterData::Type(t)),
                ..
            } => {
                if let Type::Nominal(n) = &*t {
                    if let Some(v) = ctx.nominals.borrow()[n].2.get(&*self.name.0) {
                        v.clone()
                    } else {
                        errs.push(CobaltError::VariableDoesNotExist {
                            name: self.name.0.clone(),
                            module: n.clone().into(),
                            container: "type",
                            loc: self.name.1,
                        });
                        Value::error()
                    }
                } else {
                    errs.push(CobaltError::VariableDoesNotExist {
                        name: self.name.0.clone(),
                        module: t.to_string().into(),
                        container: "type",
                        loc: self.name.1,
                    });
                    Value::error()
                }
            }
            x => ops::attr((x, self.obj.loc()), (&self.name.0, self.name.1), ctx).unwrap_or_else(
                |e| {
                    errs.push(e);
                    Value::error()
                },
            ),
        };
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
