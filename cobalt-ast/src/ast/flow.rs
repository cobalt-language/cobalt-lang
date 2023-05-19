use crate::*;
#[derive(Debug, Clone)]
pub struct IfAST {
    loc: SourceSpan,
    pub cond: Box<dyn AST>,
    pub if_true: Box<dyn AST>,
    pub if_false: Option<Box<dyn AST>>
}
impl IfAST {
    pub fn new(loc: SourceSpan, cond: Box<dyn AST>, if_true: Box<dyn AST>, if_false: Option<Box<dyn AST>>) -> Self {IfAST {loc, cond, if_true, if_false}}
}
impl AST for IfAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Some(val) = self.if_false.as_ref() {types::utils::common(&self.if_true.res_type(ctx), &val.res_type(ctx)).unwrap_or(Type::Null)}
        else {self.if_true.res_type(ctx)}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        if ctx.is_const.get() {return (Value::null(), vec![])}
        let mut errs = vec![];
        let (cond, mut es) = self.cond.codegen(ctx);
        errs.append(&mut es);
        let cv = types::utils::expl_convert(self.cond.loc(), (cond, None), (Type::Int(1, false), None), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::compiled(ctx.context.bool_type().const_int(0, false).into(), Type::Int(1, false))
        });
        if let Some(inkwell::values::BasicValueEnum::IntValue(v)) = cv.value(ctx) {
            (if let Some(if_false) = self.if_false.as_ref() {
                if let Some(f) = ctx.builder.get_insert_block().and_then(|bb| bb.get_parent()) {
                    let itb = ctx.context.append_basic_block(f, "if_true");
                    let ifb = ctx.context.append_basic_block(f, "if_false");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(v, itb, ifb);
                    ctx.builder.position_at_end(itb);
                    let (if_true, mut es) = self.if_true.codegen(ctx);
                    errs.append(&mut es);
                    ctx.builder.position_at_end(ifb);
                    let (if_false, mut es) = if_false.codegen(ctx);
                    errs.append(&mut es);
                    if let Some(ty) = types::utils::common(&if_true.data_type, &if_false.data_type) {
                        ctx.builder.position_at_end(itb);
                        let if_true = types::utils::impl_convert(self.if_true.loc(), (if_true, None), (ty.clone(), None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(ifb);
                        let if_false = types::utils::impl_convert(self.if_false.as_ref().unwrap().loc(), (if_false, None), (ty.clone(), None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(mb);
                        Value::new(
                            if let Some(llt) = ty.llvm_type(ctx) {
                                let phi = ctx.builder.build_phi(llt, "");
                                if let Some(v) = if_true.value(ctx) {phi.add_incoming(&[(&v, itb)]);}
                                if let Some(v) = if_false.value(ctx) {phi.add_incoming(&[(&v, ifb)]);}
                                Some(phi.as_basic_value())
                            } else {None},
                            if let Some(InterData::Int(v)) = cv.inter_val {
                                if v == 0 {if_false.inter_val}
                                else {if_true.inter_val}
                            } else {None},
                            ty
                        )
                    }
                    else {
                        ctx.builder.position_at_end(itb);
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(ifb);
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(mb);
                        Value::null()
                    }
                }
                else {Value::error()}
            }
            else if let Some(ip) = ctx.builder.get_insert_block() {
                if let Some(f) = ip.get_parent() {
                    let itb = ctx.context.append_basic_block(f, "if_true");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(v, itb, mb);
                    ctx.builder.position_at_end(itb);
                    let (if_true, mut es) = self.if_true.codegen(ctx);
                    errs.append(&mut es);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    Value::new(
                        if let Some(llt) = if_true.data_type.llvm_type(ctx) {
                            let phi = ctx.builder.build_phi(llt, "");
                            if let Some(v) = if_true.value(ctx) {phi.add_incoming(&[(&v, itb)]);}
                            phi.add_incoming(&[(&llt.const_zero(), ip)]);
                            Some(phi.as_basic_value())
                        } else {None},
                        if let Some(InterData::Int(v)) = cv.inter_val {
                            if v == 0 {Some(InterData::Null)}
                            else {if_true.inter_val}
                        } else {None},
                        if_true.data_type
                    )
                }
                else {Value::error()}
            }
            else {Value::error()}, errs)
        }
        else {(Value::error(), vec![])}
    }
    fn to_code(&self) -> String {
        if let Some(val) = self.if_false.as_ref() {format!("if ({}) ({}) else ({})", self.cond, self.if_true, val)}
        else {format!("if ({}) ({})", self.cond, self.if_true)}
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        if let Some(val) = self.if_false.as_ref() {
            writeln!(f, "if/else")?;
            print_ast_child(f, pre, &*self.cond, false, file)?;
            print_ast_child(f, pre, &*self.if_true, false, file)?;
            print_ast_child(f, pre, &**val, true, file)
        }
        else {
            writeln!(f, "if")?;
            print_ast_child(f, pre, &*self.cond, false, file)?;
            print_ast_child(f, pre, &*self.if_true, true, file)
        }
    }
}
#[derive(Debug, Clone)]
pub struct WhileAST {
    loc: SourceSpan,
    cond: Box<dyn AST>,
    body: Box<dyn AST>
}
impl WhileAST {
    pub fn new(loc: SourceSpan, cond: Box<dyn AST>, body: Box<dyn AST>) -> Self {WhileAST {loc, cond, body}}
}
impl AST for WhileAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        if ctx.is_const.get() {return (Value::null(), vec![])}
        if let Some(f) = ctx.builder.get_insert_block().and_then(|bb| bb.get_parent()) {
            let cond = ctx.context.append_basic_block(f, "cond");
            let body = ctx.context.append_basic_block(f, "body");
            let exit = ctx.context.append_basic_block(f, "exit");
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(cond);
            let (c, mut errs) = self.cond.codegen(ctx);
            let val = types::utils::expl_convert(self.cond.loc(), (c, None), (Type::Int(1, false), None), ctx).unwrap_or_else(|e| {
                errs.push(e);
                Value::compiled(ctx.context.bool_type().const_int(0, false).into(), Type::Int(1, false))
            }).into_value(ctx).unwrap_or(ctx.context.bool_type().const_int(0, false).into());
            ctx.builder.build_conditional_branch(val.into_int_value(), body, exit);
            ctx.builder.position_at_end(body);
            let (_, mut es) = self.body.codegen(ctx);
            errs.append(&mut es);
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(exit);
            (Value::null(), errs)
        }
        else {(Value::error(), vec![])}
    }
    fn to_code(&self) -> String {
        format!("while ({}) ({})", self.cond, self.body)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "while")?;
        print_ast_child(f, pre, &*self.cond, false, file)?;
        print_ast_child(f, pre, &*self.body, true, file)
    }
}
