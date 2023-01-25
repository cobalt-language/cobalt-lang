use crate::*;
pub struct IfAST {
    loc: Location,
    pub cond: Box<dyn AST>,
    pub if_true: Box<dyn AST>,
    pub if_false: Option<Box<dyn AST>>
}
impl IfAST {
    pub fn new(loc: Location, cond: Box<dyn AST>, if_true: Box<dyn AST>, if_false: Option<Box<dyn AST>>) -> Self {IfAST {loc, cond, if_true, if_false}}
}
impl AST for IfAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Some(val) = self.if_false.as_ref() {types::utils::common(&self.if_true.res_type(ctx), &val.res_type(ctx)).unwrap_or(Type::Null)}
        else {self.if_true.res_type(ctx)}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        if ctx.is_const.get() {return (Variable::null(None), vec![])}
        let mut errs = vec![];
        let (cond, mut es) = self.cond.codegen(ctx);
        errs.append(&mut es);
        let err = format!("cannot convert value of type {} to i1", cond.data_type);
        let v = if let Some(v) = types::utils::expl_convert(cond, Type::Int(1, false), ctx) {v} else {
            errs.push(Diagnostic::error(self.cond.loc(), 312, Some(err)));
            Variable::compiled(ctx.context.custom_width_int_type(1).const_int(0, false).into(), Type::Int(1, false))
        };
        if let Some(inkwell::values::BasicValueEnum::IntValue(v)) = v.comp_val {
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
                    let ty = if let Some(t) = types::utils::common(&if_true.data_type, &if_false.data_type) {t} else {
                        errs.push(Diagnostic::error(self.cond.loc(), 315, Some(format!("no common type for values of types {} and {}", if_true.data_type, if_false.data_type))));
                        Type::Null
                    };
                    ctx.builder.position_at_end(itb);
                    let err = format!("cannot convert value of type {} to {ty}", if_true.data_type);
                    let if_true = if let Some(v) = types::utils::impl_convert(if_true, ty.clone(), ctx) {v} else {
                        errs.push(Diagnostic::error(self.cond.loc(), 311, Some(err)));
                        Variable::error()
                    };
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(ifb);
                    let err = format!("cannot convert value of type {} to {ty}", if_false.data_type);
                    let if_false= if let Some(v) = types::utils::impl_convert(if_false, ty.clone(), ctx) {v} else {
                        errs.push(Diagnostic::error(self.cond.loc(), 311, Some(err)));
                        Variable::error()
                    };
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    if let Some(llt) = ty.llvm_type(ctx) {
                        let phi = ctx.builder.build_phi(llt, "");
                        if let Some(v) = if_true.value(ctx) {phi.add_incoming(&[(&v, itb)]);}
                        if let Some(v) = if_false.value(ctx) {phi.add_incoming(&[(&v, ifb)]);}
                        Variable::compiled(phi.as_basic_value(), ty)
                    }
                    else {Variable::error()}
                }
                else {Variable::error()}
            }
            else {
                if let Some(ip) = ctx.builder.get_insert_block() {
                    if let Some(f) = ip.get_parent() {
                        let itb = ctx.context.append_basic_block(f, "if_true");
                        let mb = ctx.context.append_basic_block(f, "merge");
                        ctx.builder.build_conditional_branch(v, itb, mb);
                        ctx.builder.position_at_end(itb);
                        let (if_true, mut es) = self.if_true.codegen(ctx);
                        errs.append(&mut es);
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(mb);
                        if let Some(llt) = if_true.data_type.llvm_type(ctx) {
                            let phi = ctx.builder.build_phi(llt, "");
                            if let Some(v) = if_true.value(ctx) {phi.add_incoming(&[(&v, itb)]);}
                            phi.add_incoming(&[(&llt.const_zero(), ip)]);
                            Variable::compiled(phi.as_basic_value(), if_true.data_type)
                        }
                        else {Variable::error()}
                    }
                    else {Variable::error()}
                }
                else {Variable::error()}
            }, errs)
        }
        else {(Variable::error(), vec![])}
    }
    fn to_code(&self) -> String {
        if let Some(val) = self.if_false.as_ref() {format!("if ({}) ({}) else ({})", self.cond, self.if_true, val)}
        else {format!("if ({}) ({})", self.cond, self.if_true)}
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        if let Some(val) = self.if_false.as_ref() {
            writeln!(f, "if/else")?;
            print_ast_child(f, pre, &*self.cond, false)?;
            print_ast_child(f, pre, &*self.if_true, false)?;
            print_ast_child(f, pre, &**val, true)
        }
        else {
            writeln!(f, "if")?;
            print_ast_child(f, pre, &*self.cond, false)?;
            print_ast_child(f, pre, &*self.if_true, true)
        }
    }
}
pub struct WhileAST {
    loc: Location,
    cond: Box<dyn AST>,
    body: Box<dyn AST>
}
impl WhileAST {
    pub fn new(loc: Location, cond: Box<dyn AST>, body: Box<dyn AST>) -> Self {WhileAST {loc, cond, body}}
}
impl AST for WhileAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        if ctx.is_const.get() {return (Variable::null(None), vec![])}
        if let Some(f) = ctx.builder.get_insert_block().and_then(|bb| bb.get_parent()) {
            let cond = ctx.context.append_basic_block(f, "cond");
            let body = ctx.context.append_basic_block(f, "body");
            let exit = ctx.context.append_basic_block(f, "exit");
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(cond);
            let (c, mut errs) = self.cond.codegen(ctx);
            let err = format!("cannot convert value of type {} to i1", c.data_type);
            let val = types::utils::expl_convert(c, Type::Int(1, false), ctx).and_then(|v| v.value(ctx)).unwrap_or_else(|| {
                errs.push(Diagnostic::error(self.cond.loc(), 312, Some(err)));
                ctx.context.custom_width_int_type(1).const_int(0, false).into()
            });
            ctx.builder.build_conditional_branch(val.into_int_value(), body, exit);
            ctx.builder.position_at_end(body);
            let (_, mut es) = self.body.codegen(ctx);
            errs.append(&mut es);
            ctx.builder.build_unconditional_branch(cond);
            ctx.builder.position_at_end(exit);
            (Variable::null(None), errs)
        }
        else {(Variable::error(), vec![])}
    }
    fn to_code(&self) -> String {
        format!("while ({}) ({})", self.cond, self.body)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "while")?;
        print_ast_child(f, pre, &*self.cond, false)?;
        print_ast_child(f, pre, &*self.body, true)
    }
}