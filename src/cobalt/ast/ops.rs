use crate::*;
pub struct BinOpAST {
    loc: Location,
    pub op: String,
    pub lhs: Box<dyn AST>,
    pub rhs: Box<dyn AST>
}
impl BinOpAST {
    pub fn new(loc: Location, op: String, lhs: Box<dyn AST>, rhs: Box<dyn AST>) -> Self {BinOpAST {loc, op, lhs, rhs}}
}
impl AST for BinOpAST {
    fn loc(&self) -> Location {(self.loc.0, self.lhs.loc().1.start..self.rhs.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if self.op == "&?" || self.op == "|?" {
            let t = self.rhs.res_type(ctx);
            if t == Type::IntLiteral {return Type::IntLiteral}
            if types::utils::expl_convertible(Type::Int(1, false), t.clone()) {t} else {Type::Null}
        }
        else {types::utils::bin_type(self.lhs.res_type(ctx), self.rhs.res_type(ctx), self.op.as_str())}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match self.op.as_str() {
            "&?" => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                if let Some(inkwell::values::BasicValueEnum::IntValue(val)) = types::utils::expl_convert(self.lhs.loc(), (lhs, None), (Type::Int(1, false), None), ctx).unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
                }).into_value(ctx) {
                    let bb = ctx.builder.get_insert_block().unwrap();
                    let f = bb.get_parent().unwrap();
                    let ab = ctx.context.append_basic_block(f, "active");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(val, ab, mb);
                    ctx.builder.position_at_end(ab);
                    let mut rhs = self.rhs.codegen_errs(ctx, &mut errs);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    if rhs.data_type == Type::IntLiteral {rhs.data_type = Type::Int(64, false);}
                    if let (Some(val), Some(ifv)) = (rhs.value(ctx), types::utils::expl_convert(self.rhs.loc(), (Value::metaval(InterData::Int(0), Type::Int(1, false)), None), (rhs.data_type.clone(), None), ctx).map_err(|e| errs.push(e)).ok().and_then(|v| v.into_value(ctx))) {
                        let llt = val.get_type();
                        let phi = ctx.builder.build_phi(llt, "");
                        phi.add_incoming(&[(&val, ab), (&ifv, bb)]);
                        (Value::compiled(phi.as_basic_value(), rhs.data_type), errs)
                    }
                    else {(Value::null(), errs)}
                }
                else {(Value::null(), errs)}
            },
            "|?" => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                if let Some(inkwell::values::BasicValueEnum::IntValue(val)) = types::utils::expl_convert(self.lhs.loc(), (lhs, None), (Type::Int(1, false), None), ctx).unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
                }).into_value(ctx) {
                    let bb = ctx.builder.get_insert_block().unwrap();
                    let f = bb.get_parent().unwrap();
                    let ab = ctx.context.append_basic_block(f, "active");
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(val, mb, ab);
                    ctx.builder.position_at_end(ab);
                    let mut rhs = self.rhs.codegen_errs(ctx, &mut errs);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                    if rhs.data_type == Type::IntLiteral {rhs.data_type = Type::Int(64, false);}
                    if let (Some(val), Some(ifv)) = (rhs.value(ctx), types::utils::expl_convert(self.rhs.loc(), (Value::metaval(InterData::Int(1), Type::Int(1, false)), None), (rhs.data_type.clone(), None), ctx).map_err(|e| errs.push(e)).ok().and_then(|v| v.into_value(ctx))) {
                        let llt = val.get_type();
                        let phi = ctx.builder.build_phi(llt, "");
                        phi.add_incoming(&[(&val, ab), (&ifv, bb)]);
                        (Value::compiled(phi.as_basic_value(), rhs.data_type), errs)
                    }
                    else {(Value::null(), errs)}
                }
                else {(Value::null(), errs)}
            },
            x => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let rhs = self.rhs.codegen_errs(ctx, &mut errs);
                if lhs.data_type == Type::Error || rhs.data_type == Type::Error {return (Value::error(), errs)}
                (types::utils::bin_op(self.loc.clone(), (lhs, self.lhs.loc()), (rhs, self.rhs.loc()), x, ctx).unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
                }), errs)
            }
        }
    }
    fn to_code(&self) -> String {
        format!("({} {} {})", self.lhs.to_code(), self.op, self.rhs.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "binary op: {}", self.op)?;
        print_ast_child(f, pre, &*self.lhs, false)?;
        print_ast_child(f, pre, &*self.rhs, true)
    }
}
pub struct PostfixAST {
    loc: Location,
    pub op: String,
    pub val: Box<dyn AST>,
}
impl PostfixAST {
    pub fn new(loc: Location, op: String, val: Box<dyn AST>) -> Self {PostfixAST {loc, op, val}}
}
impl AST for PostfixAST {
    fn loc(&self) -> Location {(self.loc.0, self.val.loc().1.start..self.loc.1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        types::utils::post_type(self.val.res_type(ctx), self.op.as_str())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {return (Value::error(), errs)}
        (types::utils::post_op(self.loc.clone(), (v, self.val.loc()), self.op.as_str(), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        }), errs)
    }
    fn to_code(&self) -> String {
        format!("{}{}", self.val.to_code(), self.op)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "postfix op: {}", self.op)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct PrefixAST {
    loc: Location,
    pub op: String,
    pub val: Box<dyn AST>,
}
impl PrefixAST {
    pub fn new(loc: Location, op: String, val: Box<dyn AST>) -> Self {PrefixAST {loc, op, val}}
}
impl AST for PrefixAST {
    fn loc(&self) -> Location {(self.loc.0, self.loc.1.start..self.val.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        types::utils::pre_type(self.val.res_type(ctx), self.op.as_str())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {return (Value::error(), errs)}
        (types::utils::pre_op(self.loc.clone(), (v, self.val.loc()), self.op.as_str(), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        }), errs)
    }
    fn to_code(&self) -> String {
        format!("{}{}", self.op, self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "prefix op: {}", self.op)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct SubAST {
    loc: Location,
    pub target: Box<dyn AST>,
    pub index: Box<dyn AST>,
}
impl SubAST {
    pub fn new(loc: Location, target: Box<dyn AST>, index: Box<dyn AST>) -> Self {SubAST {loc, target, index}}
}
impl AST for SubAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {types::utils::sub_type(self.target.res_type(ctx), self.index.res_type(ctx))}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (target, mut errs) = self.target.codegen(ctx);
        let index = self.index.codegen_errs(ctx, &mut errs);
        if target.data_type == Type::Error || index.data_type == Type::Error {return (Value::error(), errs)}
        (types::utils::subscript((target, self.target.loc()), (index, self.index.loc()), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        }), errs)
    }
    fn to_code(&self) -> String {
        format!("{}[{}]", self.target.to_code(), self.index.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "subscript")?;
        print_ast_child(f, pre, &*self.target, false)?;
        print_ast_child(f, pre, &*self.index, true)
    }
}
pub struct DotAST {
    pub obj: Box<dyn AST>,
    pub name: (String, Location)
}
impl DotAST {
    pub fn new(obj: Box<dyn AST>, name: (String, Location)) -> Self {DotAST {obj, name}}
}
impl AST for DotAST {
    fn loc(&self) -> Location {(self.name.1.0, self.obj.loc().1.start..self.name.1.1.end)}
    fn res_type(&self, ctx: &CompCtx) -> Type {
        match self.obj.res_type(ctx) {
            Type::Module => if let Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, _i)), ..} = self.obj.const_codegen(ctx).0 {s.get(&self.name.0).map_or(Type::Error, |v| v.0.data_type.clone())} else {Type::Error},
            x => types::utils::attr_type(x, &self.name.0)
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        (match self.obj.codegen_errs(ctx, &mut errs) {
            Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, _i)), ..} => s.get(&self.name.0).map_or_else(|| {
                errs.push(Diagnostic::error(self.name.1.clone(), 322, None).note(self.name.1.clone(), format!("variable name is {}", self.name.0)));
                Value::error()
            }, |x| x.0.clone()),
            x => types::utils::attr((x, self.obj.loc()), (&self.name.0, self.name.1.clone()), ctx).unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            })
        }, errs)
    }
    fn to_code(&self) -> String {format!("{}.{}", self.obj.to_code(), self.name.0)}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "attr: {}", self.name.0)?;
        print_ast_child(f, pre, &*self.obj, true)
    }
}