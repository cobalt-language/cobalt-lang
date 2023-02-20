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
        if self.op == "&&" || self.op == "||" {self.rhs.res_type(ctx)}
        else {types::utils::bin_type(self.lhs.res_type(ctx), self.rhs.res_type(ctx), self.op.as_str())}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        match self.op.as_str() {
            "&&" => todo!("short-circuiting operators aren't implemented"),
            "||" => todo!("short-circuiting operators aren't implemented"),
            x => {
                let (lhs, mut errs) = self.lhs.codegen(ctx);
                let (rhs, mut es) = self.rhs.codegen(ctx);
                errs.append(&mut es);
                if lhs.data_type == Type::Error || rhs.data_type == Type::Error {return (Variable::error(), errs)}
                let ln = format!("{}", lhs.data_type);
                let rn = format!("{}", rhs.data_type);
                let val = types::utils::bin_op(lhs, rhs, x, ctx);
                if val.is_none() {
                    errs.push(Diagnostic::error(self.loc.clone(), 310, Some(format!("binary operator {} is not defined for values of types {ln} and {rn}", self.op)))
                        .note(self.lhs.loc(), format!("left value is of type {ln}"))
                        .note(self.rhs.loc(), format!("right value is of type {rn}")));
                }
                (val.unwrap_or_else(Variable::error), errs)
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
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {return (Variable::error(), errs)}
        let n = format!("{}", v.data_type);
        let val = types::utils::post_op(v, self.op.as_str(), ctx);
        if val.is_none() {
            errs.push(Diagnostic::error(self.loc.clone(), 310, Some(format!("postfix operator {} is not defined for value of type {n}", self.op)))
                .note(self.val.loc(), format!("value is of type {n}")));
        }
        (val.unwrap_or_else(Variable::error), errs)
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
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (v, mut errs) = self.val.codegen(ctx);
        if v.data_type == Type::Error {return (Variable::error(), errs)}
        let n = format!("{}", v.data_type);
        let val = types::utils::pre_op(v, self.op.as_str(), ctx);
        if val.is_none() {
            errs.push(Diagnostic::error(self.loc.clone(), 310, Some(format!("prefix operator {} is not defined for value of type {n}", self.op)))
                .note(self.val.loc(), format!("value is of type {n}")));
        }
        (val.unwrap_or_else(Variable::error), errs)
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
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (target, mut errs) = self.target.codegen(ctx);
        let (index, mut es) = self.index.codegen(ctx);
        errs.append(&mut es);
        let t = target.data_type.to_string();
        let i = index.data_type.to_string();
        (types::utils::subscript(target, index, ctx).unwrap_or_else(|| {
            errs.push(Diagnostic::error(self.loc.clone(), 318, None).note(self.target.loc(), format!("target type is {t}")).note(self.index.loc(), format!("index type is {i}")));
            Variable::error()
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
