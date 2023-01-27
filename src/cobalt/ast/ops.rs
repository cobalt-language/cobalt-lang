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
