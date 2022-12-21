use crate::*;
pub struct BlockAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for BlockAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.vals.last().map(|x| x.res_type(ctx)).unwrap_or(Type::Null)}
    fn codegen<'ctx>(&'ctx self, ctx: &'ctx CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
        let mut out = Variable::metaval(InterData::Null, Type::Null);
        let mut errs = vec![];
        for val in self.vals.iter() {
            let (ast, mut es) = val.codegen(ctx);
            out = ast;
            errs.append(&mut es);
        }
        ctx.map_vars(|v| v.parent.unwrap());
        (out, errs)
    }
    fn to_code(&self) -> String {
        let mut out = '{'.to_string();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if count != 1 {out += "; ";}
            count -= 1;
        }
        out + "}"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "block")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
}
impl BlockAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {BlockAST {loc, vals}}
}
pub struct GroupAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for GroupAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.vals.last().map(|x| x.res_type(ctx)).unwrap_or(Type::Null)}
    fn codegen<'ctx>(&'ctx self, ctx: &'ctx CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let mut out = Variable::metaval(InterData::Null, Type::Null);
        let mut errs = vec![];
        for val in self.vals.iter() {
            let (ast, mut es) = val.codegen(ctx);
            out = ast;
            errs.append(&mut es);
        }
        (out, errs)
    }
    fn to_code(&self) -> String {
        let mut out = '('.to_string();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if count != 1 {out += "; ";}
            count -= 1;
        }
        out + ")"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "group")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
}
impl GroupAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {GroupAST {loc, vals}}
}
pub struct TopLevelAST {
    loc: Location,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for TopLevelAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&'ctx self, ctx: &'ctx CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let mut errs = vec![];
        for val in self.vals.iter() {
            let mut es = val.codegen(ctx).1;
            errs.append(&mut es);
        }
        (Variable::metaval(InterData::Null, Type::Null), errs)
    }
    fn to_code(&self) -> String {
        let mut out = String::new();
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            out += if count != 1 {"; "} else {";"};
            count -= 1;
        }
        out
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "{}", self.loc.file)?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1)?;
            count -= 1;
        }
        Ok(())
    }
}
impl TopLevelAST {
    pub fn new(loc: Location, vals: Vec<Box<dyn AST>>) -> Self {TopLevelAST {loc, vals}}
}
