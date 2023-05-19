use crate::*;
#[derive(Debug, Clone)]
pub struct BlockAST {
    loc: SourceSpan,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for BlockAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.vals.last().map(|x| x.res_type(ctx)).unwrap_or(Type::Null)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
        let mut out = Value::null();
        let mut errs = vec![];
        self.vals.iter().for_each(|val| {out = val.codegen_errs(ctx, &mut errs);});
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "block")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
impl BlockAST {
    pub fn new(loc: SourceSpan, vals: Vec<Box<dyn AST>>) -> Self {BlockAST {loc, vals}}
}
#[derive(Debug, Clone)]
pub struct GroupAST {
    loc: SourceSpan,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for GroupAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.vals.last().map(|x| x.res_type(ctx)).unwrap_or(Type::Null)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut out = Value::null();
        let mut errs = vec![];
        self.vals.iter().for_each(|val| {out = val.codegen_errs(ctx, &mut errs);});
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "group")?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
impl GroupAST {
    pub fn new(loc: SourceSpan, vals: Vec<Box<dyn AST>>) -> Self {GroupAST {loc, vals}}
}
#[derive(Debug, Clone)]
pub struct TopLevelAST {
    loc: SourceSpan,
    pub file: Option<CobaltFile>,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for TopLevelAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        if ctx.flags.prepass {
            self.vals.iter().for_each(|val| val.varfwd_prepass(ctx));
            let mut again = true;
            while again {
                again = false;
                self.vals.iter().for_each(|val| val.constinit_prepass(ctx, &mut again));
            }
            self.vals.iter().for_each(|val| val.fwddef_prepass(ctx));
        }
        let mut errs = vec![];
        self.vals.iter().for_each(|val| std::mem::drop(val.codegen_errs(ctx, &mut errs)));
        (Value::null(), errs)
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        if let Some(ref file) = self.file {writeln!(f, "{}", file.name())?} else {f.write_str("<file not set>\n")?};
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
impl TopLevelAST {
    pub fn new(loc: SourceSpan, vals: Vec<Box<dyn AST>>) -> Self {TopLevelAST {loc, vals, file: None}}
    pub fn run_passes<'ctx>(&self, ctx: &CompCtx<'ctx>) {
        self.vals.iter().for_each(|val| val.varfwd_prepass(ctx));
        let mut again = true;
        while again {
            again = false;
            self.vals.iter().for_each(|val| val.constinit_prepass(ctx, &mut again));
        }
        self.vals.iter().for_each(|val| val.fwddef_prepass(ctx));
    }
}
impl std::fmt::Display for TopLevelAST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut pre = TreePrefix::new();
        self.print_impl(f, &mut pre, self.file)
    }
}
