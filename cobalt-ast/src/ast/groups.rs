use crate::*;
#[derive(Debug, Clone)]
pub struct BlockAST {
    loc: SourceSpan,
    pub vals: Vec<Box<dyn AST>>
}
impl BlockAST {
    pub fn new(loc: SourceSpan, vals: Vec<Box<dyn AST>>) -> Self {BlockAST {loc, vals}}
}
impl AST for BlockAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn nodes(&self) -> usize {self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
        // ctx.lex_scope.incr();
        let mut out = Value::null();
        let mut errs = vec![];
        let start = cfg::Location::current(ctx);
        self.vals.iter().for_each(|val| {
            if out.name.is_none() {out.ins_dtor(ctx);}
            out = val.codegen_errs(ctx, &mut errs);
        });
        let end = cfg::Location::current(ctx);
        if let (Some(start), Some(end)) = (start, end) {
            if let Some(loc) = self.vals.last().map(|a| a.loc()) {ops::mark_move(&out, end, ctx, loc);}
            let graph = cfg::Cfg::new(start, end, ctx);
            graph.insert_dtors(ctx, true);
            unsafe {
                let seen = errs.iter()
                    .filter_map(|err| if let CobaltError::DoubleMove {loc, name, ..} = err {Some((*loc, &*(name.as_str() as *const str)))} else {None})
                    .collect::<std::collections::HashSet<_>>();
                errs.extend(graph.validate()
                    .into_iter()
                    .filter(|cfg::DoubleMove {name, loc, ..}| !seen.contains(&(*loc, name.as_str())))
                    .map(|cfg::DoubleMove {name, loc, prev, guaranteed}| CobaltError::DoubleMove {loc, prev, name, guaranteed}));
            }
        }
        // let mut b = ctx.moves.borrow_mut();
        // b.0.retain(|v| v.name.1 < ctx.lex_scope.get());
        // b.1.retain(|v| v.name.1 < ctx.lex_scope.get());
        ctx.map_vars(|v| v.parent.unwrap());
        (out, errs)
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
#[derive(Debug, Clone)]
pub struct GroupAST {
    pub vals: Vec<Box<dyn AST>>
}
impl GroupAST {
    pub fn new(vals: Vec<Box<dyn AST>>) -> Self {
        assert!(!vals.is_empty());
        GroupAST {vals}
    }
}
impl AST for GroupAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.vals[0].loc(), self.vals.last().unwrap().loc())}
    fn nodes(&self) -> usize {self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut out = Value::null();
        let mut errs = vec![];
        self.vals.iter().for_each(|val| {
            if out.name.is_none() {out.ins_dtor(ctx);}
            out = val.codegen_errs(ctx, &mut errs);
        });
        if let (Some(loc), Some(end)) = (self.vals.last().map(|a| a.loc()), cfg::Location::current(ctx)) {ops::mark_move(&out, end, ctx, loc);}
        (out, errs)
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
#[derive(Debug, Clone, Default)]
pub struct TopLevelAST {
    pub file: Option<CobaltFile>,
    pub vals: Vec<Box<dyn AST>>
}
impl AST for TopLevelAST {
    fn loc(&self) -> SourceSpan {unreachable_span()}
    fn nodes(&self) -> usize {self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
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
    pub fn new(vals: Vec<Box<dyn AST>>) -> Self {TopLevelAST {vals, file: None}}
    pub fn run_passes(&self, ctx: &CompCtx) {
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
