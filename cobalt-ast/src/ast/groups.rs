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
        ctx.moves.borrow_mut().push(Default::default());
        let mut out = Value::null();
        let mut errs = vec![];
        let start = ctx.builder.get_insert_block().map(|b| b.get_last_instruction().ok_or(b));
        self.vals.iter().for_each(|val| {out = val.codegen_errs(ctx, &mut errs);});
        let end = ctx.builder.get_insert_block().map(|b| b.get_last_instruction().ok_or(b));
        if let (Some(start), Some(end)) = (start, end) {
            let graph = cfg::Cfg::new(either::Either::from(start).flip(), either::Either::from(end).flip(), ctx);
            graph.insert_dtors(ctx, true);
            errs.extend(graph.validate().into_iter().map(|cfg::DoubleMove {name, loc, prev, guaranteed}| CobaltError::DoubleMove {loc, prev, name, guaranteed}));
        }
        ctx.moves.borrow_mut().pop();
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
        self.vals.iter().for_each(|val| {out = val.codegen_errs(ctx, &mut errs);});
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
