use crate::*;
use std::fmt::{Display, Formatter, Result};
#[derive(Default, Clone)]
pub struct TreePrefix(bitvec::vec::BitVec);
impl TreePrefix {
    fn new() -> Self {
        Self::default()
    }
    fn push(&mut self, val: bool) -> &mut Self {
        self.0.push(val);
        self
    }
    fn pop(&mut self) -> &mut Self {
        self.0.pop();
        self
    }
}
impl Display for TreePrefix {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for val in self.0.iter() {
            write!(f, "{}", if *val { "    " } else { "│   " })?;
        }
        Ok(())
    }
}
pub trait ASTClone<'src> {
    fn clone_ast(&self) -> Box<DynAST<'src>>;
}
impl<'src, T: AST<'src> + Clone + 'src> ASTClone<'src> for T {
    fn clone_ast(&self) -> Box<DynAST<'src>> {
        Box::new(self.clone())
    }
}
pub trait AST<'src>: ASTClone<'src> + std::fmt::Debug {
    fn loc(&self) -> SourceSpan;
    fn nodes(&self) -> usize {
        1
    }
    // AST properties
    fn is_const(&self) -> bool {
        false
    }
    // pretty printing
    fn print_impl(
        &self,
        f: &mut Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> Result;
    // prepasses
    fn varfwd_prepass(&self, _ctx: &CompCtx<'src, '_>) {} // runs once, inserts uninit symbols with correct names
    fn constinit_prepass(&self, _ctx: &CompCtx<'src, '_>, _needs_another: &mut bool) {} // runs while needs_another is set to true, pretty much only for ConstDefAST
    fn fwddef_prepass(&self, _ctx: &CompCtx<'src, '_>) {} // create forward definitions for functions in LLVM

    // code generation
    /// Implementation point for AST code generation
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>);
    /// Generate code
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let (mut val, errs) = self.codegen_impl(ctx);
        val.loc = self.loc();
        (val, errs)
    }
    /// Generate code in a constant context
    fn const_codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let old_is_const = ctx.is_const.replace(true);
        let res = self.codegen(ctx);
        ctx.is_const.set(old_is_const);
        res
    }

    /// Just calls `codegen()` and appends the errors to `errs`.
    fn codegen_errs<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let (val, mut es) = self.codegen(ctx);
        errs.append(&mut es);
        val
    }
    fn const_codegen_errs<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let old_is_const = ctx.is_const.replace(true);
        let (val, mut es) = self.codegen(ctx);
        errs.append(&mut es);
        ctx.is_const.set(old_is_const);
        val
    }
}
impl Display for dyn AST<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut pre = TreePrefix::new();
        self.print_impl(f, &mut pre, None)
    }
}
impl Clone for BoxedAST<'_> {
    fn clone(&self) -> Self {
        self.clone_ast()
    }
}
pub fn print_ast_child(
    f: &mut Formatter,
    pre: &mut TreePrefix,
    ast: &dyn AST,
    last: bool,
    file: Option<CobaltFile>,
) -> Result {
    write!(f, "{pre}{} ", if last { "└──" } else { "├──" })?;
    if f.alternate() {
        if let Some(Err(e)) = (|| -> Option<Result> {
            let file = file?;
            let slice = ast.loc();
            let (sl, sc) = file.source_loc(slice.offset()).ok()?;
            let (el, ec) = file.source_loc(slice.offset() + slice.len()).ok()?;
            Some(write!(f, "({sl}:{sc}..{el}:{ec}) "))
        })() {
            return Err(e);
        };
    }
    pre.push(last);
    let res = ast.print_impl(f, pre, file);
    pre.pop();
    res
}
pub type DynAST<'src> = dyn AST<'src> + 'src;
pub type BoxedAST<'src> = Box<DynAST<'src>>;

pub mod flow;
pub mod funcs;
pub mod groups;
pub mod literals;
pub mod misc;
pub mod ops;
pub mod scope;
pub mod vars;

pub use flow::*;
pub use funcs::*;
pub use groups::*;
pub use literals::*;
pub use misc::*;
pub use ops::*;
pub use scope::*;
pub use vars::*;
