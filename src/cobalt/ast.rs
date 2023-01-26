use crate::*;
use codespan_reporting::files::Files;
use std::fmt::{Display, Formatter, Result};
#[derive(Default, Clone)]
pub struct TreePrefix(bitvec::vec::BitVec);
impl TreePrefix {
    fn new() -> Self {Self::default()}
    fn push(&mut self, val: bool) -> &mut Self {self.0.push(val); self}
    fn pop(&mut self) -> &mut Self {self.0.pop(); self}
}
impl Display for TreePrefix {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for val in self.0.iter() {
            write!(f, "{}", if *val {"    "} else {"│   "})?;
        }
        Ok(())
    }
}
pub trait AST {
    fn loc(&self) -> Location;
    fn is_const(&self) -> bool {false}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type;
    fn codegen<'ctx>(& self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>);
    fn to_code(&self) -> String;
    fn print_impl(&self, f: &mut Formatter, pre: &mut TreePrefix) -> Result;
}
impl Display for dyn AST {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if f.alternate() {
            let files = errors::files::FILES.read().unwrap();
            let (file, slice) = self.loc();
            let start = files.location(file, slice.start).unwrap();
            let end = files.location(file, slice.end).unwrap();
            write!(f, "({0}:{1}:{2}..{0}:{3}:{4})", files.name(file).unwrap(), start.line_number, start.column_number, end.line_number, end.column_number)?;
        }
        let mut pre = TreePrefix::new();
        self.print_impl(f, &mut pre)
    }
}
pub fn print_ast_child(f: &mut Formatter, pre: &mut TreePrefix, ast: &dyn AST, last: bool) -> Result {
    write!(f, "{}{}", pre, if last {"└── "} else {"├── "})?;
    if f.alternate() {
        let files = errors::files::FILES.read().unwrap();
        let (file, slice) = ast.loc();
        let start = files.location(file, slice.start).unwrap();
        let end = files.location(file, slice.end).unwrap();
        write!(f, "({0}:{1}:{2}..{0}:{3}:{4}): ", files.name(file).unwrap(), start.line_number, start.column_number, end.line_number, end.column_number)?;
    }
    pre.push(last);
    let res = ast.print_impl(f, pre);
    pre.pop();
    res
}
pub mod vars;
pub mod groups;
pub mod literals;
pub mod scope;
pub mod misc;
pub mod funcs;
pub mod ops;
pub mod flow;

pub use vars::*;
pub use groups::*;
pub use literals::*;
pub use scope::*;
pub use misc::*;
pub use funcs::*;
pub use ops::*;
pub use flow::*;
