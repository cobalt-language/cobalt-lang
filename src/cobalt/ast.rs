use std::any::Any;
use inkwell::values::AnyValueEnum;
use crate::*;
pub trait AST {
    fn loc(&self) -> Location;
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef;
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef);
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef);
}
pub mod vars;
pub mod flow;
pub mod literals;
pub mod scope;

pub use vars::*;
pub use flow::*;
pub use literals::*;
pub use scope::*;