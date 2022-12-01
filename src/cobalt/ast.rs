use std::any::Any;
use inkwell::values::AnyValueEnum;
use crate::*;
pub trait AST<'loc> {
    fn loc(&self) -> Location<'loc>;
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef;
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef);
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef);
}
pub mod vars;
pub mod flow;
pub mod literals;