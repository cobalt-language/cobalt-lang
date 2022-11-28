use std::any::Any;
use inkwell::values::AnyValueEnum;
use crate::*;
pub trait AST<'loc> {
    fn loc(&self) -> Location<'loc>;
    fn res_type(&self, ctx: &BaseCtx) -> TypeRef;
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef);
    fn eval(&self, ctx: &BaseCtx) -> (Box<dyn Any>, TypeRef);
}
