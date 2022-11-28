use inkwell::types::*;
use crate::CompCtx;
pub trait Type {
    fn llvm_type<'ctx>(&self, ctx: CompCtx<'ctx>) -> AnyTypeEnum<'ctx>;
}
pub type TypeRef = &'static dyn Type;
