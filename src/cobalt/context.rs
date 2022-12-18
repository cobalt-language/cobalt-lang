use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
pub struct CompCtx<'ctx> {
    pub flags: Flags,
    pub vars: Box<VarMap<'ctx>>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>
}
impl<'ctx> CompCtx<'ctx> {
    pub fn new(ctx: &'ctx Context, name: &str) -> Self {
        CompCtx {
            flags: Flags::default(),
            vars: Box::default(),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder()
        }
    }
    pub fn with_flags(ctx: &'ctx Context, name: &str, flags: Flags) -> Self {
        CompCtx {
            flags,
            vars: Box::default(),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder()
        }
    }
}
