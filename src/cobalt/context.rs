use std::ops::{Deref, DerefMut};
use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
#[derive(Default)]
pub struct BaseCtx {
    pub flags: Flags
}
pub struct CompCtx<'ctx> {
    pub base: BaseCtx,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>
}
impl<'ctx> CompCtx<'ctx> {
    pub fn new<'a>(ctx: &'ctx Context, name: &'a str) -> Self {
        CompCtx {
            base: BaseCtx::default(),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder()
        }
    }
}
impl Deref for CompCtx<'_> {
    type Target = BaseCtx;
    fn deref(&self) -> &Self::Target {&self.base}
}
impl DerefMut for CompCtx<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {&mut self.base}
}
