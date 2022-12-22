use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
use std::mem::MaybeUninit;
use std::cell::Cell;
pub struct CompCtx<'ctx> {
    pub flags: Flags,
    vars: Cell<MaybeUninit<Box<VarMap<'ctx>>>>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub is_const: Cell<bool>
}
impl<'ctx> CompCtx<'ctx> {
    pub fn new(ctx: &'ctx Context, name: &str) -> Self {
        CompCtx {
            flags: Flags::default(),
            vars: Cell::new(MaybeUninit::new(Box::default())),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false)
        }
    }
    pub fn with_flags(ctx: &'ctx Context, name: &str, flags: Flags) -> Self {
        CompCtx {
            flags,
            vars: Cell::new(MaybeUninit::new(Box::default())),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false)
        }
    }
    pub fn with_vars<R, F: FnOnce(&'ctx mut VarMap<'ctx>) -> R>(&self, f: F) -> R {
        let mut val = unsafe {self.vars.replace(MaybeUninit::uninit()).assume_init()};
        let out = f(unsafe {std::mem::transmute::<&mut _, &'ctx mut _>(val.as_mut())});
        self.vars.set(MaybeUninit::new(val));
        out
    }
    pub fn map_vars<F: FnOnce(Box<VarMap<'ctx>>) -> Box<VarMap<'ctx>>>(&self, f: F) -> &Self {
        let val = self.vars.replace(MaybeUninit::uninit());
        self.vars.set(MaybeUninit::new(unsafe {f(val.assume_init())}));
        self
    }
}
impl<'ctx> Drop for CompCtx<'ctx> {
    fn drop(&mut self) {
        unsafe {
            self.vars.replace(MaybeUninit::uninit()).assume_init_drop();
        }
    }
}
