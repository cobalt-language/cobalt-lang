use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
use std::mem::MaybeUninit;
use std::cell::Cell;
use either::Either::{self, *};
pub struct CompCtx<'ctx> {
    pub flags: Flags,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub is_const: Cell<bool>,
    vars: Cell<MaybeUninit<Box<VarMap<'ctx>>>>,
    name: Cell<MaybeUninit<String>>
}
impl<'ctx> CompCtx<'ctx> {
    pub fn new(ctx: &'ctx Context, name: &str) -> Self {
        CompCtx {
            flags: Flags::default(),
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false),
            vars: Cell::new(MaybeUninit::new(Box::default())),
            name: Cell::new(MaybeUninit::new(".".to_string()))
        }
    }
    pub fn with_flags(ctx: &'ctx Context, name: &str, flags: Flags) -> Self {
        CompCtx {
            flags,
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false),
            vars: Cell::new(MaybeUninit::new(Box::default())),
            name: Cell::new(MaybeUninit::new(".".to_string()))
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
    pub fn mangle(&self, name: &DottedName) -> String {
        if name.global {format!("{name}")}
        else {
            unsafe {
                let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                let out = format!("{base}{name}");
                self.name.set(MaybeUninit::new(base.to_string()));
                out
            }
        }
    }
    pub fn push_scope(&self, name: &DottedName) -> Either<usize, String> {
        unsafe {
            if name.global {Right(self.name.replace(MaybeUninit::new(name.to_string())).assume_init())}
            else {
                let mut old = self.name.replace(MaybeUninit::uninit()).assume_init();
                let len = old.len();
                old += &format!("{name}.");
                self.name.set(MaybeUninit::new(old));
                Left(len)
            }
        }
    }
    pub fn restore_scope(&self, old: Either<usize, String>) {
        match old {
            Left(len) => unsafe {
                let mut old = self.name.replace(MaybeUninit::uninit()).assume_init();
                old.truncate(len);
                self.name.set(MaybeUninit::new(old));
            },
            Right(old) => self.name.set(MaybeUninit::new(old))
        }
    }
}
impl<'ctx> Drop for CompCtx<'ctx> {
    fn drop(&mut self) {
        unsafe {
            self.vars.replace(MaybeUninit::uninit()).assume_init_drop();
        }
    }
}
