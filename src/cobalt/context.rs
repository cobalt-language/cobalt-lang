use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
use std::mem::MaybeUninit;
use std::cell::Cell;
use std::collections::hash_map::{HashMap, Entry};
use either::Either::{self, *};
pub struct CompCtx<'ctx> {
    pub flags: Flags,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub is_const: Cell<bool>,
    pub global: Cell<bool>,
    pub export: Cell<bool>,
    pub null_type: inkwell::types::BasicTypeEnum<'ctx>,
    pub priority: Counter,
    int_types: Cell<MaybeUninit<HashMap<(u16, bool), Symbol<'ctx>>>>,
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
            global: Cell::new(false),
            export: Cell::new(false),
            null_type: ctx.opaque_struct_type("null").into(),
            priority: Counter::max(),
            int_types: Cell::new(MaybeUninit::new(HashMap::new())),
            vars: Cell::new(MaybeUninit::new(Box::new(VarMap::new(Some([
                ("true", Value::interpreted(ctx.bool_type().const_int(1, false).into(), InterData::Int(1), Type::Int(1, false)).into()),
                ("false", Value::interpreted(ctx.bool_type().const_int(0, false).into(), InterData::Int(0), Type::Int(1, false)).into()),
                ("bool", Value::make_type(Type::Int(1, false)).into()),
                ("f16", Value::make_type(Type::Float16).into()),
                ("f32", Value::make_type(Type::Float32).into()),
                ("f64", Value::make_type(Type::Float64).into()),
                ("f128", Value::make_type(Type::Float128).into()),
                ("isize", Value::make_type(Type::Int(std::mem::size_of::<isize>() as u16 * 8, false)).into()),
                ("usize", Value::make_type(Type::Int(std::mem::size_of::<isize>() as u16 * 8, true)).into()),
            ].into_iter().map(|(k, v)| (k.to_string(), v)).collect::<HashMap<_, _>>().into()))))),
            name: Cell::new(MaybeUninit::new(".".to_string()))
        }
    }
    pub fn with_flags(ctx: &'ctx Context, name: &str, flags: Flags) -> Self {
        CompCtx {
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false),
            global: Cell::new(false),
            export: Cell::new(false),
            null_type: ctx.opaque_struct_type("null").into(),
            priority: Counter::max(),
            int_types: Cell::new(MaybeUninit::new(HashMap::new())),
            vars: Cell::new(MaybeUninit::new(Box::new(VarMap::new(Some([
                ("true", Value::interpreted(ctx.bool_type().const_int(1, false).into(), InterData::Int(1), Type::Int(1, false)).into()),
                ("false", Value::interpreted(ctx.bool_type().const_int(0, false).into(), InterData::Int(0), Type::Int(1, false)).into()),
                ("bool", Value::make_type(Type::Int(1, false)).into()),
                ("f16", Value::make_type(Type::Float16).into()),
                ("f32", Value::make_type(Type::Float32).into()),
                ("f64", Value::make_type(Type::Float64).into()),
                ("f128", Value::make_type(Type::Float128).into()),
                ("isize", Value::make_type(Type::Int(flags.word_size * 8, false)).into()),
                ("usize", Value::make_type(Type::Int(flags.word_size * 8, true)).into()),
            ].into_iter().map(|(k, v)| (k.to_string(), v)).collect::<HashMap<_, _>>().into()))))),
            name: Cell::new(MaybeUninit::new(".".to_string())),
            flags
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
    pub fn map_split_vars<R, F: FnOnce(Box<VarMap<'ctx>>) -> (Box<VarMap<'ctx>>, R)>(&self, f: F) -> R {
        let val = self.vars.replace(MaybeUninit::uninit());
        let (v, out) = unsafe {f(val.assume_init())};
        self.vars.set(MaybeUninit::new(v));
        out
    }
    pub fn mangle(&self, name: &DottedName) -> String {
        if name.global {format!("{name}")}
        else {
            unsafe {
                let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                let out = format!("{base}{name}");
                self.name.set(MaybeUninit::new(base));
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
    pub fn get_int_symbol(&self, size: u16, unsigned: bool) -> &Symbol<'ctx> {
        unsafe {
            let mut val = self.int_types.replace(MaybeUninit::uninit()).assume_init();
            let out = std::mem::transmute::<&mut _, &'ctx _>(match val.entry((size, unsigned)) {
                Entry::Occupied(x) => x.into_mut(),
                Entry::Vacant(x) => x.insert(Value::make_type(Type::Int(size, unsigned)).into())
            });
            self.int_types.set(MaybeUninit::new(val));
            out
        }
    }
    pub fn lookup(&self, name: &str, global: bool) -> Option<&Symbol<'ctx>> {
        self.with_vars(|v| v.lookup(name, global)).or_else(|| match name {
            x if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Some(self.get_int_symbol(x[1..].parse().unwrap_or(64), false)),
            x if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Some(self.get_int_symbol(x[1..].parse().unwrap_or(64), true)),
            _ => None
        })
    }
}
impl<'ctx> Drop for CompCtx<'ctx> {
    fn drop(&mut self) {
        unsafe {
            self.vars.replace(MaybeUninit::uninit()).assume_init_drop();
            self.name.replace(MaybeUninit::uninit()).assume_init_drop();
            self.int_types.replace(MaybeUninit::uninit()).assume_init_drop();
        }
    }
}
