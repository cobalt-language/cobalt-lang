use crate::*;
use either::Either::{self, *};
use inkwell::{builder::Builder, context::Context, module::Module};
use owned_chars::OwnedCharsExt;
use std::cell::{Cell, RefCell};
use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::io::{self, BufRead, Read, Write};
use std::mem::MaybeUninit;
use std::pin::Pin;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u16,
    pub bounds_checks: bool,
    pub prepass: bool,
    pub dbg_mangle: bool,
    pub all_move_metadata: bool,
    pub private_syms: bool,
}
impl Default for Flags {
    fn default() -> Self {
        Flags {
            word_size: std::mem::size_of::<isize>() as u16,
            bounds_checks: true,
            prepass: true,
            dbg_mangle: false,
            all_move_metadata: false,
            private_syms: true,
        }
    }
}
pub struct CompCtx<'src, 'ctx> {
    pub flags: Flags,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub is_const: Cell<bool>,
    pub global: Cell<bool>,
    pub export: Cell<bool>,
    pub prepass: Cell<bool>,
    pub null_type: inkwell::types::BasicTypeEnum<'ctx>,
    pub priority: Counter<i32>,
    pub var_scope: Counter<usize>,
    pub lex_scope: Counter<usize>,
    pub values: RefCell<slab::Slab<Value<'src, 'ctx>>>,
    pub moves: RefCell<(
        HashSet<cfg::Use<'src, 'ctx>>,
        HashSet<cfg::Store<'src, 'ctx>>,
    )>,
    pub nom_info: RefCell<slab::Slab<NominalInfo<'ctx>>>,
    pub nom_stack: RefCell<Vec<NominalInfo<'ctx>>>,
    pub to_drop: RefCell<Vec<Vec<Value<'src, 'ctx>>>>,
    int_types: Cell<MaybeUninit<HashMap<(u16, bool), Symbol<'src, 'ctx>>>>,
    vars: Cell<Option<Pin<Box<VarMap<'src, 'ctx>>>>>,
    name: Cell<MaybeUninit<String>>,
}
impl<'src, 'ctx> CompCtx<'src, 'ctx> {
    pub fn new(ctx: &'ctx Context, name: &str) -> Self {
        Self::with_flags(ctx, name, Flags::default())
    }
    pub fn with_flags(ctx: &'ctx Context, name: &str, flags: Flags) -> Self {
        CompCtx {
            context: ctx,
            module: ctx.create_module(name),
            builder: ctx.create_builder(),
            is_const: Cell::new(false),
            global: Cell::new(false),
            export: Cell::new(false),
            prepass: Cell::new(false),
            null_type: ctx.opaque_struct_type("null").into(),
            priority: i32::MAX.into(),
            var_scope: 0.into(),
            lex_scope: 0.into(),
            values: RefCell::default(),
            moves: RefCell::default(),
            nom_info: RefCell::default(),
            nom_stack: RefCell::default(),
            to_drop: RefCell::default(),
            int_types: Cell::new(MaybeUninit::new(HashMap::new())),
            vars: Cell::new(Some(Box::pin(VarMap::new(Some(
                [
                    (
                        "true",
                        Value::interpreted(
                            ctx.bool_type().const_int(1, false).into(),
                            InterData::Int(1),
                            types::Int::bool(),
                        ),
                    ),
                    (
                        "false",
                        Value::interpreted(
                            ctx.bool_type().const_int(0, false).into(),
                            InterData::Int(0),
                            types::Int::bool(),
                        ),
                    ),
                    ("bool", Value::make_type(types::Int::bool())),
                    ("f16", Value::make_type(types::Float::f16())),
                    ("f32", Value::make_type(types::Float::f32())),
                    ("f64", Value::make_type(types::Float::f64())),
                    ("f128", Value::make_type(types::Float::f128())),
                    (
                        "isize",
                        Value::make_type(types::Int::signed(flags.word_size * 8)),
                    ),
                    (
                        "usize",
                        Value::make_type(types::Int::unsigned(flags.word_size * 8)),
                    ),
                ]
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect::<HashMap<_, _>>()
                .into(),
            ))))),
            name: Cell::new(MaybeUninit::new(".".to_string())),
            flags,
        }
    }
    pub fn with_vars<'a, R, F: FnOnce(&'a mut VarMap<'src, 'ctx>) -> R>(&'a self, f: F) -> R {
        let mut val = self.vars.take().expect("recursive access to VarMap!");
        let out = f(unsafe { std::mem::transmute(val.as_mut().get_mut()) }); // reference stuff
        self.vars.set(Some(val));
        out
    }
    pub fn map_vars<F: FnOnce(Box<VarMap<'src, 'ctx>>) -> Box<VarMap<'src, 'ctx>>>(
        &self,
        f: F,
    ) -> &Self {
        self.vars.set(Some(Pin::new(f(Pin::into_inner(
            self.vars.take().expect("recursive access to VarMap!"),
        )))));
        self
    }
    pub fn map_split_vars<R, F: FnOnce(Box<VarMap<'src, 'ctx>>) -> (Box<VarMap<'src, 'ctx>>, R)>(
        &self,
        f: F,
    ) -> R {
        let (v, out) = f(Pin::into_inner(
            self.vars.take().expect("recursive access to VarMap!"),
        ));
        self.vars.set(Some(Pin::new(v)));
        out
    }
    pub fn is_cfunc(&self, name: &DottedName) -> bool {
        name.ids.len() == 1
            && (name.global
                || unsafe {
                    let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                    let b = base == ".";
                    self.name.set(MaybeUninit::new(base));
                    b
                })
            && matches!(&*name.ids.last().unwrap().0, "main") // this match will become larger if more intrinisic stuff is needed
    }
    pub fn mangle(&self, name: &DottedName) -> String {
        let raw = if name.global {
            format!("{name}")
        } else {
            unsafe {
                let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                let out = format!("{base}{name}");
                self.name.set(MaybeUninit::new(base));
                out
            }
        };
        if self.flags.dbg_mangle {
            raw
        } else {
            std::iter::once("_C".to_string())
                .chain(raw.split('.').skip(1).map(|x| format!("{}{}", x.len(), x)))
                .flat_map(|x| x.into_chars())
                .collect()
        }
    }
    pub fn format(&self, name: &DottedName) -> String {
        (if name.global {
            format!("{name}")
        } else {
            unsafe {
                let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                let out = format!("{base}{name}");
                self.name.set(MaybeUninit::new(base));
                out
            }
        })[1..]
            .to_string()
    }
    pub fn push_scope(&self, name: &DottedName) -> Either<usize, String> {
        unsafe {
            if name.global {
                Right(
                    self.name
                        .replace(MaybeUninit::new(name.to_string()))
                        .assume_init(),
                )
            } else {
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
            Right(old) => self.name.set(MaybeUninit::new(old)),
        }
    }
    pub fn get_int_symbol(&self, size: u16, unsigned: bool) -> &Symbol<'src, 'ctx> {
        unsafe {
            let mut val = self.int_types.replace(MaybeUninit::uninit()).assume_init();
            let out = std::mem::transmute::<&mut _, &'ctx _>(match val.entry((size, unsigned)) {
                Entry::Occupied(x) => x.into_mut(),
                Entry::Vacant(x) => {
                    x.insert(Value::make_type(types::Int::new(size, unsigned)).into())
                }
            });
            self.int_types.set(MaybeUninit::new(val));
            out
        }
    }
    pub fn lookup(&self, name: &str, global: bool) -> Option<&Symbol<'src, 'ctx>> {
        self.with_vars(|v| v.lookup(name, global))
            .or_else(|| match name {
                x if x.as_bytes()[0] == 0x69
                    && x.len() > 1
                    && x[1..].chars().all(char::is_numeric) =>
                {
                    Some(self.get_int_symbol(x[1..].parse().unwrap_or(64), false))
                }
                x if x.as_bytes()[0] == 0x75
                    && x.len() > 1
                    && x[1..].chars().all(char::is_numeric) =>
                {
                    Some(self.get_int_symbol(x[1..].parse().unwrap_or(64), true))
                }
                _ => None,
            })
    }
    pub fn lookup_full(&self, name: &DottedName<'src>) -> Option<Value<'src, 'ctx>> {
        let v = self.lookup(&name.ids.first()?.0, name.global)?;
        if !v.1.init {
            return None;
        }
        let mut v = v.0.clone();
        for name in name.ids[1..].iter() {
            v = match v.data_type.self_kind() {
                types::Module::KIND => {
                    if let Some(InterData::Module(s, i, _)) = v.inter_val {
                        self.with_vars(|v| VarMap::lookup_in_mod((&s, &i), &name.0, v))
                            .and_then(|Symbol(v, d)| if d.init { Some(v) } else { None })?
                            .clone()
                    } else {
                        None?
                    }
                }
                types::TypeData::KIND => {
                    if let Some(InterData::Type(t)) = v.inter_val {
                        t.static_attr(&name.0, self)?
                    } else {
                        None?
                    }
                }
                _ => v.attr(name.clone(), self).ok()?,
            };
        }
        Some(v)
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        self.with_vars(|v| v.save(out))
    }
    pub fn load<R: Read + BufRead>(&self, buf: &mut R) -> io::Result<Vec<Cow<'src, str>>> {
        let mut out = vec![];
        while !buf.fill_buf()?.is_empty() {
            out.append(&mut self.with_vars(|v| v.load(buf, self))?);
        }
        Ok(out)
    }
}
impl Drop for CompCtx<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            self.name.replace(MaybeUninit::uninit()).assume_init_drop();
            self.int_types
                .replace(MaybeUninit::uninit())
                .assume_init_drop();
        }
    }
}
