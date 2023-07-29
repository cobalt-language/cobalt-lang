use inkwell::{context::Context, module::Module, builder::Builder};
use crate::*;
use std::pin::Pin;
use std::mem::MaybeUninit;
use std::cell::{Cell, RefCell};
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::io::{self, Write, Read, BufRead};
use either::Either::{self, *};
use owned_chars::OwnedCharsExt;
pub struct CompCtx<'ctx> {
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
    pub nominals: RefCell<HashMap<String, (Type, bool, HashMap<String, Value<'ctx>>, NominalInfo<'ctx>)>>,
    pub moves: RefCell<Vec<(HashSet<cfg::Use<'ctx>>, HashSet<cfg::Store<'ctx>>)>>,
    int_types: Cell<MaybeUninit<HashMap<(u16, bool), Symbol<'ctx>>>>,
    vars: Cell<Option<Pin<Box<VarMap<'ctx>>>>>,
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
            prepass: Cell::new(false),
            null_type: ctx.opaque_struct_type("null").into(),
            priority: i32::MAX.into(),
            var_scope: 0.into(),
            nominals: RefCell::default(),
            moves: RefCell::default(),
            int_types: Cell::new(MaybeUninit::new(HashMap::new())),
            vars: Cell::new(Some(Box::pin(VarMap::new(Some([
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
            prepass: Cell::new(false),
            null_type: ctx.opaque_struct_type("null").into(),
            priority: i32::MAX.into(),
            var_scope: 0.into(),
            nominals: RefCell::default(),
            moves: RefCell::default(),
            int_types: Cell::new(MaybeUninit::new(HashMap::new())),
            vars: Cell::new(Some(Box::pin(VarMap::new(Some([
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
    pub fn with_vars<'a, R, F: FnOnce(&'a mut VarMap<'ctx>) -> R>(&'a self, f: F) -> R {
        let mut val = self.vars.take().expect("recursive access to VarMap!");
        let out = f(unsafe {std::mem::transmute(val.as_mut().get_mut())}); // reference stuff
        self.vars.set(Some(val));
        out
    }
    pub fn map_vars<F: FnOnce(Box<VarMap<'ctx>>) -> Box<VarMap<'ctx>>>(&self, f: F) -> &Self {
        self.vars.set(Some(Pin::new(f(Pin::into_inner(self.vars.take().expect("recursive access to VarMap!"))))));
        self
    }
    pub fn map_split_vars<R, F: FnOnce(Box<VarMap<'ctx>>) -> (Box<VarMap<'ctx>>, R)>(&self, f: F) -> R {
        let (v, out) = f(Pin::into_inner(self.vars.take().expect("recursive access to VarMap!")));
        self.vars.set(Some(Pin::new(v)));
        out
    }
    pub fn is_cfunc(&self, name: &DottedName) -> bool {
        name.ids.len() == 1 && (name.global || unsafe {
            let base = self.name.replace(MaybeUninit::uninit()).assume_init();
            let b = base == ".";
            self.name.set(MaybeUninit::new(base));
            b
        }) && matches!(name.ids.last().unwrap().0.as_str(), "main") // this match will become larger if more intrinisic stuff is needed
    }
    pub fn mangle(&self, name: &DottedName) -> String {
        let raw = 
            if name.global {format!("{name}")}
            else {
                unsafe {
                    let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                    let out = format!("{base}{name}");
                    self.name.set(MaybeUninit::new(base));
                    out
                }
            };
        if self.flags.dbg_mangle {raw}
        else {
            std::iter::once("_C".to_string()).chain(raw.split('.').skip(1).map(|x| format!("{}{}", x.len(), x))).flat_map(|x| x.into_chars()).collect()
        }
    }
    pub fn format(&self, name: &DottedName) -> String {
        (if name.global {format!("{name}")}
        else {
            unsafe {
                let base = self.name.replace(MaybeUninit::uninit()).assume_init();
                let out = format!("{base}{name}");
                self.name.set(MaybeUninit::new(base));
                out
            }
        })[1..].to_string()
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
    pub fn lookup_full(&self, name: &DottedName) -> Option<Value<'ctx>> {
        let v = self.lookup(&name.ids.first()?.0, name.global)?;
        if !v.1.init {return None}
        let mut v = v.0.clone();
        for name in name.ids[1..].iter() {
            v = match v {
                Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i, _)), ..} => self.with_vars(|v| VarMap::lookup_in_mod((&s, &i), &name.0, v)).and_then(|Symbol(v, d)| if d.init {Some(v)} else {None})?.clone(),
                Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(t)), ..} => {
                    if let Type::Nominal(n) = *t {
                        self.nominals.borrow()[&n].2.get(&name.0)?.clone()
                    }
                    else {
                        return None
                    }
                }
                x => ops::attr((x, unreachable_span()), (&name.0, unreachable_span()), self).ok()?
            };
        }
        Some(v)
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        self.with_vars(|v| v.symbols.values().for_each(|s| if s.1.export {s.0.data_type.export(self)}));
        for (n, (t, e, m, i)) in self.nominals.borrow().iter() {
            if *e {
                out.write_all(n.as_bytes())?;
                out.write_all(&[0])?;
                t.save(out)?;
                i.save(out)?;
                for (n, v) in m.iter() {
                    out.write_all(n.as_bytes())?;
                    out.write_all(&[0])?;
                    v.save(out)?;
                }
                out.write_all(&[0])?;
            }
        }
        out.write_all(&[0])?;
        self.with_vars(|v| v.save(out))
    }
    pub fn load<R: Read + BufRead>(&self, buf: &mut R) -> io::Result<Vec<String>> {
        let mut out = vec![];
        while !buf.fill_buf()?.is_empty() { // stable implementation of BufRead::has_data_left
            loop {
                let mut vec = vec![];
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {vec.pop();}
                if vec.is_empty() {break}
                let name = String::from_utf8(std::mem::take(&mut vec)).expect("Nominal types should be valid UTF-8");
                let t = Type::load(buf)?;
                let i = NominalInfo::load(buf, self)?;
                if self.nominals.borrow().get(&name).map_or(false, |x| x.0.unwrapped(self) == t.unwrapped(self)) {out.push(name.clone())}
                self.nominals.borrow_mut().insert(name.clone(), (t, false, Default::default(), i));
                let mut ms = HashMap::new();
                loop {
                    buf.read_until(0, &mut vec)?;
                    if vec.last() == Some(&0) {vec.pop();}
                    if vec.is_empty() {break}
                    ms.insert(String::from_utf8(std::mem::take(&mut vec)).expect("Nominal types should be valid UTF-8"), Value::load(buf, self)?);
                }
                self.nominals.borrow_mut().get_mut(&name).unwrap().2 = ms;
            }
            out.append(&mut self.with_vars(|v| v.load(buf, self))?);
        }
        Ok(out)
    }
}
impl<'ctx> Drop for CompCtx<'ctx> {
    fn drop(&mut self) {
        unsafe {
            self.name.replace(MaybeUninit::uninit()).assume_init_drop();
            self.int_types.replace(MaybeUninit::uninit()).assume_init_drop();
        }
    }
}
