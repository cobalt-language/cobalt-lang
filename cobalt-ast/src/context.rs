use crate::types::TYPE_SERIAL_REGISTRY;
use crate::*;
use cobalt_utils::misc::new_lifetime_mut;
use either::Either::{self, *};
use hashbrown::hash_map::{Entry, HashMap};
use hashbrown::HashSet;
use inkwell::{builder::Builder, context::Context, module::Module};
use owned_chars::OwnedCharsExt;
use serde::de::DeserializeSeed;
use std::cell::{Cell, RefCell};
use std::io::{Read, Write};
use std::mem::MaybeUninit;
use std::pin::Pin;

/// Simple number to check if a header is compatible for loading
/// Bump this whenever a breaking change is made to the format
const HEADER_FMT_VERSION: u16 = 0;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u16,
    pub bounds_checks: bool,
    pub prepass: bool,
    pub dbg_mangle: bool,
    pub all_move_metadata: bool,
    pub private_syms: bool,
    pub skip_header_version_check: bool,
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
            skip_header_version_check: false,
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
                    ("null", Value::null()),
                ]
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect::<std::collections::HashMap<_, _>>()
                .into(),
            ))))),
            name: Cell::new(MaybeUninit::new(".".to_string())),
            flags,
        }
    }
    pub fn with_vars<'a, R, F: FnOnce(&'a mut VarMap<'src, 'ctx>) -> R>(&'a self, f: F) -> R {
        let mut val = self.vars.take().expect("recursive access to VarMap!");
        let out = f(unsafe { new_lifetime_mut(val.as_mut().get_mut()) }); // reference stuff
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
            let out = new_lifetime_mut(match val.entry((size, unsigned)) {
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
            v = match v.data_type.kind() {
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
    pub fn save<W: Write>(&self, out: &mut W) -> serde_cbor::Result<()> {
        serde_cbor::to_writer(out, self)
    }
    pub fn load<R: Read>(&self, buf: &mut R) -> serde_cbor::Result<Vec<String>> {
        self.deserialize(&mut serde_cbor::Deserializer::from_reader(buf))
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
struct FnDeserializer<
    T,
    F: FnOnce(&mut dyn erased_serde::Deserializer) -> Result<T, erased_serde::Error>,
>(pub F);
impl<'de, T, F: FnOnce(&mut dyn erased_serde::Deserializer) -> Result<T, erased_serde::Error>>
    DeserializeSeed<'de> for FnDeserializer<T, F>
{
    type Value = T;
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        (self.0)(&mut <dyn erased_serde::Deserializer>::erase(deserializer))
            .map_err(de::Error::custom)
    }
}

struct CtxTypeSerde<'a, 's, 'c>(&'a CompCtx<'s, 'c>);
impl Serialize for CtxTypeSerde<'_, '_, '_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ser::*;
        let tsr = TYPE_SERIAL_REGISTRY.pin();
        let mut map = serializer.serialize_map(Some(tsr.len()))?;
        for (id, info) in &tsr {
            map.serialize_entry(id, &(info.erased_header)())?;
        }
        map.end()
    }
}
impl<'de> de::Visitor<'de> for CtxTypeSerde<'_, '_, '_> {
    type Value = ();
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a map of type headers")
    }
    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let tsr = TYPE_SERIAL_REGISTRY.pin();
        while let Some(id) = map.next_key::<u64>()? {
            let Some(loader) = tsr.get(&id) else {
                return Err(de::Error::custom("unknown type ID {:0>16x}"));
            };
            map.next_value_seed(FnDeserializer(loader.load_header))?;
        }
        Ok(())
    }
}
impl<'de> DeserializeSeed<'de> for CtxTypeSerde<'_, '_, '_> {
    type Value = ();
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}
impl Serialize for CompCtx<'_, '_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ser::*;
        let mut map = serializer.serialize_struct("Context", 3)?;
        map.serialize_field("version", &HEADER_FMT_VERSION)?;
        map.serialize_field("types", &CtxTypeSerde(self))?;
        self.with_vars(|v| map.serialize_field("vars", v))?;
        map.end()
    }
}
#[derive(Deserialize)]
#[serde(bound = "'a: 'de")]
struct ContextDeProxy<'a> {
    version: u16,
    #[serde(borrow = "'a")]
    types: serde::__private::de::Content<'a>,
    #[serde(borrow = "'a")]
    vars: serde::__private::de::Content<'a>,
}
impl<'de> DeserializeSeed<'de> for &CompCtx<'_, '_> {
    type Value = Vec<String>;
    fn deserialize<D>(mut self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        use de::*;
        let proxy = ContextDeProxy::deserialize(deserializer)?;
        if proxy.version != HEADER_FMT_VERSION {
            return Err(D::Error::custom(format!("this header was saved with version {}, but version {HEADER_FMT_VERSION} is expected", proxy.version)));
        }
        CtxTypeSerde(self)
            .deserialize(serde::__private::de::ContentDeserializer::new(proxy.types))?;
        let vars = VarMap::deserialize_state(
            &mut self,
            serde::__private::de::ContentDeserializer::new(proxy.vars),
        )?;
        Ok(self.with_vars(|v| varmap::merge(&mut v.symbols, vars.symbols)))
    }
}
