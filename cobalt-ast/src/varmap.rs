use crate::*;
use std::collections::hash_map::{Entry, HashMap};
use std::num::NonZeroUsize;
#[derive(Debug, Clone, Copy)]
pub enum UndefVariable {
    NotAModule(usize),
    DoesNotExist(usize),
}
#[derive(Debug, Clone)]
pub enum RedefVariable<'src, 'ctx> {
    NotAModule(usize, Symbol<'src, 'ctx>),
    AlreadyExists(usize, Option<SourceSpan>, Symbol<'src, 'ctx>),
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariableData {
    pub good: bool,
    pub export: bool,
    pub init: bool,
    pub fwd: bool,
    pub loc: Option<SourceSpan>,
    pub scope: Option<NonZeroUsize>,
}
impl VariableData {
    pub fn new(loc: SourceSpan) -> Self {
        VariableData {
            good: true,
            export: true,
            init: true,
            fwd: false,
            loc: Some(loc),
            scope: None,
        }
    }
    pub fn with_vis(loc: SourceSpan, export: bool) -> Self {
        VariableData {
            good: true,
            export,
            init: true,
            fwd: false,
            loc: Some(loc),
            scope: None,
        }
    }
    pub fn uninit(loc: SourceSpan) -> Self {
        VariableData {
            good: false,
            export: true,
            init: false,
            fwd: false,
            loc: Some(loc),
            scope: None,
        }
    }
    pub fn uninit_vis(loc: SourceSpan, export: bool) -> Self {
        VariableData {
            good: false,
            export,
            init: false,
            fwd: false,
            loc: Some(loc),
            scope: None,
        }
    }
}
impl Default for VariableData {
    fn default() -> Self {
        VariableData {
            good: true,
            export: true,
            init: true,
            fwd: false,
            loc: None,
            scope: None,
        }
    }
}
impl<'src, 'ctx> From<Value<'src, 'ctx>> for Symbol<'src, 'ctx> {
    fn from(val: Value<'src, 'ctx>) -> Self {
        Symbol(val, VariableData::default())
    }
}
#[derive(Debug, Clone, SerializeState, DeserializeState)]
#[serde(serialize_state = "()")]
#[serde(de_parameters = "'a", deserialize_state = "&'a CompCtx<'src, 'ctx>")]
pub struct Symbol<'src, 'ctx>(
    #[serde(deserialize_state)] pub Value<'src, 'ctx>,
    pub VariableData,
);
impl<'src, 'ctx> Symbol<'src, 'ctx> {
    pub fn into_mod(self) -> Option<(HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>, String)> {
        self.0.into_mod()
    }
    pub fn as_mod(&self) -> Option<(&HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>, &String)> {
        self.0.as_mod()
    }
    pub fn as_mod_mut(
        &mut self,
    ) -> Option<(
        &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &mut String,
    )> {
        self.0.as_mod_mut()
    }
}
impl Serialize for Symbol<'_, '_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize_state(serializer, &())
    }
}
fn skip_serializing_parent(p: &Option<Box<VarMap>>) -> bool {
    p.as_ref().map_or(true, |p| p.parent.is_none())
}
fn serialize_var_symbols<S: Serializer>(
    syms: &HashMap<Cow<str>, Symbol>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    serializer.collect_map(syms.iter().filter(|v| v.1 .1.export))
}

#[derive(Debug, Clone, Default, SerializeState, DeserializeState)]
#[serde(serialize_state = "()")]
#[serde(de_parameters = "'a", deserialize_state = "&'a CompCtx<'src, 'ctx>")]
pub struct VarMap<'src, 'ctx> {
    #[serde(deserialize_state, skip_serializing_if = "skip_serializing_parent")]
    pub parent: Option<Box<Self>>,
    #[serde(deserialize_state, serialize_with = "serialize_var_symbols")]
    pub symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
}
impl<'src, 'ctx> VarMap<'src, 'ctx> {
    pub fn new(parent: Option<Box<Self>>) -> Self {
        VarMap {
            parent,
            ..Self::default()
        }
    }
    pub fn orphan(self) -> Self {
        VarMap {
            parent: None,
            ..self
        }
    }
    pub fn reparent(self, parent: Box<Self>) -> Self {
        VarMap {
            parent: Some(parent),
            ..self
        }
    }
    pub fn root(&self) -> &Self {
        self.parent
            .as_ref()
            .map(|x| if x.parent.is_some() { x.root() } else { self })
            .unwrap_or(self)
    }
    pub fn root_mut(&mut self) -> &mut Self {
        if self.parent.is_some() && self.parent.as_ref().unwrap().parent.is_some() {
            self.parent.as_mut().unwrap().root_mut()
        } else {
            self
        }
    }
    pub fn is_true_root(&self) -> bool {
        self.parent.is_none()
    }
    pub fn is_root(&self) -> bool {
        self.parent.as_ref().map_or(true, |p| p.parent.is_none())
    }
    pub fn insert(
        &mut self,
        name: &DottedName<'src>,
        sym: Symbol<'src, 'ctx>,
    ) -> Result<&Symbol<'src, 'ctx>, RedefVariable<'src, 'ctx>> {
        let mut this = if name.global {
            &mut self.root_mut().symbols
        } else {
            &mut self.symbols
        };
        let mut idx = 0;
        if name.ids.is_empty() {
            panic!("mod_insert cannot insert a value at an empty name")
        }
        while idx + 1 < name.ids.len() {
            if let Some((x, _)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Value::empty_mod(name.start(idx + 1).to_string()).into())
                .0
                .as_mod_mut()
            {
                this = x
            } else {
                return Err(RedefVariable::NotAModule(idx, sym));
            }
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(x) => {
                let data = x.get().1.clone();
                if data.fwd || (data.loc.is_some() && data.loc == sym.1.loc) {
                    let v = x.into_mut();
                    *v = sym;
                    Ok(&*v)
                } else {
                    Err(RedefVariable::AlreadyExists(idx, x.get().1.loc, sym))
                }
            }
            Entry::Vacant(x) => Ok(&*x.insert(sym)),
        }
    }
    pub fn insert_mod(
        &mut self,
        name: &DottedName<'src>,
        sym: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        mod_name: String,
    ) -> Result<(&HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>, &String), RedefVariable<'src, 'ctx>>
    {
        let mut this = if name.global {
            &mut self.root_mut().symbols
        } else {
            &mut self.symbols
        };
        let mut idx = 0;
        if name.ids.is_empty() {
            panic!("mod_insert cannot insert a value at an empty name")
        }
        let mut old = String::new();
        while idx + 1 < name.ids.len() {
            if let Some((x, n)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Value::empty_mod(old + "." + &name.ids[idx].0).into())
                .0
                .as_mod_mut()
            {
                this = x;
                old = n.clone();
            } else {
                return Err(RedefVariable::NotAModule(
                    idx,
                    Value::make_mod(sym, mod_name).into(),
                ));
            }
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => {
                if let Some((m, _)) = x.get_mut().0.as_mod_mut() {
                    *m = sym;
                    Ok(x.into_mut().as_mod().unwrap())
                } else {
                    Err(RedefVariable::AlreadyExists(
                        idx,
                        x.get_mut().1.loc,
                        Value::make_mod(sym, mod_name).into(),
                    ))
                }
            }
            Entry::Vacant(x) => Ok(x
                .insert(Value::make_mod(sym, mod_name).into())
                .as_mod()
                .unwrap()),
        }
    }
    pub fn lookup_mod(
        &mut self,
        name: &DottedName<'src>,
    ) -> Result<(HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>, String), UndefVariable> {
        let mut this = if name.global {
            &mut self.root_mut().symbols
        } else {
            &mut self.symbols
        };
        let mut idx = 0;
        let mut old = String::new();
        if name.ids.is_empty() {
            panic!("mod_lookup_insert cannot find a module at an empty name")
        }
        while idx + 1 < name.ids.len() {
            if let Some((x, n)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Value::empty_mod(old + "." + &name.ids[idx].0).into())
                .as_mod_mut()
            {
                this = x;
                old = n.clone();
            } else {
                return Err(UndefVariable::NotAModule(idx));
            }
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(x) => {
                if x.get().0.data_type.kind() == types::Module::KIND {
                    if let Some(InterData::Module(s, n)) = x.remove().0.inter_val {
                        Ok((s, n))
                    } else {
                        Err(UndefVariable::NotAModule(idx))
                    }
                } else {
                    Err(UndefVariable::DoesNotExist(idx))
                }
            }
            Entry::Vacant(_) => Ok(Default::default()),
        }
    }
    pub fn lookup(&self, name: &str, global: bool) -> Option<&Symbol<'src, 'ctx>> {
        let root = self.root();
        if global {
            root.lookup(name, false)
        } else {
            self.symbols
                .get(name)
                .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name, global)))
        }
    }
}
impl Serialize for VarMap<'_, '_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize_state(serializer, &())
    }
}
impl<'src, 'ctx> From<HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>> for VarMap<'src, 'ctx> {
    fn from(symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>) -> Self {
        VarMap {
            parent: None,
            symbols,
        }
    }
}
impl<'src, 'ctx> From<HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>> for Box<VarMap<'src, 'ctx>> {
    fn from(symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>) -> Self {
        Box::new(VarMap {
            parent: None,
            symbols,
        })
    }
}

pub(crate) fn merge<'src, 'ctx>(
    base: &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
    new: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
) -> Vec<String> {
    let mut out = vec![];
    for (key, val) in new {
        match base.entry(key) {
            Entry::Occupied(mut e) => {
                let l = e.get_mut();
                let r = val;
                const MOD: u64 = types::Module::KIND;
                if let (MOD, MOD, Some(InterData::Module(bs, _)), Some(InterData::Module(ns, _))) = (
                    l.0.data_type.kind(),
                    r.0.data_type.kind(),
                    &mut l.0.inter_val,
                    r.0.inter_val,
                ) {
                    out.append(&mut merge(bs, ns));
                } else {
                    out.push(e.key().clone().into_owned())
                }
            }
            Entry::Vacant(e) => {
                e.insert(val);
            }
        }
    }
    out
}
