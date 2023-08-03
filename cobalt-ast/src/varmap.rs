use crate::*;
use std::collections::{
    hash_map::{Entry, HashMap},
    LinkedList,
};
use std::io::{self, BufRead, Read, Write};
use std::num::NonZeroUsize;
#[derive(Debug, Clone, Copy)]
pub enum UndefVariable {
    NotAModule(usize),
    DoesNotExist(usize),
}
#[derive(Debug, Clone)]
pub enum RedefVariable<'ctx> {
    NotAModule(usize, Symbol<'ctx>),
    AlreadyExists(usize, Option<SourceSpan>, Symbol<'ctx>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
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
impl<'ctx> From<Value<'ctx>> for Symbol<'ctx> {
    fn from(val: Value<'ctx>) -> Self {
        Symbol(val, VariableData::default())
    }
}
#[derive(Debug, Clone)]
pub struct Symbol<'ctx>(pub Value<'ctx>, pub VariableData);
impl<'ctx> Symbol<'ctx> {
    pub fn into_mod(
        self,
    ) -> Option<(
        HashMap<String, Symbol<'ctx>>,
        Vec<(CompoundDottedName, bool)>,
        String,
    )> {
        self.0.into_mod()
    }
    pub fn as_mod(
        &self,
    ) -> Option<(
        &HashMap<String, Symbol<'ctx>>,
        &Vec<(CompoundDottedName, bool)>,
        &String,
    )> {
        self.0.as_mod()
    }
    pub fn as_mod_mut(
        &mut self,
    ) -> Option<(
        &mut HashMap<String, Symbol<'ctx>>,
        &mut Vec<(CompoundDottedName, bool)>,
        &mut String,
    )> {
        self.0.as_mod_mut()
    }
    pub fn empty_mod(name: String) -> Self {
        Value::empty_mod(name).into()
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        self.0.save(out)
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        Ok(Symbol(
            Value::load(buf, ctx)?,
            VariableData {
                export: false,
                ..VariableData::default()
            },
        ))
    }
    pub fn dump(&self, depth: usize) {
        match self {
            Symbol(
                Value {
                    data_type: Type::Module,
                    inter_val: Some(InterData::Module(s, i, n)),
                    ..
                },
                _,
            ) => {
                let pre = " ".repeat(depth);
                eprintln!("module {n:?}");
                for (i, _) in i {
                    eprintln!("{pre}    import: {i}")
                }
                for (k, s) in s {
                    eprint!("{pre}    {k:?}: ");
                    s.dump(depth + 4)
                }
            }
            Symbol(Value { data_type: dt, .. }, _) => eprintln!("variable of type {dt}"),
        }
    }
}
#[derive(Default)]
pub struct VarMap<'ctx> {
    pub parent: Option<Box<VarMap<'ctx>>>,
    pub symbols: HashMap<String, Symbol<'ctx>>,
    pub imports: Vec<(CompoundDottedName, bool)>,
}
impl<'ctx> VarMap<'ctx> {
    pub fn new(parent: Option<Box<VarMap<'ctx>>>) -> Self {
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
    pub fn reparent(self, parent: Box<VarMap<'ctx>>) -> Self {
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
    pub fn insert(
        &mut self,
        name: &DottedName,
        sym: Symbol<'ctx>,
    ) -> Result<&Symbol<'ctx>, RedefVariable<'ctx>> {
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
            if let Symbol(
                Value {
                    data_type: Type::Module,
                    inter_val: Some(InterData::Module(x, _, _)),
                    ..
                },
                _,
            ) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Symbol::empty_mod(name.start(idx + 1).to_string()))
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
        name: &DottedName,
        mut sym: (
            HashMap<String, Symbol<'ctx>>,
            Vec<(CompoundDottedName, bool)>,
        ),
        mod_name: String,
    ) -> Result<
        (
            &HashMap<String, Symbol<'ctx>>,
            &Vec<(CompoundDottedName, bool)>,
            &String,
        ),
        RedefVariable<'ctx>,
    > {
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
            if let Symbol(
                Value {
                    data_type: Type::Module,
                    inter_val: Some(InterData::Module(x, _, n)),
                    ..
                },
                _,
            ) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Symbol::empty_mod(old + "." + &name.ids[idx].0))
            {
                this = x;
                old = n.clone();
            } else {
                return Err(RedefVariable::NotAModule(
                    idx,
                    Value::make_mod(sym.0, sym.1, mod_name).into(),
                ));
            }
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol(
                    Value {
                        data_type: Type::Module,
                        inter_val: Some(InterData::Module(ref mut m, ref mut i, _)),
                        ..
                    },
                    _,
                ) => {
                    *m = sym.0;
                    i.append(&mut sym.1);
                    Ok(x.into_mut().as_mod().unwrap())
                }
                Symbol(_, d) => Err(RedefVariable::AlreadyExists(
                    idx,
                    d.loc,
                    Value::make_mod(sym.0, sym.1, mod_name).into(),
                )),
            },
            Entry::Vacant(x) => Ok(x
                .insert(Value::make_mod(sym.0, sym.1, mod_name).into())
                .as_mod()
                .unwrap()),
        }
    }
    pub fn lookup_mod(
        &mut self,
        name: &DottedName,
    ) -> Result<
        (
            HashMap<String, Symbol<'ctx>>,
            Vec<(CompoundDottedName, bool)>,
            String,
        ),
        UndefVariable,
    > {
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
            if let Some((x, _, n)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| Symbol::empty_mod(old + "." + &name.ids[idx].0))
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
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol(
                    Value {
                        data_type: Type::Module,
                        ..
                    },
                    _,
                ) => {
                    if let Symbol(
                        Value {
                            data_type: Type::Module,
                            inter_val: Some(InterData::Module(s, i, n)),
                            ..
                        },
                        _,
                    ) = x.remove()
                    {
                        Ok((s, i, n))
                    } else {
                        Err(UndefVariable::NotAModule(idx))
                    }
                }
                Symbol(..) => Err(UndefVariable::DoesNotExist(idx)), // should be AlreadyExists, but DoesNotExist wouldn't arise here
            },
            Entry::Vacant(_) => Ok(Default::default()),
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for (name, sym) in self.symbols.iter() {
            if sym.1.export {
                out.write_all(name.as_bytes())?;
                out.write_all(&[0])?;
                sym.save(out)?;
            }
        }
        out.write_all(&[0])?;
        for import in self
            .imports
            .iter()
            .filter_map(|(s, b)| if *b { Some(s) } else { None })
        {
            import.save(out)?;
        }
        out.write_all(&[0])
    }
    pub fn load<R: Read + BufRead>(
        &mut self,
        buf: &mut R,
        ctx: &CompCtx<'ctx>,
    ) -> io::Result<Vec<String>> {
        let mut out = vec![];
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {
                name.pop();
            }
            if name.is_empty() {
                break;
            }
            let name = String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8");
            match self.symbols.entry(name) {
                Entry::Occupied(mut x) => match (x.get_mut(), Symbol::load(buf, ctx)?) {
                    (
                        Symbol(
                            Value {
                                data_type: Type::Module,
                                inter_val: Some(InterData::Module(bs, bi, _)),
                                ..
                            },
                            _,
                        ),
                        Symbol(
                            Value {
                                data_type: Type::Module,
                                inter_val: Some(InterData::Module(ns, mut ni, _)),
                                ..
                            },
                            _,
                        ),
                    ) => {
                        bi.append(&mut ni);
                        out.append(&mut merge(bs, ns));
                    }
                    _ => out.push(x.key().clone()),
                },
                Entry::Vacant(x) => {
                    x.insert(Symbol::load(buf, ctx)?);
                }
            }
        }
        while let Some(val) = CompoundDottedName::load(buf)? {
            self.imports.push((val, false));
        }
        Ok(out)
    }
    pub fn load_new<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut out = HashMap::new();
        let mut imports = vec![];
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {
                name.pop();
            }
            if name.is_empty() {
                break;
            }
            out.insert(
                String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8"),
                Symbol::load(buf, ctx)?,
            );
        }
        while let Some(val) = CompoundDottedName::load(buf)? {
            imports.push((val, false));
        }
        Ok(VarMap {
            parent: None,
            symbols: out,
            imports,
        })
    }
    pub fn dump(&self) {
        eprintln!("top level");
        self.imports
            .iter()
            .for_each(|(i, _)| eprintln!("    import {i}"));
        self.symbols.iter().for_each(|(k, v)| {
            eprint!("    {k:?}: ");
            v.dump(4);
        })
    }
    pub fn satisfy<'vm>(
        (symbols, imports): (
            &'vm HashMap<String, Symbol<'ctx>>,
            &'vm Vec<(CompoundDottedName, bool)>,
        ),
        name: &str,
        pattern: &[CompoundDottedNameSegment],
        root: &'vm VarMap<'ctx>,
    ) -> Option<&'vm Symbol<'ctx>> {
        use CompoundDottedNameSegment::*;
        match pattern.first()? {
            Identifier(x, _) => {
                if pattern.len() == 1 {
                    if x == name {
                        symbols.get(name).or_else(|| {
                            imports
                                .iter()
                                .filter_map(|(i, _)| if i.ends_with(name) { Some(i) } else { None })
                                .find_map(|i| {
                                    Self::satisfy(
                                        if i.global {
                                            (&root.symbols, &root.imports)
                                        } else {
                                            (symbols, imports)
                                        },
                                        name,
                                        &i.ids,
                                        root,
                                    )
                                })
                        })
                    } else {
                        None
                    }
                } else if let Some(Symbol(
                    Value {
                        data_type: Type::Module,
                        inter_val: Some(InterData::Module(s, i, _)),
                        ..
                    },
                    _,
                )) = symbols.get(x.as_str())
                {
                    Self::satisfy((s, i), name, &pattern[1..], root)
                } else {
                    None
                }
            }
            Glob(_) => {
                if pattern.len() == 1 {
                    symbols.get(name).or_else(|| {
                        imports
                            .iter()
                            .filter_map(|(i, _)| if i.ends_with(name) { Some(i) } else { None })
                            .find_map(|i| {
                                Self::satisfy(
                                    if i.global {
                                        (&root.symbols, &root.imports)
                                    } else {
                                        (symbols, imports)
                                    },
                                    name,
                                    &i.ids,
                                    root,
                                )
                            })
                    })
                } else {
                    symbols.values().find_map(|v| {
                        if let Symbol(
                            Value {
                                data_type: Type::Module,
                                inter_val: Some(InterData::Module(s, i, _)),
                                ..
                            },
                            _,
                        ) = v
                        {
                            Self::satisfy((s, i), name, &pattern[1..], root)
                        } else {
                            None
                        }
                    })
                }
            }
            Group(x) => x.iter().find_map(|v| {
                let mut v = v.clone();
                v.extend_from_slice(&pattern[1..]);
                Self::satisfy((symbols, imports), name, &v, root)
            }),
        }
    }
    pub fn lookup_in_mod<'vm>(
        (symbols, imports): (
            &'vm HashMap<String, Symbol<'ctx>>,
            &'vm Vec<(CompoundDottedName, bool)>,
        ),
        name: &str,
        root: &'vm VarMap<'ctx>,
    ) -> Option<&'vm Symbol<'ctx>> {
        symbols.get(name).or_else(|| {
            imports
                .iter()
                .filter_map(|(i, _)| if i.ends_with(name) { Some(i) } else { None })
                .find_map(|i| Self::satisfy((symbols, imports), name, &i.ids, root))
        })
    }
    pub fn lookup(&self, name: &str, global: bool) -> Option<&Symbol<'ctx>> {
        let root = self.root();
        if global {
            root.lookup(name, false)
        } else {
            self.symbols
                .get(name)
                .or_else(|| {
                    self.imports
                        .iter()
                        .filter_map(|(i, _)| if i.ends_with(name) { Some(i) } else { None })
                        .find_map(|i| {
                            let this = if i.global { self.root() } else { self };
                            Self::satisfy((&this.symbols, &this.imports), name, &i.ids, root)
                        })
                })
                .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name, global)))
        }
    }
    pub fn verify_in_mod<'vm>(
        (symbols, imports): (
            &'vm HashMap<String, Symbol<'ctx>>,
            &'vm Vec<(CompoundDottedName, bool)>,
        ),
        pattern: &[CompoundDottedNameSegment],
        root: &'vm VarMap<'ctx>,
    ) -> Vec<SourceSpan> {
        use CompoundDottedNameSegment::*;
        match pattern.first() {
            None => vec![],
            Some(Identifier(x, l)) => match Self::lookup_in_mod((symbols, imports), x, root) {
                Some(_) if pattern.len() == 1 => vec![],
                Some(Symbol(
                    Value {
                        data_type: Type::Module,
                        inter_val: Some(InterData::Module(s, i, _)),
                        ..
                    },
                    _,
                )) => Self::verify_in_mod((s, i), &pattern[1..], root),
                _ => vec![*l],
            },
            Some(Glob(l)) => {
                if pattern.len() == 1 {
                    if symbols.is_empty() && imports.is_empty() {
                        vec![*l]
                    } else {
                        vec![]
                    }
                } else {
                    let mut ll = symbols
                        .values()
                        .filter_map(|x| x.as_mod())
                        .map(|(s, i, _)| Self::verify_in_mod((s, i), &pattern[1..], root))
                        .collect::<LinkedList<_>>();
                    if let Some(mut out) = ll.pop_front() {
                        ll.into_iter().for_each(|v| out.retain(|l| v.contains(l)));
                        out
                    } else {
                        vec![]
                    }
                }
            }
            Some(Group(x)) => {
                let mut ll = x
                    .iter()
                    .map(|v| {
                        let mut vec = v.clone();
                        vec.extend_from_slice(&pattern[1..]);
                        Self::verify_in_mod((symbols, imports), &vec, root)
                    })
                    .collect::<LinkedList<_>>();
                if let Some(mut out) = ll.pop_front() {
                    ll.into_iter().for_each(|v| out.retain(|l| v.contains(l)));
                    out
                } else {
                    vec![]
                }
            }
        }
    }
    pub fn verify(&self, pattern: &CompoundDottedName) -> Vec<SourceSpan> {
        let root = self.root();
        if pattern.global
            && self
                .parent
                .as_ref()
                .and_then(|p| p.parent.as_ref())
                .is_some()
        {
            root.verify(pattern)
        } else {
            let mut vec = Self::verify_in_mod((&self.symbols, &self.imports), &pattern.ids, root);
            if let Some(p) = &self.parent {
                let v2 = p.verify(pattern);
                vec.retain(|l| v2.contains(l));
            }
            vec
        }
    }
}
impl<'ctx> From<HashMap<String, Symbol<'ctx>>> for VarMap<'ctx> {
    fn from(symbols: HashMap<String, Symbol<'ctx>>) -> Self {
        VarMap {
            parent: None,
            symbols,
            imports: Vec::new(),
        }
    }
}
impl<'ctx> From<HashMap<String, Symbol<'ctx>>> for Box<VarMap<'ctx>> {
    fn from(symbols: HashMap<String, Symbol<'ctx>>) -> Self {
        Box::new(VarMap {
            parent: None,
            symbols,
            imports: Vec::new(),
        })
    }
}
fn merge<'ctx>(
    base: &mut HashMap<String, Symbol<'ctx>>,
    new: HashMap<String, Symbol<'ctx>>,
) -> Vec<String> {
    let mut out = vec![];
    for (key, val) in new {
        match base.entry(key) {
            Entry::Occupied(mut e) => match (e.get_mut(), val) {
                (
                    Symbol(
                        Value {
                            data_type: Type::Module,
                            inter_val: Some(InterData::Module(bs, bi, _)),
                            ..
                        },
                        _,
                    ),
                    Symbol(
                        Value {
                            data_type: Type::Module,
                            inter_val: Some(InterData::Module(ns, mut ni, _)),
                            ..
                        },
                        _,
                    ),
                ) => {
                    bi.append(&mut ni);
                    out.extend(merge(bs, ns).into_iter().map(|x| e.key().to_owned() + &x));
                }
                _ => out.push(e.key().clone()),
            },
            Entry::Vacant(e) => {
                e.insert(val);
            }
        }
    }
    out
}
