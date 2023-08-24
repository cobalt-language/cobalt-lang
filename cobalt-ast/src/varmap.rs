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
pub enum RedefVariable<'src, 'ctx> {
    NotAModule(usize, Symbol<'src, 'ctx>),
    AlreadyExists(usize, Option<SourceSpan>, Symbol<'src, 'ctx>),
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
impl<'src, 'ctx> From<Value<'src, 'ctx>> for Symbol<'src, 'ctx> {
    fn from(val: Value<'src, 'ctx>) -> Self {
        Symbol(val, VariableData::default())
    }
}
#[derive(Debug, Clone)]
pub struct Symbol<'src, 'ctx>(pub Value<'src, 'ctx>, pub VariableData);
impl<'src, 'ctx> Symbol<'src, 'ctx> {
    pub fn into_mod(
        self,
    ) -> Option<(
        HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        Vec<(CompoundDottedName<'src>, bool)>,
        String,
    )> {
        self.0.into_mod()
    }
    pub fn as_mod(
        &self,
    ) -> Option<(
        &HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &Vec<(CompoundDottedName<'src>, bool)>,
        &String,
    )> {
        self.0.as_mod()
    }
    pub fn as_mod_mut(
        &mut self,
    ) -> Option<(
        &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &mut Vec<(CompoundDottedName<'src>, bool)>,
        &mut String,
    )> {
        self.0.as_mod_mut()
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        self.0.save(out)
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'src, 'ctx>) -> io::Result<Self> {
        Ok(Symbol(
            Value::load(buf, ctx)?,
            VariableData {
                export: false,
                ..VariableData::default()
            },
        ))
    }
    pub fn dump(&self, depth: usize) {
        if let (types::Module::KIND, Some(InterData::Module(s, i, n))) =
            (self.0.data_type.self_kind(), &self.0.inter_val)
        {
            let pre = " ".repeat(depth);
            eprintln!("module {n:?}");
            for (i, _) in i {
                eprintln!("{pre}    import: {i}")
            }
            for (k, s) in s {
                eprint!("{pre}    {k:?}: ");
                s.dump(depth + 4)
            }
        } else {
            eprintln!("variable of type {}", self.0.data_type)
        }
    }
}
#[derive(Default)]
pub struct VarMap<'src, 'ctx> {
    pub parent: Option<Box<Self>>,
    pub symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
    pub imports: Vec<(CompoundDottedName<'src>, bool)>,
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
            if let Some((x, _, _)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| {
                    Value::empty_mod(unreachable_span(), name.start(idx + 1).to_string()).into()
                })
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
        mut sym: (
            HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            Vec<(CompoundDottedName<'src>, bool)>,
        ),
        mod_name: String,
    ) -> Result<
        (
            &HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            &Vec<(CompoundDottedName<'src>, bool)>,
            &String,
        ),
        RedefVariable<'src, 'ctx>,
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
            if let Some((x, _, n)) = this
                .entry(name.ids[idx].0.clone())
                .or_insert_with(|| {
                    Value::empty_mod(unreachable_span(), old + "." + &name.ids[idx].0).into()
                })
                .0
                .as_mod_mut()
            {
                this = x;
                old = n.clone();
            } else {
                return Err(RedefVariable::NotAModule(
                    idx,
                    Value::make_mod(unreachable_span(), sym.0, sym.1, mod_name).into(),
                ));
            }
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => {
                if let Some((m, i, _)) = x.get_mut().0.as_mod_mut() {
                    *m = sym.0;
                    i.append(&mut sym.1);
                    Ok(x.into_mut().as_mod().unwrap())
                } else {
                    Err(RedefVariable::AlreadyExists(
                        idx,
                        x.get_mut().1.loc,
                        Value::make_mod(unreachable_span(), sym.0, sym.1, mod_name).into(),
                    ))
                }
            }
            Entry::Vacant(x) => Ok(x
                .insert(Value::make_mod(unreachable_span(), sym.0, sym.1, mod_name).into())
                .as_mod()
                .unwrap()),
        }
    }
    pub fn lookup_mod(
        &mut self,
        name: &DottedName<'src>,
    ) -> Result<
        (
            HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            Vec<(CompoundDottedName<'src>, bool)>,
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
                .or_insert_with(|| {
                    Value::empty_mod(unreachable_span(), old + "." + &name.ids[idx].0).into()
                })
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
                if x.get().0.data_type.self_kind() == types::Module::KIND {
                    if let Some(InterData::Module(s, i, n)) = x.remove().0.inter_val {
                        Ok((s, i, n))
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
        ctx: &CompCtx<'src, 'ctx>,
    ) -> io::Result<Vec<Cow<'src, str>>> {
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
            match self.symbols.entry(name.into()) {
                Entry::Occupied(mut x) => {
                    let l = x.get_mut();
                    let r = Symbol::load(buf, ctx)?;
                    const MOD: std::num::NonZeroU64 = types::Module::KIND;
                    if let (
                        MOD,
                        MOD,
                        Some(InterData::Module(bs, bi, _)),
                        Some(InterData::Module(ns, mut ni, _)),
                    ) = (
                        l.0.data_type.self_kind(),
                        r.0.data_type.self_kind(),
                        &mut l.0.inter_val,
                        r.0.inter_val,
                    ) {
                        bi.append(&mut ni);
                        out.append(&mut merge(bs, ns));
                    } else {
                        out.push(x.key().clone())
                    }
                }
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
    pub fn load_new<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'src, 'ctx>) -> io::Result<Self> {
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
                String::from_utf8(name)
                    .expect("Cobalt symbols should be valid UTF-8")
                    .into(),
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
            &'vm HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            &'vm Vec<(CompoundDottedName, bool)>,
        ),
        name: &str,
        pattern: &[CompoundDottedNameSegment],
        root: &'vm VarMap<'src, 'ctx>,
    ) -> Option<&'vm Symbol<'src, 'ctx>> {
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
                } else if let Some((s, i, _)) = symbols.get(&**x).and_then(|s| s.0.as_mod()) {
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
                        if let Some((s, i, _)) = v.as_mod() {
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
            &'vm HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            &'vm Vec<(CompoundDottedName<'src>, bool)>,
        ),
        name: &str,
        root: &'vm VarMap<'src, 'ctx>,
    ) -> Option<&'vm Symbol<'src, 'ctx>> {
        symbols.get(name).or_else(|| {
            imports
                .iter()
                .filter_map(|(i, _)| if i.ends_with(name) { Some(i) } else { None })
                .find_map(|i| Self::satisfy((symbols, imports), name, &i.ids, root))
        })
    }
    pub fn lookup(&self, name: &str, global: bool) -> Option<&Symbol<'src, 'ctx>> {
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
            &'vm HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
            &'vm Vec<(CompoundDottedName<'src>, bool)>,
        ),
        pattern: &[CompoundDottedNameSegment<'src>],
        root: &'vm VarMap<'src, 'ctx>,
    ) -> Vec<SourceSpan> {
        use CompoundDottedNameSegment::*;
        match pattern.first() {
            None => vec![],
            Some(Identifier(x, l)) => {
                if pattern.len() == 1 {
                    vec![]
                } else if let Some((s, i, _)) =
                    Self::lookup_in_mod((symbols, imports), x, root).and_then(|s| s.0.as_mod())
                {
                    Self::verify_in_mod((s, i), &pattern[1..], root)
                } else {
                    vec![*l]
                }
            }
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
    pub fn verify(&self, pattern: &CompoundDottedName<'src>) -> Vec<SourceSpan> {
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
impl<'src, 'ctx> From<HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>> for VarMap<'src, 'ctx> {
    fn from(symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>) -> Self {
        VarMap {
            parent: None,
            symbols,
            imports: Vec::new(),
        }
    }
}
impl<'src, 'ctx> From<HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>> for Box<VarMap<'src, 'ctx>> {
    fn from(symbols: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>) -> Self {
        Box::new(VarMap {
            parent: None,
            symbols,
            imports: Vec::new(),
        })
    }
}
fn merge<'src, 'ctx>(
    base: &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
    new: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
) -> Vec<Cow<'src, str>> {
    let mut out = vec![];
    for (key, val) in new {
        match base.entry(key) {
            Entry::Occupied(mut e) => {
                let l = e.get_mut();
                let r = val;
                const MOD: std::num::NonZeroU64 = types::Module::KIND;
                if let (
                    MOD,
                    MOD,
                    Some(InterData::Module(bs, bi, _)),
                    Some(InterData::Module(ns, mut ni, _)),
                ) = (
                    l.0.data_type.self_kind(),
                    r.0.data_type.self_kind(),
                    &mut l.0.inter_val,
                    r.0.inter_val,
                ) {
                    bi.append(&mut ni);
                    out.append(&mut merge(bs, ns));
                } else {
                    out.push(e.key().clone())
                }
            }
            Entry::Vacant(e) => {
                e.insert(val);
            }
        }
    }
    out
}
