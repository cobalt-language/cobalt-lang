use crate::*;
use inkwell::values::BasicValueEnum;
use inkwell::types::{BasicTypeEnum::*, BasicMetadataTypeEnum, BasicType};
use std::collections::hash_map::{HashMap, Entry};
use std::io::{self, Write, Read, BufRead};
#[derive(Debug, Clone, Copy)]
pub enum UndefVariable {
    NotAModule(usize),
    DoesNotExist(usize)
}
#[derive(Clone)]
pub enum RedefVariable<'ctx> {
    NotAModule(usize, Symbol<'ctx>),
    AlreadyExists(usize, Option<Location>, Symbol<'ctx>)
}
#[derive(Clone)]
pub struct FnData<'ctx> {
    pub defaults: Vec<InterData<'ctx>>,
    pub cconv: u32
}
#[derive(Clone)]
pub enum InterData<'ctx> {
    Null,
    Int(i128),
    Float(f64),
    Str(String),
    Array(Vec<InterData<'ctx>>),
    Function(FnData<'ctx>),
    InlineAsm(Box<Type>, String, String),
    Type(Box<Type>),
    Module(HashMap<String, Symbol<'ctx>>, Vec<(CompoundDottedName, bool)>)
}
impl<'ctx> InterData<'ctx> {
    pub fn into_compiled(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match self {
            InterData::Int(val) => Some(BasicValueEnum::IntValue(ctx.context.i64_type().const_int(*val as u64, true))),
            InterData::Float(val) => Some(BasicValueEnum::FloatValue(ctx.context.f64_type().const_float(*val))),
            InterData::Str(val) => Some(BasicValueEnum::PointerValue(ctx.builder.build_global_string_ptr(val.as_str(), "cobalt.str").as_pointer_value())),
            _ => None
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            InterData::Null => out.write_all(&[1]),
            InterData::Int(v) => out.write_all(&[2]).and_then(|_| out.write_all(&v.to_be_bytes())),
            InterData::Float(v) => out.write_all(&[3]).and_then(|_| out.write_all(&v.to_be_bytes())),
            InterData::Str(v) => {
                out.write_all(&[4])?;
                out.write_all(v.as_bytes())?;
                out.write_all(&[0])
            },
            InterData::Array(v) => {
                out.write_all(&[5])?;
                out.write_all(&(v.len() as u32).to_be_bytes())?; // length
                for val in v.iter() {val.save(out)?;} // InterData is self-puncatuating
                Ok(())
            },
            InterData::Function(v) => { // serialized the same as InterData::Array
                out.write_all(&[6])?;
                out.write_all(&(v.defaults.len() as u32).to_be_bytes())?;
                for val in v.defaults.iter() {val.save(out)?;}
                out.write_all(&v.cconv.to_be_bytes())?;
                Ok(())
            },
            InterData::InlineAsm(r, c, b) => {
                out.write_all(&[7])?;
                out.write_all(c.as_bytes())?;
                out.write_all(&[0])?;
                out.write_all(b.as_bytes())?;
                out.write_all(&[0])?;
                r.save(out)
            },
            InterData::Type(t) => {
                out.write_all(&[8])?;
                t.save(out)
            },
            InterData::Module(v, i) => {
                out.write_all(&[9])?;
                for (name, sym) in v.iter() {
                    if sym.1.export {
                        out.write_all(name.as_bytes())?; // name, null-terminated
                        out.write_all(&[0])?;
                        sym.save(out)?;
                    }
                }
                out.write_all(&[0])?; // null terminator for symbol list
                for import in i.iter().filter_map(|(s, b)| if *b {Some(s)} else {None}) {
                    import.save(out)?;
                }
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Option<Self>> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        Ok(match c {
            0 => None,
            1 => Some(InterData::Null),
            2 => {
                let mut bytes = [0; 16];
                buf.read_exact(&mut bytes)?;
                Some(InterData::Int(i128::from_be_bytes(bytes)))
            },
            3 => {
                let mut bytes = [0; 8];
                buf.read_exact(&mut bytes)?;
                Some(InterData::Float(f64::from_be_bytes(bytes)))
            },
            4 => {
                let mut vec = Vec::new();
                buf.read_until(0, &mut vec)?;
                Some(InterData::Str(String::from_utf8(vec).expect("Interpreted strings should be valid UTF-8")))
            },
            5 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                let len = u32::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {vec.push(Self::load(buf, ctx)?.expect("# of unwrapped array elements doesn't match the prefixed count"))}
                Some(InterData::Array(vec))
            },
            6 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                let len = u32::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {vec.push(Self::load(buf, ctx)?.expect("# of unwrapped default parameters doesn't match the prefixed count"))}
                buf.read_exact(&mut bytes)?;
                Some(InterData::Function(FnData{defaults: vec, cconv: u32::from_be_bytes(bytes)}))
            },
            7 => {
                let mut constraint = Vec::new();
                let mut body = Vec::new();
                buf.read_until(0, &mut constraint)?;
                buf.read_until(0, &mut body)?;
                Some(InterData::InlineAsm(Box::new(Type::load(buf)?), String::from_utf8(constraint).expect("Inline assmebly constraint should be valid UTF-8"), String::from_utf8(body).expect("Inline assembly should be valid UTF-8")))
            },
            8 => Some(InterData::Type(Box::new(Type::load(buf)?))),
            9 => {
                let mut out = HashMap::new();
                let mut imports = vec![];
                loop {
                    let mut name = vec![];
                    buf.read_until(0, &mut name)?;
                    if name.last() == Some(&0) {name.pop();}
                    if name.is_empty() {break}
                    out.insert(String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8"), Symbol::load(buf, ctx)?);
                }
                while let Some(val) = CompoundDottedName::load(buf)? {imports.push((val, false));}
                Some(InterData::Module(out, imports))
            },
            x => panic!("read interpreted data type expecting number in 1..=9, got {x}")
        })
    }
}
#[derive(Clone)]
pub struct Value<'ctx> {
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData<'ctx>>,
    pub data_type: Type
}
impl<'ctx> Value<'ctx> {
    pub fn error() -> Self {Value {comp_val: None, inter_val: None, data_type: Type::Error}}
    pub fn null() -> Self {Value {comp_val: None, inter_val: Some(InterData::Null), data_type: Type::Null}}
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: Type) -> Self {Value {comp_val: Some(comp_val), inter_val: None, data_type}}
    pub fn interpreted(comp_val: BasicValueEnum<'ctx>, inter_val: InterData<'ctx>, data_type: Type) -> Self {Value {comp_val: Some(comp_val), inter_val: Some(inter_val), data_type}}
    pub fn metaval(inter_val: InterData<'ctx>, data_type: Type) -> Self {Value {comp_val: None, inter_val: Some(inter_val), data_type}}
    pub fn make_type(type_: Type) -> Self {Value {comp_val: None, inter_val: Some(InterData::Type(Box::new(type_))), data_type: Type::TypeData}}
    pub fn empty_mod() -> Self {Value {comp_val: None, inter_val: Some(InterData::Module(HashMap::new(), vec![])), data_type: Type::Module}}
    pub fn make_mod(syms: HashMap<String, Symbol<'ctx>>, imps: Vec<(CompoundDottedName, bool)>) -> Self {Value {comp_val: None, inter_val: Some(InterData::Module(syms, imps)), data_type: Type::Module}}
    pub fn value(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}
    pub fn into_value(self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}
}
#[derive(Clone, PartialEq, Eq)]
pub struct VariableData {
    pub good: bool,
    pub export: bool,
    pub loc: Option<Location>,
}
impl VariableData {
    pub fn new(loc: Location) -> Self {
        VariableData {
            good: true,
            export: true,
            loc: Some(loc)
        }
    }
    pub fn with_vis(loc: Location, export: bool) -> Self {
        VariableData {
            good: true,
            export,
            loc: Some(loc)
        }
    }
}
impl Default for VariableData {
    fn default() -> Self {
        VariableData {
            good: true,
            export: true,
            loc: None
        }
    }
}
impl<'ctx> From<Value<'ctx>> for Symbol<'ctx> {
    fn from(val: Value<'ctx>) -> Self {
        Symbol(val, VariableData::default())
    }
}
#[derive(Clone)]
pub struct Symbol<'ctx>(pub Value<'ctx>, pub VariableData);
impl<'ctx> Symbol<'ctx> {
    pub fn into_mod(self) -> Option<(HashMap<String, Symbol<'ctx>>, Vec<(CompoundDottedName, bool)>)> {if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i)), ..}, _) = self {Some((s, i))} else {None}}
    pub fn as_mod(&self) -> Option<(&HashMap<String, Symbol<'ctx>>, &Vec<(CompoundDottedName, bool)>)> {if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i)), ..}, _) = self {Some((s, i))} else {None}}
    pub fn as_mod_mut(&mut self) -> Option<(&mut HashMap<String, Symbol<'ctx>>, &mut Vec<(CompoundDottedName, bool)>)> {if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i)), ..}, _) = self {Some((s, i))} else {None}}
    pub fn empty_mod() -> Self {Value::empty_mod().into()}
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let v = &self.0;
        out.write_all(v.comp_val.as_ref().map(|v| v.into_pointer_value().get_name().to_bytes().to_owned()).unwrap_or_else(Vec::new).as_slice())?; // LLVM symbol name, null-terminated
        out.write_all(&[0])?;
        if let Some(v) = v.inter_val.as_ref() {v.save(out)?}
        else {out.write_all(&[0])?} // Interpreted value, self-punctuating
        v.data_type.save(out) // Type
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut var = Value::error();
        let mut name = vec![];
        buf.read_until(0, &mut name)?;
        if name.last() == Some(&0) {name.pop();}
        var.inter_val = InterData::load(buf, ctx)?;
        var.data_type = Type::load(buf)?;
        if !name.is_empty() {
            use inkwell::module::Linkage::DLLImport;
            if let Type::Function(ret, params) = &var.data_type {
                if let Some(llt) = ret.llvm_type(ctx) {
                    let mut good = true;
                    let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                    if good {
                        let ft = llt.fn_type(&ps, false);
                        let fv = ctx.module.add_function(std::str::from_utf8(&name).expect("LLVM function names should be valid UTF-8"), ft, None);
                        if let Some(InterData::Function(FnData {cconv, ..})) = var.inter_val {fv.set_call_conventions(cconv)}
                        let gv = fv.as_global_value();
                        gv.set_linkage(DLLImport);
                        var.comp_val = Some(BasicValueEnum::PointerValue(gv.as_pointer_value()));
                    }
                }
                else if **ret == Type::Null {
                    let mut good = true;
                    let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                    if good {
                        let ft = ctx.context.void_type().fn_type(&ps, false);
                        let fv = ctx.module.add_function(std::str::from_utf8(&name).expect("LLVM function names should be valid UTF-8"), ft, None);
                        if let Some(InterData::Function(FnData {cconv, ..})) = var.inter_val {fv.set_call_conventions(cconv)}
                        let gv = fv.as_global_value();
                        gv.set_linkage(DLLImport);
                        var.comp_val = Some(BasicValueEnum::PointerValue(gv.as_pointer_value()));
                    }
                }
            }
            else if let Some(t) = if let Type::Reference(ref b, _) = var.data_type {b.llvm_type(ctx)} else {None} {
                let gv = ctx.module.add_global(t, None, std::str::from_utf8(&name).expect("LLVM variable names should be valid UTF-8")); // maybe do something with linkage/call convention?
                gv.set_linkage(DLLImport);
                var.comp_val = Some(BasicValueEnum::PointerValue(gv.as_pointer_value()));
            }
        }
        Ok(Symbol(var, VariableData {export: false, ..VariableData::default()}))
    }
    pub fn dump(&self, depth: usize) {
        match self {
            Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i)), ..}, _) => {
                let pre = " ".repeat(depth);
                eprintln!("module");
                for (i, _) in i {eprintln!("{pre}    import: {i}")}
                for (k, s) in s {
                    eprint!("{pre}    {k}: ");
                    s.dump(depth + 4)
                }
            },
            Symbol(Value {data_type: dt, ..}, _) => eprintln!("variable of type {dt}")
        }
    }
}
#[derive(Default)]
pub struct VarMap<'ctx> {
    pub parent: Option<Box<VarMap<'ctx>>>,
    pub symbols: HashMap<String, Symbol<'ctx>>,
    pub imports: Vec<(CompoundDottedName, bool)>
}
impl<'ctx> VarMap<'ctx> {
    pub fn new(parent: Option<Box<VarMap<'ctx>>>) -> Self {VarMap {parent, ..Self::default()}}
    pub fn orphan(self) -> Self {VarMap {parent: None, ..self}}
    pub fn reparent(self, parent: Box<VarMap<'ctx>>) -> Self {VarMap {parent: Some(parent), ..self}}
    pub fn root(&self) -> &Self {self.parent.as_ref().map(|x| x.root()).unwrap_or(self)}
    pub fn root_mut(&mut self) -> &mut Self {
        if self.parent.is_some() {self.parent.as_mut().unwrap().root_mut()}
        else {self}
    }
    pub fn find_sym(&self, name: &str) -> Option<&Symbol<'ctx>> {self.symbols.get(name).or_else(|| self.parent.as_ref().and_then(|p| p.find_sym(name)))}
    fn satisfy<'a>((symbols, imports): (&'a HashMap<String, Symbol<'ctx>>, &'a Vec<(CompoundDottedName, bool)>), parent: &'a Option<Box<VarMap<'ctx>>>, root: &VarMap, name: &[(String, Location)], pat: &[CompoundDottedNameSegment]) -> Option<&'a Symbol<'ctx>> {
        use CompoundDottedNameSegment::*;
        match pat.get(0)? {
            Identifier(id, _) =>
                if pat.len() == 1 {
                    if name.len() == 1 && &name[0].0 == id {
                        symbols.get(id).or_else(|| parent.as_ref().and_then(|p| p.find_sym(id)))
                    } else {None}
                }
                else {
                    Self::satisfy(symbols.get(id).and_then(|s| s.as_mod()).or_else(|| parent.as_ref().and_then(|p| p.find_sym(id))?.as_mod())?, parent, root, name, &pat[1..])
                },
            Group(ids) => ids.iter().cloned().find_map(|mut v| {
                v.extend_from_slice(&pat[1..]);
                Self::satisfy((symbols, imports), parent, root, name, &v)
            }),
            Glob(_) =>
                if pat.len() == 1 {
                    if name.len() == 1 {
                        symbols.get(&name[0].0).or_else(|| parent.as_ref().and_then(|p| p.find_sym(&name[0].0)))
                    } else {None}
                }
                else {symbols.values().find_map(|v| Self::satisfy(v.as_mod()?, parent, root, name, &pat[1..]))}
        }
    }
    pub fn insert(&mut self, name: &DottedName, sym: Symbol<'ctx>) -> Result<&Symbol<'ctx>, RedefVariable<'ctx>> {
        let mut this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.is_empty() {panic!("mod_insert cannot insert a value at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(x, _)), ..}, _) = this.entry(name.ids[idx].0.clone()).or_insert_with(Symbol::empty_mod) {this = x}
            else {return Err(RedefVariable::NotAModule(idx, sym))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(x) => Err(RedefVariable::AlreadyExists(idx, x.get().1.loc.clone(), sym)),
            Entry::Vacant(x) => Ok(&*x.insert(sym))
        }
    }
    pub fn insert_mod(&mut self, name: &DottedName, mut sym: (HashMap<String, Symbol<'ctx>>, Vec<(CompoundDottedName, bool)>)) -> Result<(&HashMap<String, Symbol<'ctx>>, &Vec<(CompoundDottedName, bool)>), RedefVariable<'ctx>> {
        let mut this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.is_empty() {panic!("mod_insert cannot insert a value at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(x, _)), ..}, _) = this.entry(name.ids[idx].0.clone()).or_insert_with(Symbol::empty_mod) {this = x}
            else {return Err(RedefVariable::NotAModule(idx, Value::make_mod(sym.0, sym.1).into()))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(ref mut m, ref mut i)), ..}, _) => {
                    *m = sym.0;
                    i.append(&mut sym.1);
                    Ok(x.into_mut().as_mod().unwrap())
                },
                Symbol(_, d) => Err(RedefVariable::AlreadyExists(idx, d.loc.clone(), Value::make_mod(sym.0, sym.1).into()))
            },
            Entry::Vacant(x) => Ok(x.insert(Value::make_mod(sym.0, sym.1).into()).as_mod().unwrap())
        }
    }
    pub fn lookup_mod(&mut self, name: &DottedName) -> Result<(HashMap<String, Symbol<'ctx>>, Vec<(CompoundDottedName, bool)>), UndefVariable> {
        let mut this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.is_empty() {panic!("mod_lookup_insert cannot find a module at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Some((x, _)) = this.entry(name.ids[idx].0.clone()).or_insert_with(Symbol::empty_mod).as_mod_mut() {this = x}
            else {return Err(UndefVariable::NotAModule(idx))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol(Value {data_type: Type::Module, ..}, _) => if let Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, i)), ..}, _) = x.remove() {Ok((s, i))} else {Err(UndefVariable::NotAModule(idx))},
                Symbol(..) => Err(UndefVariable::DoesNotExist(idx)) // should be AlreadyExists, but DoesNotExist wouldn't arise here
            },
            Entry::Vacant(_) => Ok(Default::default())
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for (k, (v, e)) in types::NOMINAL_TYPES.read().expect("Value should not be poisoned!").iter() {
            if *e {
                out.write_all(k.as_bytes())?;
                out.write_all(&[0])?;
                v.save(out)?;
            }
        }
        out.write_all(&[0])?;
        for (name, sym) in self.symbols.iter() {
            if sym.1.export {
                out.write_all(name.as_bytes())?;
                out.write_all(&[0])?;
                sym.save(out)?;
            }
        }
        out.write_all(&[0])?;
        for import in self.imports.iter().filter_map(|(s, b)| if *b {Some(s)} else {None}) {
            import.save(out)?;
        }
        out.write_all(&[0])
    }
    pub fn load<R: Read + BufRead>(&mut self, buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Vec<String>> {
        let mut out = vec![];
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.is_empty() {break}
            types::NOMINAL_TYPES.write().expect("Value should not be poisoned!").insert(String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8"), (Type::load(buf)?, false));
        }
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.is_empty() {break}
            let name = String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8");
            match self.symbols.entry(name) {
                Entry::Occupied(mut x) => match (x.get_mut(), Symbol::load(buf, ctx)?) {
                    (Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(bs, bi)), ..}, _), Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(ns, mut ni)), ..}, _)) => {
                        bi.append(&mut ni);
                        out.append(&mut merge(bs, ns));
                    },
                    _ => out.push(x.key().clone())
                },
                Entry::Vacant(x) => {x.insert(Symbol::load(buf, ctx)?);}
            }
        }
        while let Some(val) = CompoundDottedName::load(buf)? {self.imports.push((val, false));}
        Ok(out)
    }
    pub fn load_new<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.is_empty() {break}
            types::NOMINAL_TYPES.write().expect("Value should not be poisoned!").insert(String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8"), (Type::load(buf)?, false));
        }
        let mut out = HashMap::new();
        let mut imports = vec![];
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.is_empty() {break}
            out.insert(String::from_utf8(name).expect("Cobalt symbols should be valid UTF-8"), Symbol::load(buf, ctx)?);
        }
        while let Some(val) = CompoundDottedName::load(buf)? {imports.push((val, false));}
        Ok(VarMap {parent: None, symbols: out, imports})
    }
    pub fn dump(&self) {
        eprintln!("module");
        self.imports.iter().for_each(|(i, _)| eprintln!("    import {i}"));
        self.symbols.iter().for_each(|(k, v)| {
            eprint!("    {k:?}: ");
            v.dump(4);
        })
    }
    pub fn lookup_one(&self, name: &str, loc: &Location, global: bool) -> Option<&Symbol<'ctx>> {
        if global {self.root().lookup_one(name, loc, false)}
        else {
            self.symbols.get(name).or_else(|| self.parent.as_ref().and_then(|v| v.lookup_one(name, loc, global))).or_else(|| self.imports.iter().find_map(|(i, _)| {
                Self::satisfy((&self.symbols, &self.imports), &self.parent, self.root(), &[(name.to_string(), loc.clone())], &i.ids)
            }))
        }
    }
}
impl<'ctx> From<HashMap<String, Symbol<'ctx>>> for VarMap<'ctx> {
    fn from(symbols: HashMap<String, Symbol<'ctx>>) -> Self {
        VarMap {
            parent: None,
            symbols,
            imports: Vec::new()
        }
    }
}
impl<'ctx> From<HashMap<String, Symbol<'ctx>>> for Box<VarMap<'ctx>> {
    fn from(symbols: HashMap<String, Symbol<'ctx>>) -> Self {
        Box::new(VarMap {
            parent: None,
            symbols,
            imports: Vec::new()
        })
    }
}
fn merge<'ctx>(base: &mut HashMap<String, Symbol<'ctx>>, new: HashMap<String, Symbol<'ctx>>) -> Vec<String> {
    let mut out = vec![];
    for (key, val) in new {
        match base.entry(key) {
            Entry::Occupied(mut e) => match (e.get_mut(), val) {
                (Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(bs, bi)), ..}, _), Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(ns, mut ni)), ..}, _)) => {
                    bi.append(&mut ni);
                    out.extend(merge(bs, ns).into_iter().map(|x| e.key().to_owned() + &x));
                },
                _ => out.push(e.key().clone())
            },
            Entry::Vacant(e) => {e.insert(val);}
        }
    }
    out
}
