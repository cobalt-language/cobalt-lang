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
    AlreadyExists(usize, Symbol<'ctx>)
}
#[derive(Clone)]
pub struct FnData {
    pub defaults: Vec<InterData>
}
#[derive(Clone)]
pub enum InterData {
    Null,
    Int(i128),
    Float(f64),
    Str(String),
    Array(Vec<InterData>),
    Function(FnData),
    InlineAsm(String, String)
}
impl InterData {
    pub fn into_compiled<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match self {
            InterData::Int(val) => Some(BasicValueEnum::IntValue(ctx.context.i64_type().const_int(*val as u64, true))),
            InterData::Float(val) => Some(BasicValueEnum::FloatValue(ctx.context.f64_type().const_float(*val))),
            InterData::Str(val) => Some(BasicValueEnum::PointerValue(ctx.builder.build_global_string_ptr(val.as_str(), "__internals.str").as_pointer_value())),
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
                out.write_all(&(v.len() as u64).to_be_bytes())?; // length
                for val in v.iter() {val.save(out)?;} // InterData is self-puncatuating
                Ok(())
            },
            InterData::Function(v) => { // serialized the same as InterData::Array
                out.write_all(&[6])?;
                out.write_all(&(v.defaults.len() as u64).to_be_bytes())?;
                for val in v.defaults.iter() {val.save(out)?;}
                Ok(())
            },
            InterData::InlineAsm(c, b) => {
                out.write_all(&[7])?;
                out.write_all(c.as_bytes())?;
                out.write_all(&[0])?;
                out.write_all(b.as_bytes())?;
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R) -> io::Result<Option<Self>> {
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
                Some(InterData::Str(std::str::from_utf8(&vec).expect("Interpreted strings should be valid UTF-8").to_string()))
            },
            5 => {
                let mut bytes = [0; 8];
                buf.read_exact(&mut bytes)?;
                let len = u64::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {vec.push(Self::load(buf)?.expect("# of unwrapped array elements doesn't match the prefixed count"))}
                Some(InterData::Array(vec))
            },
            6 => {
                let mut bytes = [0; 8];
                buf.read_exact(&mut bytes)?;
                let len = u64::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {vec.push(Self::load(buf)?.expect("# of unwrapped default parameters doesn't match the prefixed count"))}
                Some(InterData::Function(FnData{defaults: vec}))
            },
            7 => {
                let mut constraint = Vec::new();
                let mut body = Vec::new();
                buf.read_until(0, &mut constraint)?;
                buf.read_until(0, &mut body)?;
                Some(InterData::InlineAsm(std::str::from_utf8(&constraint).expect("Inline assmebly constraint should be valid UTF-8").to_string(), std::str::from_utf8(&body).expect("Inline assembly should be valid UTF-8").to_string()))
            },
            x => panic!("read interpreted data type expecting number in 1..=7, got {x}")
        })
    }
}
#[derive(Clone)]
pub struct Variable<'ctx> {
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData>,
    pub data_type: Type,
    pub export: bool
}
impl<'ctx> Variable<'ctx> {
    pub fn error() -> Self {Variable {comp_val: None, inter_val: None, data_type: Type::Null, export: true}}
    pub fn null(data_type: Option<Type>) -> Self {Variable {comp_val: None, inter_val: None, data_type: data_type.unwrap_or(Type::Null), export: true}}
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: None, data_type, export: true}}
    pub fn interpreted(comp_val: BasicValueEnum<'ctx>, inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: Some(inter_val), data_type, export: true}}
    pub fn metaval(inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: None, inter_val: Some(inter_val), data_type, export: true}}
    pub fn value(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.clone().or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}
}
#[derive(Clone)]
pub enum Symbol<'ctx> {
    Variable(Variable<'ctx>),
    Module(HashMap<String, Symbol<'ctx>>, Vec<CompoundDottedName>)
}
impl<'ctx> Symbol<'ctx> {
    pub fn into_var(self) -> Option<Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn into_mod(self) -> Option<(HashMap<String, Symbol<'ctx>>, Vec<CompoundDottedName>)> {if let Symbol::Module(x, i) = self {Some((x, i))} else {None}}
    pub fn as_var(&self) -> Option<&Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn as_mod(&self) -> Option<(&HashMap<String, Symbol<'ctx>>, &Vec<CompoundDottedName>)> {if let Symbol::Module(x) = self {Some((x, i))} else {None}}
    pub fn as_var_mut(&mut self) -> Option<&mut Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn as_mod_mut(&mut self) -> Option<(&mut HashMap<String, Symbol<'ctx>>, &mut Vec<CompoundDottedName>)> {if let Symbol::Module(x) = self {Some((x, i))} else {None}}
    pub fn is_var(&self) -> bool {if let Symbol::Variable(_) = self {true} else {false}}
    pub fn is_mod(&self) -> bool {if let Symbol::Module(_) = self {true} else {false}}
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            Symbol::Variable(v) => if v.export {
                out.write_all(&[1])?; // Variable
                out.write_all(v.comp_val.as_ref().map(|v| v.into_pointer_value().get_name().to_bytes().to_owned()).unwrap_or_else(Vec::new).as_slice())?; // LLVM symbol name, null-terminated
                out.write_all(&[0])?;
                if let Some(v) = v.inter_val.as_ref() {v.save(out)?}
                else {out.write_all(&[0])?} // Interpreted value, self-punctuating
                v.data_type.save(out) // Type
            } else {Ok(())},
            Symbol::Module(v, i) => {
                out.write_all(&[2])?; // Module
                for (name, sym) in v.iter() {
                    out.write_all(name.as_bytes())?; // name, null-terminated
                    out.write_all(&[0])?;
                    sym.save(out)?;
                }
                out.write_all(&[0])?; // null terminator for symbol list
                for import in i.iter() {
                    import.save(out)?;
                }
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        match c {
            1 => {
                let mut var = Variable::error();
                var.export = true;
                let mut name = vec![];
                buf.read_until(0, &mut name)?;
                if name.last() == Some(&0) {name.pop();}
                var.inter_val = InterData::load(buf)?;
                var.data_type = Type::load(buf)?;
                if name.len() > 0 {
                    if let Type::Function(ret, params) = &var.data_type {
                        if let Some(llt) = ret.llvm_type(ctx) {
                            let mut good = true;
                            let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                            if good {
                                let ft = llt.fn_type(&ps, false);
                                let fv = ctx.module.add_function(std::str::from_utf8(&name).expect("LLVM function names should be valid UTF-8"), ft, None);
                                var.comp_val = Some(BasicValueEnum::PointerValue(fv.as_global_value().as_pointer_value()));
                            }
                        }
                        else if **ret == Type::Null {
                            let mut good = true;
                            let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                            if good {
                                let ft = ctx.context.void_type().fn_type(&ps, false);
                                let fv = ctx.module.add_function(std::str::from_utf8(&name).expect("LLVM function names should be valid UTF-8"), ft, None);
                                var.comp_val = Some(BasicValueEnum::PointerValue(fv.as_global_value().as_pointer_value()));
                            }
                        }
                    }
                    else if let Some(t) = var.data_type.llvm_type(ctx) {
                        let gv = ctx.module.add_global(t, None, std::str::from_utf8(&name).expect("LLVM variable names should be valid UTF-8")); // maybe do something with linkage/call convention?
                        var.comp_val = Some(BasicValueEnum::PointerValue(gv.as_pointer_value()));
                    }
                }
                Ok(Symbol::Variable(var))
            },
            2 => {
                let mut out = HashMap::new();
                let mut imports = vec![];
                loop {
                    let mut name = vec![];
                    buf.read_until(0, &mut name)?;
                    if name.last() == Some(&0) {name.pop();}
                    if name.len() == 0 {break}
                    out.insert(std::str::from_utf8(&name).expect("Cobalt symbols should be valid UTF-8").to_string(), Symbol::load(buf, ctx)?);
                }
                loop {
                    if let Some(val) = CompoundDottedName::load(buf) {imports.push(val);}
                    else {break}
                }
                Ok(Symbol::Module(out, imports))
            },
            x => panic!("read symbol type expecting 1 or 2, got {x}")
        }
    }
}
#[derive(Default)]
pub struct VarMap<'ctx> {
    pub parent: Option<Box<VarMap<'ctx>>>,
    pub symbols: HashMap<String, Symbol<'ctx>>,
    pub imports: Vec<CompoundDottedName>
}
impl<'ctx> VarMap<'ctx> {
    pub fn new(parent: Option<Box<VarMap<'ctx>>>) -> Self {VarMap {parent, ..Self::default()}}
    pub fn orphan(self) -> Self {VarMap {parent: None, ..self}}
    pub fn reparent(self, parent: Box<VarMap<'ctx>>) -> Self {VarMap {parent: Some(parent), ..self}}
    pub fn root(&self) -> &Self {self.parent.as_ref().map(|x| x.root()).unwrap_or(&self)}
    pub fn root_mut(&mut self) -> &mut Self {
        if self.parent.is_some() {self.parent.as_mut().unwrap().root_mut()}
        else {self}
    }
    pub fn merge(&mut self, other: HashMap<String, Symbol<'ctx>>) -> HashMap<DottedName, Symbol<'ctx>> {
        mod_merge(&mut self.symbols, other)
    }
    fn satisfy<'a>((symbols, imports): (&'a HashMap<String, Symbol<'ctx>>, &'a Vec<CompoundDottedName>), name: &[(String, Location)], pat: &[CompoundDottedNameSegment]) -> Option<&'a Symbol<'ctx>> {
        use CompoundDottedNameSegment::*;
        if pat.len() == 0 {return None}
        if name.len() == 1 && pat.matches(name) {return self.symbols.get(&name[0].0)}
        match &pat[0] {
            Glob(p) => None, // no
            Group(ids) => ids.iter().filter_map(|v| {
                let mut v = v.clone();
                v.extend_from_slice(&pat[1..]);
                Self::satisfy((symbols, imports), name, &v)
            }).next(),
            Identifier(id) => if name.get(0) == Some(id) {self.symbols.get(id).and_then(|v| Self::satisfy(v.as_mod()?, &name[1..], &pat[1..]))} else {None}
        }
    }
    fn self_satisfy(&self, name: &[(String, Location)], pat: &[CompoundDottedNameSegment]) -> Option<&Symbol<'ctx>> {Self::satisfy((&self.symbols, &self.imports), name, pat)}
    pub fn lookup(&self, name: &DottedName) -> Result<&Symbol<'ctx>, UndefVariable> {
        let (mut this, mut imports) = if name.global {(&self.root().symbols, &self.root().imports)} else {(&self.symbols, &self.imports)};
        let mut idx = 0;
        if name.ids.len() == 0 {panic!("mod_lookup cannot lookup an empty name")}
        while idx + 1 < name.ids.len() {
            match this.get(&name.ids[idx].0) {
                None => return imports.iter().filter_map(|i| Self::satisfy((if i.global {let root = self.root(); (&root.symbols, &root.imports)} else {(&this, &imports)}), &name.ids[(idx + 1)..], &i.ids[(idx + 1)..])).next().ok_or_else(|| self.parent.as_ref().map(|p| p.lookup(name)).unwrap_or(Err(UndefVariable::DoesNotExist(x)))),
                Some(Symbol::Variable(_)) => return Err(UndefVariable::NotAModule(idx)),
                Some(Symbol::Module(x, i)) => {
                    this = x;
                    imports = i;
                }
            }
            idx += 1;
        }
        this.get(&name.ids[idx].0).ok_or_else(|| imports.iter().filter_map(|i| Self::satisfy((if i.global {let root = self.root(); (&root.symbols, &root.imports)} else {(&this, &imports)}), &name.ids[(idx + 1)..], &i.ids[(idx + 1)..])).next()).ok_or_else(|| self.parent.as_ref().map(|p| p.lookup(name)).unwrap_or(Err(UndefVariable::DoesNotExist(x))))
    }
    pub fn insert(&mut self, name: &DottedName, sym: Symbol<'ctx>) -> Result<&Symbol<'ctx>, RedefVariable<'ctx>> {
        let this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.len() == 0 {panic!("mod_insert cannot insert a value at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Some(x) = this.entry(name.ids[idx].0.clone()).or_insert_with(|| Symbol::Module(HashMap::new(), vec![])).as_mod_mut() {this = x}
            else {return Err(RedefVariable::NotAModule(idx, sym))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol::Variable(_) => Err(RedefVariable::AlreadyExists(idx, sym)),
                Symbol::Module(m, i) => Err(RedefVariable::AlreadyExists(idx, sym))
            },
            Entry::Vacant(x) => Ok(&*x.insert(sym))
        }
    }
    pub fn insert_mod(&mut self, name: &DottedName, sym: HashMap<String, Symbol<'ctx>>) -> Result<&HashMap<String, Symbol<'ctx>>, RedefVariable<'ctx>> {
        let this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.len() == 0 {panic!("mod_insert cannot insert a value at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Some(x) = this.entry(name.ids[idx].0.clone()).or_insert_with(|| Symbol::Module(HashMap::new(), vec![])).as_mod_mut() {this = x}
            else {return Err(RedefVariable::NotAModule(idx, Symbol::Module(sym.0, sym.1)))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol::Variable(_) => Err(RedefVariable::AlreadyExists(idx, Symbol::Module(sym.0, sym.1))),
                Symbol::Module(ref mut m, ref mut i) => {
                    *m = sym.0;
                    i.append(&mut sym.1);
                    Ok(&x.into_mut().as_mod().unwrap())
                }
            },
            Entry::Vacant(x) => Ok(&x.insert(Symbol::Module(sym, vec![])).as_mod().unwrap())
        }
    }
    pub fn lookup_mod(&mut self, name: &DottedName) -> Result<HashMap<String, Symbol<'ctx>>, UndefVariable> {
        let this = if name.global {&mut self.root_mut().symbols} else {&mut self.symbols};
        let mut idx = 0;
        if name.ids.len() == 0 {panic!("mod_lookup_insert cannot find a module at an empty name")}
        while idx + 1 < name.ids.len() {
            if let Some(x) = this.entry(name.ids[idx].0.clone()).or_insert_with(|| Symbol::Module(HashMap::new(), vec![])).as_mod_mut() {this = x}
            else {return Err(UndefVariable::NotAModule(idx))}
            idx += 1;
        }
        match this.entry(name.ids[idx].0.clone()) {
            Entry::Occupied(mut x) => match x.get_mut() {
                Symbol::Variable(_) => Err(UndefVariable::DoesNotExist(idx)), // should be AlreadyExists, but DoesNotExist wouldn't arise here
                Symbol::Module(_) => Ok(x.remove().into_mod().unwrap())
            },
            Entry::Vacant(x) => Ok(x.insert(Symbol::Module(HashMap::new(), vec![])).as_mod().unwrap().clone())
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for (name, sym) in self.symbols.iter() {
            out.write_all(name.as_bytes())?;
            out.write_all(&[0])?;
            sym.save(out)?;
        }
        Ok(())
    }
    pub fn load<R: Read + BufRead>(&mut self, buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<()> {
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.len() == 0 {break}
            self.symbols.insert(std::str::from_utf8(&name).expect("Cobalt symbols should be valid UTF-8").to_string(), Symbol::load(buf, ctx)?);
        }
        loop {
            if let Some(val) = CompoundDottedName::load(buf) {self.imports.push(val);}
            else {break}
        }
        Ok(())
    }
    pub fn load_new<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut out = HashMap::new();
        let mut imports = vec![];
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.len() == 0 {break}
            out.insert(std::str::from_utf8(&name).expect("Cobalt symbols should be valid UTF-8").to_string(), Symbol::load(buf, ctx)?);
        }
        loop {
            if let Some(val) = CompoundDottedName::load(buf) {imports.push(val);}
            else {break}
        }
        Ok(VarMap {parent: None, symbols: out, imports})
    }
}
fn mod_merge<'ctx>(this: &mut HashMap<String, Symbol<'ctx>>, other: HashMap<String, Symbol<'ctx>>) -> HashMap<DottedName, Symbol<'ctx>> {
    let mut out: HashMap<DottedName, Symbol> = HashMap::new();
    for (name, sym) in other {
        match this.entry(name.clone()) {
            Entry::Occupied(mut x) => {
                if let Symbol::Module(x, i) = x.get_mut() {
                    if sym.is_mod() {out.extend(mod_merge(x, sym.into_mod().unwrap()).into_iter().map(|mut x| {x.0.ids.insert(0, (name.clone(), (0, 0..0))); x}));}
                    else {out.insert(DottedName::local((name, (0, (0..0)))), sym);}
                }
                else {out.insert(DottedName::local((name, (0, 0..0))), sym);}
            },
            Entry::Vacant(x) => {x.insert(sym);}
        }
    }
    out
}
