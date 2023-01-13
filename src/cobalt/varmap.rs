use crate::*;
use inkwell::values::BasicValueEnum;
use inkwell::types::{BasicTypeEnum::*, BasicMetadataTypeEnum, BasicType};
use std::collections::hash_map::{HashMap, Entry};
use std::cell::Cell;
use std::io::{self, Write, Read, BufRead};
pub enum UndefVariable {
    NotAModule(usize),
    DoesNotExist(usize)
}
pub enum RedefVariable<'ctx> {
    NotAModule(usize, Symbol<'ctx>),
    AlreadyExists(usize, Symbol<'ctx>),
    MergeConflict(usize, HashMap<DottedName, Symbol<'ctx>>)
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
    Function(FnData)
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
            x => panic!("read interpreted data type expecting number in 1..=6, got {x}")
        })
    }
}
#[derive(Clone)]
pub struct Variable<'ctx> {
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData>,
    pub data_type: Type,
    pub good: Cell<bool>
}
impl<'ctx> Variable<'ctx> {
    pub fn error() -> Self {Variable {comp_val: None, inter_val: None, data_type: Type::Null, good: Cell::new(false)}}
    pub fn null(data_type: Option<Type>) -> Self {Variable {comp_val: None, inter_val: None, data_type: data_type.unwrap_or(Type::Null), good: Cell::new(false)}}
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: None, data_type, good: Cell::new(true)}}
    pub fn interpreted(comp_val: BasicValueEnum<'ctx>, inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: Some(inter_val), data_type, good: Cell::new(true)}}
    pub fn metaval(inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: None, inter_val: Some(inter_val), data_type, good: Cell::new(true)}}
    pub fn value(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.clone().or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}
}
pub enum Symbol<'ctx> {
    Variable(Variable<'ctx>),
    Module(HashMap<String, Symbol<'ctx>>)
}
impl<'ctx> Symbol<'ctx> {
    pub fn into_var(self) -> Option<Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn into_mod(self) -> Option<HashMap<String, Symbol<'ctx>>> {if let Symbol::Module(x) = self {Some(x)} else {None}}
    pub fn as_var(&self) -> Option<&Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn as_mod(&self) -> Option<&HashMap<String, Symbol<'ctx>>> {if let Symbol::Module(x) = self {Some(x)} else {None}}
    pub fn as_var_mut(&mut self) -> Option<&mut Variable<'ctx>> {if let Symbol::Variable(x) = self {Some(x)} else {None}}
    pub fn as_mod_mut(&mut self) -> Option<&mut HashMap<String, Symbol<'ctx>>> {if let Symbol::Module(x) = self {Some(x)} else {None}}
    pub fn is_var(&self) -> bool {if let Symbol::Variable(_) = self {true} else {false}}
    pub fn is_mod(&self) -> bool {if let Symbol::Module(_) = self {true} else {false}}
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            Symbol::Variable(v) => {
                out.write_all(&[1])?; // Variable
                out.write_all(v.comp_val.as_ref().map(|v| v.into_pointer_value().get_name().to_bytes().to_owned()).unwrap_or_else(Vec::new).as_slice())?; // LLVM symbol name, null-terminated
                out.write_all(&[0])?;
                if let Some(v) = v.inter_val.as_ref() {v.save(out)?}
                else {out.write_all(&[0])?} // Interpreted value, self-punctuating
                v.data_type.save(out) // Type
            },
            Symbol::Module(v) => {
                out.write_all(&[2])?; // Module
                for (name, sym) in v.iter() {
                    out.write_all(name.as_bytes())?; // name, null-terminated
                    out.write_all(&[0])?;
                    sym.save(out)?;
                }
                out.write_all(&[0]) // null terminator for symbol list
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        match c {
            1 => {
                let mut var = Variable::error();
                var.good.set(true);
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
                loop {
                    let mut name = vec![];
                    buf.read_until(0, &mut name)?;
                    if name.last() == Some(&0) {name.pop();}
                    if name.len() == 0 {break}
                    out.insert(std::str::from_utf8(&name).expect("Cobalt symbols should be valid UTF-8").to_string(), Symbol::load(buf, ctx)?);
                }
                Ok(Symbol::Module(out))
            },
            x => panic!("read symbol type expecting 1 or 2, got {x}")
        }
    }
}
#[derive(Default)]
pub struct VarMap<'ctx> {
    pub parent: Option<Box<VarMap<'ctx>>>,
    pub symbols: HashMap<String, Symbol<'ctx>>
}
impl<'ctx> VarMap<'ctx> {
    pub fn new(parent: Option<Box<VarMap<'ctx>>>) -> Self {VarMap {parent, symbols: HashMap::new()}}
    pub fn orphan(self) -> Self {VarMap {parent: None, symbols: self.symbols}}
    pub fn reparent(self, parent: Box<VarMap<'ctx>>) -> Self {VarMap {parent: Some(parent), symbols: self.symbols}}
    pub fn root(&self) -> &Self {self.parent.as_ref().map(|x| x.root()).unwrap_or(&self)}
    pub fn root_mut(&mut self) -> &mut Self {
        if self.parent.is_some() {self.parent.as_mut().unwrap().root_mut()}
        else {self}
    }
    pub fn merge(&mut self, other: HashMap<String, Symbol<'ctx>>) -> HashMap<DottedName, Symbol<'ctx>> {
        mod_merge(&mut self.symbols, other)
    }
    pub fn lookup(&self, name: &DottedName) -> Result<&Symbol<'ctx>, UndefVariable> {
        match mod_lookup(if name.global {&self.root().symbols} else {&self.symbols}, name) {
            Err(UndefVariable::DoesNotExist(x)) => self.parent.as_ref().map(|p| p.lookup(name)).unwrap_or(Err(UndefVariable::DoesNotExist(x))),
            x => x
        }
    }
    pub fn insert(&mut self, name: &DottedName, sym: Symbol<'ctx>) -> Result<&Symbol<'ctx>, RedefVariable<'ctx>> {
        mod_insert(if name.global {&mut self.root_mut().symbols} else {&mut self.symbols}, name, sym)
    }
    pub fn insert_mod(&mut self, name: &DottedName, sym: HashMap<String, Symbol<'ctx>>) -> Result<&HashMap<String, Symbol<'ctx>>, RedefVariable<'ctx>> {
         mod_insert_mod(if name.global {&mut self.root_mut().symbols} else {&mut self.symbols}, name, sym)
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
        Ok(())
    }
    pub fn load_new<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut out = HashMap::new();
        loop {
            let mut name = vec![];
            buf.read_until(0, &mut name)?;
            if name.last() == Some(&0) {name.pop();}
            if name.len() == 0 {break}
            out.insert(std::str::from_utf8(&name).expect("Cobalt symbols should be valid UTF-8").to_string(), Symbol::load(buf, ctx)?);
        }
        Ok(VarMap {parent: None, symbols: out})
    }
}
pub fn mod_lookup<'a, 'ctx>(mut this: &'a HashMap<String, Symbol<'ctx>>, name: &DottedName) -> Result<&'a Symbol<'ctx>, UndefVariable> {
    let mut idx = 0;
    if name.ids.len() == 0 {panic!("mod_lookup cannot lookup an empty name")}
    while idx + 1 < name.ids.len() {
        match this.get(&name.ids[idx]) {
            None => return Err(UndefVariable::DoesNotExist(idx)),
            Some(Symbol::Variable(_)) => return Err(UndefVariable::NotAModule(idx)),
            Some(Symbol::Module(x)) => this = x
        }
        idx += 1;
    }
    this.get(&name.ids[idx]).ok_or(UndefVariable::DoesNotExist(idx))
}
pub fn mod_insert<'a, 'ctx>(mut this: &'a mut HashMap<String, Symbol<'ctx>>, name: &DottedName, sym: Symbol<'ctx>) -> Result<&'a Symbol<'ctx>, RedefVariable<'ctx>> {
    let mut idx = 0;
    if name.ids.len() == 0 {panic!("mod_insert cannot insert a value at an empty name")}
    while idx + 1 < name.ids.len() {
        if let Some(x) = this.entry(name.ids[idx].clone()).or_insert_with(|| Symbol::Module(HashMap::new())).as_mod_mut() {this = x}
        else {return Err(RedefVariable::NotAModule(idx, sym))}
        idx += 1;
    }
    match this.entry(name.ids[idx].clone()) {
        Entry::Occupied(mut x) => match x.get_mut() {
            Symbol::Variable(_) => Err(RedefVariable::AlreadyExists(idx, sym)),
            Symbol::Module(m) => {
                if sym.is_var() {Err(RedefVariable::AlreadyExists(idx, sym))}
                else {
                    let errs = mod_merge(m, sym.into_mod().unwrap());
                    if errs.len() == 0 {Ok(&*x.into_mut())}
                    else {Err(RedefVariable::MergeConflict(idx, errs))}
                }
            }
        },
        Entry::Vacant(x) => Ok(&*x.insert(sym))
    }
}
pub fn mod_insert_mod<'a, 'ctx>(mut this: &'a mut HashMap<String, Symbol<'ctx>>, name: &DottedName, sym: HashMap<String, Symbol<'ctx>>) -> Result<&'a HashMap<String, Symbol<'ctx>>, RedefVariable<'ctx>> {
    let mut idx = 0;
    if name.ids.len() == 0 {panic!("mod_insert cannot insert a value at an empty name")}
    while idx + 1 < name.ids.len() {
        if let Some(x) = this.entry(name.ids[idx].clone()).or_insert_with(|| Symbol::Module(HashMap::new())).as_mod_mut() {this = x}
        else {return Err(RedefVariable::NotAModule(idx, Symbol::Module(sym)))}
        idx += 1;
    }
    match this.entry(name.ids[idx].clone()) {
        Entry::Occupied(mut x) => match x.get_mut() {
            Symbol::Variable(_) => Err(RedefVariable::AlreadyExists(idx, Symbol::Module(sym))),
            Symbol::Module(ref mut m) => {
                    let errs = mod_merge(m, sym);
                    if errs.len() == 0 {Ok(&x.into_mut().as_mod().unwrap())}
                    else {Err(RedefVariable::MergeConflict(idx, errs))}
            }
        },
        Entry::Vacant(x) => Ok(&x.insert(Symbol::Module(sym)).as_mod().unwrap())
    }
}
pub fn mod_merge<'ctx>(this: &mut HashMap<String, Symbol<'ctx>>, other: HashMap<String, Symbol<'ctx>>) -> HashMap<DottedName, Symbol<'ctx>> {
    let mut out: HashMap<DottedName, Symbol> = HashMap::new();
    for (name, sym) in other {
        match this.entry(name.clone()) {
            Entry::Occupied(mut x) => {
                if let Symbol::Module(x) = x.get_mut() {
                    if sym.is_mod() {out.extend(mod_merge(x, sym.into_mod().unwrap()).into_iter().map(|mut x| {x.0.ids.insert(0, name.clone()); x}));}
                    else {out.insert(DottedName::local(name), sym);}
                }
                else {out.insert(DottedName::local(name), sym);}
            },
            Entry::Vacant(x) => {x.insert(sym);}
        }
    }
    out
}
