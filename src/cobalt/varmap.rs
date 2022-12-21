use crate::*;
use inkwell::values::BasicValueEnum;
use std::collections::hash_map::{HashMap, Entry};
use std::{cell::Cell, rc::Rc};
pub enum UndefVariable {
    NotAModule(usize),
    DoesNotExist(usize)
}
pub enum RedefVariable<'ctx> {
    NotAModule(usize, Symbol<'ctx>),
    AlreadyExists(usize, Symbol<'ctx>),
    MergeConflict(usize, HashMap<DottedName, Symbol<'ctx>>)
}
#[derive(Clone, Debug)]
pub enum InterData {
    Null,
    Int(i128),
    Float(f64),
    Str(String),
    Array(Vec<InterData>)
}
#[derive(Clone)]
pub struct Variable<'ctx> {
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData>,
    pub data_type: Type,
    pub good: Rc<Cell<bool>>
}
impl<'ctx> Variable<'ctx> {
    pub fn error() -> Self {Variable {comp_val: None, inter_val: None, data_type: Type::Null, good: Rc::new(Cell::new(false))}}
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: None, data_type, good: Rc::new(Cell::new(true))}}
    pub fn interpreted(comp_val: BasicValueEnum<'ctx>, inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: Some(comp_val), inter_val: Some(inter_val), data_type, good: Rc::new(Cell::new(true))}}
    pub fn metaval(inter_val: InterData, data_type: Type) -> Self {Variable {comp_val: None, inter_val: Some(inter_val), data_type, good: Rc::new(Cell::new(true))}}
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
            Err(UndefVariable::DoesNotExist(x)) => self.parent.as_ref().map(|p| mod_lookup(&p.symbols, name)).unwrap_or(Err(UndefVariable::DoesNotExist(x))),
            x => x
        }
    }
    pub fn insert(&mut self, name: &DottedName, sym: Symbol<'ctx>) -> Result<&Symbol<'ctx>, RedefVariable<'ctx>> {
        mod_insert(if name.global {&mut self.root_mut().symbols} else {&mut self.symbols}, name, sym)
    }
    pub fn insert_mod(&mut self, name: &DottedName, sym: HashMap<String, Symbol<'ctx>>) -> Result<&HashMap<String, Symbol<'ctx>>, RedefVariable<'ctx>> {
         mod_insert_mod(if name.global {&mut self.root_mut().symbols} else {&mut self.symbols}, name, sym)       
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
