use crate::*;
use inkwell::types::{BasicTypeEnum::*, BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;
use std::io::{self, Write, Read, BufRead};
use std::rc::Rc;
use std::cell::Cell;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodType {Normal, Static, Getter}
#[derive(Debug, Clone)]
pub struct FnData<'ctx> {
    pub defaults: Vec<InterData<'ctx>>,
    pub cconv: u32,
    pub mt: MethodType
}
#[derive(Debug, Clone)]
pub enum InterData<'ctx> {
    Null,
    Int(i128),
    Float(f64),
    Str(String),
    Array(Vec<InterData<'ctx>>),
    Function(FnData<'ctx>),
    InlineAsm(String, String),
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
                out.write_all(std::slice::from_ref(&match v.mt {
                    MethodType::Normal => 1,
                    MethodType::Static => 2,
                    MethodType::Getter => 3
                }))
            },
            InterData::InlineAsm(c, b) => {
                out.write_all(&[7])?;
                out.write_all(c.as_bytes())?;
                out.write_all(&[0])?;
                out.write_all(b.as_bytes())?;
                out.write_all(&[0])
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
                let mut c = 0u8;
                buf.read_exact(&mut std::slice::from_mut(&mut c))?;
                let mt = match c {
                    1 => MethodType::Normal,
                    2 => MethodType::Static,
                    3 => MethodType::Getter,
                    x => panic!("Expected 1, 2, or 3 for method type, got {x}")
                };
                Some(InterData::Function(FnData{defaults: vec, cconv: u32::from_be_bytes(bytes), mt}))
            },
            7 => {
                let mut constraint = Vec::new();
                let mut body = Vec::new();
                buf.read_until(0, &mut constraint)?;
                buf.read_until(0, &mut body)?;
                Some(InterData::InlineAsm(String::from_utf8(constraint).expect("Inline assmebly constraint should be valid UTF-8"), String::from_utf8(body).expect("Inline assembly should be valid UTF-8")))
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
#[derive(Debug, Clone)]
pub struct Value<'ctx> {
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData<'ctx>>,
    pub data_type: Type,
    pub address: Rc<Cell<Option<PointerValue<'ctx>>>>
}
impl<'ctx> Value<'ctx> {
    pub fn error() -> Self {Value {comp_val: None, inter_val: None, data_type: Type::Error, address: Rc::default()}}
    pub fn null() -> Self {Value {comp_val: None, inter_val: None, data_type: Type::Null, address: Rc::default()}}
    pub fn new(comp_val: Option<BasicValueEnum<'ctx>>, inter_val: Option<InterData<'ctx>>, data_type: Type) -> Self {Value {comp_val, inter_val, data_type, address: Rc::default()}}
    pub fn with_addr(comp_val: Option<BasicValueEnum<'ctx>>, inter_val: Option<InterData<'ctx>>, data_type: Type, addr: PointerValue<'ctx>) -> Self {Value {comp_val, inter_val, data_type, address: Rc::new(Cell::new(Some(addr)))}}
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: Type) -> Self {Value {comp_val: Some(comp_val), inter_val: None, data_type, address: Rc::default()}}
    pub fn interpreted(comp_val: BasicValueEnum<'ctx>, inter_val: InterData<'ctx>, data_type: Type) -> Self {Value {comp_val: Some(comp_val), inter_val: Some(inter_val), data_type, address: Rc::default()}}
    pub fn metaval(inter_val: InterData<'ctx>, data_type: Type) -> Self {Value {comp_val: None, inter_val: Some(inter_val), data_type, address: Rc::default()}}
    pub fn make_type(type_: Type) -> Self {Value {comp_val: None, inter_val: Some(InterData::Type(Box::new(type_))), data_type: Type::TypeData, address: Rc::default()}}
    pub fn empty_mod() -> Self {Value {comp_val: None, inter_val: Some(InterData::Module(HashMap::new(), vec![])), data_type: Type::Module, address: Rc::default()}}
    pub fn make_mod(syms: HashMap<String, Symbol<'ctx>>, imps: Vec<(CompoundDottedName, bool)>) -> Self {Value {comp_val: None, inter_val: Some(InterData::Module(syms, imps)), data_type: Type::Module, address: Rc::default()}}
    
    pub fn addr(&self, ctx: &CompCtx<'ctx>) -> Option<PointerValue<'ctx>> {
        self.address.get().or_else(|| {
            let ctv = self.value(ctx)?;
            let alloca = ctx.builder.build_alloca(ctv.get_type(), "");
            ctx.builder.build_store(alloca, ctv);
            self.address.set(Some(alloca));
            Some(alloca)
        })
    }

    pub fn value(&self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}
    pub fn into_value(self, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {self.comp_val.or_else(|| self.inter_val.as_ref().and_then(|v| v.into_compiled(ctx)))}

    pub fn into_type(self) -> Option<Type> {if let Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(t)), ..} = self {Some(*t)} else {None}}
    pub fn as_type(&self) -> Option<&Type> {if let Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(t)), ..} = self {Some(t.as_ref())} else {None}}
    pub fn into_mod(self) -> Option<(HashMap<String, Symbol<'ctx>>, Vec<(CompoundDottedName, bool)>)> {if let Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, m)), ..} = self {Some((s, m))} else {None}}
    pub fn as_mod(&self) -> Option<(&HashMap<String, Symbol<'ctx>>, &Vec<(CompoundDottedName, bool)>)> {if let Value {data_type: Type::Module, inter_val: Some(InterData::Module(s, m)), ..} = self {Some((s, m))} else {None}}

    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        out.write_all(self.comp_val.as_ref().map(|v| v.into_pointer_value().get_name().to_bytes().to_owned()).unwrap_or_else(Vec::new).as_slice())?; // LLVM symbol name, null-terminated
        out.write_all(&[0])?;
        if let Some(v) = self.inter_val.as_ref() {v.save(out)?}
        else {out.write_all(&[0])?} // Interpreted value, self-punctuating
        self.data_type.save(out) // Type
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
        Ok(var)
    }
}
