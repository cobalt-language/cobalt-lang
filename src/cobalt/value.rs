use crate::*;
use inkwell::values::BasicValueEnum;
use std::collections::HashMap;
use std::io::{self, Write, Read, BufRead};
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
                Ok(())
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
                Some(InterData::Function(FnData{defaults: vec, cconv: u32::from_be_bytes(bytes)}))
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
