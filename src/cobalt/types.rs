use inkwell::types::{BasicType, BasicTypeEnum::{self, *}};
use crate::*;
use Type::{*, Error};
use SizeType::*;
use std::fmt::*;
use std::io::{self, Write, Read, BufRead};
use std::sync::RwLock;
use std::collections::HashMap;
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum SizeType {
    Static(u32),
    Dynamic,
    Meta
}
impl SizeType {
    pub fn is_static(self) -> bool {matches!(self, Static(_))}
    pub fn is_dynamic(self) -> bool {self == Dynamic}
    pub fn is_meta(self) -> bool {self == Meta}
    pub fn as_static(self) -> Option<u32> {if let Static(x) = self {Some(x)} else {None}}
    pub fn map_static<F: FnOnce(u32) -> u32>(self, f: F) -> SizeType {if let Static(x) = self {Static(f(x))} else {self}}
}
impl Display for SizeType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            SizeType::Static(size) => write!(f, "{size}"),
            SizeType::Dynamic => write!(f, "dynamic"),
            SizeType::Meta => write!(f, "meta")
        }
    }
}
lazy_static::lazy_static! {
    pub static ref NOMINAL_TYPES: RwLock<HashMap<String, (Type, bool)>> = RwLock::new(HashMap::new());
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    IntLiteral, Char,
    Int(u16, bool),
    Float16, Float32, Float64, Float128,
    Pointer(Box<Type>, bool), Reference(Box<Type>, bool), Borrow(Box<Type>),
    Null, Module, TypeData, InlineAsm(Box<Type>), Array(Box<Type>, Option<u32>),
    Function(Box<Type>, Vec<(Type, bool)>), Nominal(String),
    Error
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            IntLiteral => write!(f, "<int literal>"),
            Int(1, _) => write!(f, "bool"),
            Int(size, false) => write!(f, "i{size}"),
            Int(size, true) => write!(f, "u{size}"),
            Char => write!(f, "char"),
            Float16 => write!(f, "f16"),
            Float32 => write!(f, "f32"),
            Float64 => write!(f, "f64"),
            Float128 => write!(f, "f128"),
            Pointer(x, false) => write!(f, "{} const*", *x),
            Pointer(x, true) => write!(f, "{} mut*", *x),
            Reference(x, false) => write!(f, "{} const&", *x),
            Reference(x, true) => write!(f, "{} mut&", *x),
            Borrow(x) => write!(f, "{}^", *x),
            Null => write!(f, "null"),
            Module => write!(f, "module"),
            TypeData => write!(f, "type"),
            InlineAsm(x) => write!(f, "inline assembly (returing {})", *x),
            Array(x, None) => write!(f, "{}[]", *x),
            Array(x, Some(s)) => write!(f, "{}[{s}]", *x),
            Function(ret, args) => {
                write!(f, "fn (")?;
                let mut len = args.len();
                for (arg, ty) in args.iter() {
                    write!(f, "{}{}", match ty {
                        true => "const ",
                        false => ""
                    }, arg)?;
                    if len > 1 {write!(f, ", ")?}
                    len -= 1;
                }
                write!(f, "): {}", *ret)
            },
            Nominal(n) => write!(f, "{n}"),
            Error => write!(f, "<error>")
        }
    }
}
impl Type {
    pub fn size(&self) -> SizeType {
        match self {
            IntLiteral => Static(8),
            Int(size, _) => Static(((size + 7) / 8) as u32),
            Char => Static(4),
            Float16 => Static(2),
            Float32 => Static(4),
            Float64 => Static(8),
            Float128 => Static(16),
            Null => Static(0),
            Array(b, Some(s)) => b.size().map_static(|x| x * s),
            Array(_, None) => Dynamic,
            Function(..) | Module | TypeData | InlineAsm(_) | Error => Meta,
            Pointer(..) | Reference(..) => Static(8),
            Borrow(b) => b.size(),
            Nominal(n) => NOMINAL_TYPES.read().expect("Value should not be poisoned!")[n].0.size()
        }
    }
    pub fn align(&self) -> u16 {
        match self {
            IntLiteral => 8,
            Int(size, _) => match size {
                0..=8 => 1,
                9..=16 => 2,
                17..=32 => 4,
                33.. => 8
            },
            Char => 4,
            Float16 => 2,
            Float32 => 4,
            Float64 | Float128 => 8,
            Null => 1,
            Array(b, _) => b.align(),
            Function(..) | Module | TypeData | InlineAsm(_) | Error => 0,
            Pointer(..) | Reference(..) => 8,
            Borrow(b) => b.align(),
            Nominal(n) => NOMINAL_TYPES.read().expect("Value should not be poisoned!")[n].0.align()
        }
    }
    pub fn llvm_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            IntLiteral => Some(IntType(ctx.context.i64_type())),
            Int(size, _) => Some(IntType(ctx.context.custom_width_int_type(*size as u32))),
            Char => Some(IntType(ctx.context.i32_type())),
            Float16 => Some(FloatType(ctx.context.f16_type())),
            Float32 => Some(FloatType(ctx.context.f32_type())),
            Float64 => Some(FloatType(ctx.context.f64_type())),
            Float128 => Some(FloatType(ctx.context.f128_type())),
            Null | Function(..) | Module | TypeData | InlineAsm(_) | Error => None,
            Array(_, Some(_)) => None,
            Array(_, None) => None,
            Pointer(b, _) | Reference(b, _) => match &**b {
                Type::Array(b, None) => Some(ctx.context.struct_type(&[b.llvm_type(ctx)?.ptr_type(Default::default()).into(), ctx.context.i64_type().into()], false).into()),
                Type::Array(b, Some(_)) => Some(b.llvm_type(ctx)?.ptr_type(Default::default()).into()),
                b => if b.size() == Static(0) {Some(PointerType(ctx.null_type.ptr_type(Default::default())))} else {Some(PointerType(b.llvm_type(ctx)?.ptr_type(Default::default())))}
            },
            Borrow(b) => b.llvm_type(ctx),
            Nominal(n) => NOMINAL_TYPES.read().expect("Value should not be poisoned!")[n].0.llvm_type(ctx)
        }
    }
    pub fn register(&self) -> bool {
        match self {
            IntLiteral | Int(_, _) | Char | Float16 | Float32 | Float64 | Float128 | Null | Function(..) | Pointer(..) | Reference(..) => true,
            Borrow(b) => b.register(),
            Nominal(n) => NOMINAL_TYPES.read().expect("Value should not be poisoned!")[n].0.register(),
            _ => false
        }
    }
    pub fn copyable(&self) -> bool {
        match self {
            IntLiteral | Int(_, _) | Char | Float16 | Float32 | Float64 | Float128 | Null | Function(..) | Pointer(..) | Reference(..) | Borrow(_) => true,
            Nominal(n) => NOMINAL_TYPES.read().expect("Value should not be poisoned!")[n].0.copyable(),
            _ => false
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            IntLiteral => panic!("There shouldn't be an int literal in a variable!"),
            Int(s, u) => {
                out.write_all(&[1])?;
                let mut v = *s as i16;
                if *u {v = -v;}
                out.write_all(&v.to_be_bytes())
            },
            Char => out.write_all(&[2]),
            Float16 => out.write_all(&[3]),
            Float32 => out.write_all(&[4]),
            Float64 => out.write_all(&[5]),
            Float128 => out.write_all(&[6]),
            Null => out.write_all(&[7]),
            Pointer(b, false) => {
                out.write_all(&[8])?;
                b.save(out)
            },
            Pointer(b, true) => {
                out.write_all(&[9])?;
                b.save(out)
            },
            Reference(b, false) => {
                out.write_all(&[10])?;
                b.save(out)
            },
            Reference(b, true) => {
                out.write_all(&[11])?;
                b.save(out)
            },
            Borrow(b) => {
                out.write_all(&[12])?;
                b.save(out)
            },
            Function(b, p) => {
                out.write_all(&[13])?;
                out.write_all(&(p.len() as u16).to_be_bytes())?; // # of params
                b.save(out)?;
                for (par, c) in p {
                    par.save(out)?;
                    out.write_all(&[u8::from(*c)])?; // param is const
                }
                Ok(())
            },
            InlineAsm(b) => {
                out.write_all(&[14])?;
                b.save(out)
            },
            Error => panic!("error values shouldn't be serialized!"),
            Module => out.write_all(&[19]),
            Array(b, None) => {
                out.write_all(&[15])?;
                b.save(out)
            },
            Array(b, Some(s)) => {
                let mut data = [16u8, 0, 0, 0, 0];
                data[1..].copy_from_slice(&s.to_be_bytes());
                out.write_all(&data)?;
                b.save(out)
            },
            TypeData => out.write_all(&[17]),
            Nominal(n) => {
                out.write_all(&[18])?;
                out.write_all(n.as_bytes())?;
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R) -> io::Result<Self> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        Ok(match c {
            1 => {
                let mut bytes = [0; 2];
                buf.read_exact(&mut bytes)?;
                let v = i16::from_be_bytes(bytes);
                Type::Int(v.unsigned_abs(), v < 0)
            },
            2 => Type::Char,
            3 => Type::Float16,
            4 => Type::Float32,
            5 => Type::Float64,
            6 => Type::Float128,
            7 => Type::Null,
            8 => Type::Pointer(Box::new(Type::load(buf)?), false),
            9 => Type::Pointer(Box::new(Type::load(buf)?), true),
            10 => Type::Reference(Box::new(Type::load(buf)?), false),
            11 => Type::Reference(Box::new(Type::load(buf)?), true),
            12 => Type::Borrow(Box::new(Type::load(buf)?)),
            13 => {
                let mut bytes = [0; 2];
                buf.read_exact(&mut bytes)?;
                let v = u16::from_be_bytes(bytes);
                let ret = Type::load(buf)?;
                let mut vec = Vec::with_capacity(v as usize);
                for _ in 0..v {
                    let t = Type::load(buf)?;
                    buf.read_exact(std::slice::from_mut(&mut c))?;
                    vec.push((t, c != 0));
                }
                Type::Function(Box::new(ret), vec)
            }
            14 => Type::InlineAsm(Box::new(Type::load(buf)?)),
            15 => Type::Array(Box::new(Type::load(buf)?), None),
            16 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                Type::Array(Box::new(Type::load(buf)?), Some(u32::from_be_bytes(bytes)))
            },
            17 => Type::TypeData,
            18 => {
                let mut vec = Vec::<u8>::new();
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {vec.pop();}
                Type::Nominal(String::from_utf8(vec).expect("Type names should be valid UTF-8!"))
            },
            19 => Type::Module,
            x => panic!("read type value expecting value in 1..=19, got {x}")
        })
    }
}
pub mod utils;
