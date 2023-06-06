use crate::*;
use inkwell::types::{BasicType, BasicTypeEnum::{self, *}, BasicMetadataTypeEnum};
use inkwell::values::BasicValueEnum;
use Type::{*, Error};
use SizeType::*;
use std::fmt::*;
use std::io::{self, Write, Read, BufRead};
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
impl std::ops::Add for SizeType {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (SizeType::Meta, _) | (_, SizeType::Meta) => SizeType::Meta,
            (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => SizeType::Dynamic,
            (SizeType::Static(l), SizeType::Static(r)) => SizeType::Static(l + r)
        }
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    IntLiteral, Intrinsic(String),
    Int(u16, bool),
    Float16, Float32, Float64, Float128,
    Pointer(Box<Type>, bool), Reference(Box<Type>, bool), Borrow(Box<Type>),
    Null, Module, TypeData, InlineAsm(Box<Type>), Array(Box<Type>, Option<u32>),
    Function(Box<Type>, Vec<(Type, bool)>), BoundMethod(Box<Type>, Box<Type>, Vec<(Type, bool)>, bool), Nominal(String),
    Tuple(Vec<Type>),
    Error
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            IntLiteral => write!(f, "<int literal>"),
            Int(1, _) => write!(f, "bool"),
            Int(size, false) => write!(f, "i{size}"),
            Int(size, true) => write!(f, "u{size}"),
            Intrinsic(s) => write!(f, "@{s}"),
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
            BoundMethod(base, ret, args, m) => {
                write!(f, "{} {base}.fn (", if *m {"mut"} else {"const"})?;
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
            Tuple(v) => {
                write!(f, "(")?;
                let mut it = v.iter().peekable();
                while let Some(v) = it.next() {
                    write!(f, "{v}")?;
                    if it.peek().is_some() {write!(f, ", ")?;}
                }
                write!(f, ")")
            }
            Error => write!(f, "<error>")
        }
    }
}
impl Type {
    pub fn size<'ctx>(&self, ctx: &CompCtx<'ctx>) -> SizeType {
        match self {
            IntLiteral => Static(8),
            Int(size, _) => Static(((size + 7) / 8) as u32),
            Float16 => Static(2),
            Float32 => Static(4),
            Float64 => Static(8),
            Float128 => Static(16),
            Null => Static(0),
            Array(b, Some(s)) => b.size(ctx).map_static(|x| x * s),
            Array(_, None) => Dynamic,
            Function(..) | Module | TypeData | InlineAsm(_) | Intrinsic(_) | Error => Meta,
            BoundMethod(..) => Static(ctx.flags.word_size as u32 * 2),
            Pointer(b, _) | Reference(b, _) => match **b {
                Type::Array(_, None) => Static(ctx.flags.word_size as u32 * 2),
                _ => Static(ctx.flags.word_size as u32)
            },
            Borrow(b) => b.size(ctx),
            Tuple(v) => v.iter().fold(Static(0), |v, t| if let Static(bs) = v {
                let n = t.size(ctx);
                if let Static(ns) = n {
                    let a = t.align(ctx) as u32;
                    if a == 0 || a == 1 {Static(bs + ns)}
                    else {Static(((bs + a - 1) / a) * a + ns)}
                } else {n}
            } else {v}),
            Nominal(n) => ctx.nominals.borrow()[n].0.size(ctx)
        }
    }
    pub fn align<'ctx>(&self, ctx: &CompCtx<'ctx>) -> u16 {
        match self {
            IntLiteral => 8,
            Int(size, _) => match size {
                0..=8 => 1,
                9..=16 => 2,
                17..=32 => 4,
                33.. => 8
            },
            Float16 => 2,
            Float32 => 4,
            Float64 | Float128 => 8,
            Null => 1,
            Array(b, _) => b.align(ctx),
            Function(..) | Module | TypeData | InlineAsm(_) | Intrinsic(_) | Error => 0,
            Pointer(..) | Reference(..) | BoundMethod(..) => ctx.flags.word_size,
            Borrow(b) => b.align(ctx),
            Tuple(v) => v.iter().map(|x| x.align(ctx)).max().unwrap_or(1),
            Nominal(n) => ctx.nominals.borrow()[n].0.align(ctx)
        }
    }
    pub fn llvm_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            IntLiteral => Some(IntType(ctx.context.i64_type())),
            Int(size, _) => Some(IntType(ctx.context.custom_width_int_type(*size as u32))),
            Float16 => Some(FloatType(ctx.context.f16_type())),
            Float32 => Some(FloatType(ctx.context.f32_type())),
            Float64 => Some(FloatType(ctx.context.f64_type())),
            Float128 => Some(FloatType(ctx.context.f128_type())),
            Null | Function(..) | Module | TypeData | InlineAsm(_) | Intrinsic(_) | Error => None,
            BoundMethod(base, ret, params, _) => Some(ctx.context.struct_type(&[Type::Pointer(base.clone(), false).llvm_type(ctx)?, Type::Pointer(Box::new(Type::Function(ret.clone(), params.clone())), false).llvm_type(ctx)?], false).into()),
            Array(_, Some(_)) => None,
            Array(_, None) => None,
            Pointer(b, _) | Reference(b, _) => match &**b {
                Type::Array(b, None) => Some(ctx.context.struct_type(&[b.llvm_type(ctx)?.ptr_type(Default::default()).into(), ctx.context.i64_type().into()], false).into()),
                Type::Array(b, Some(_)) => Some(b.llvm_type(ctx)?.ptr_type(Default::default()).into()),
                Type::Function(r, p) => {
                    let mut args = Vec::<BasicMetadataTypeEnum>::with_capacity(p.len());
                    for (p, c) in p {if !c {args.push(p.llvm_type(ctx)?.into());}}
                    Some(if r.size(ctx) == Static(0) {ctx.context.void_type().fn_type(&args, false)} else {r.llvm_type(ctx)?.fn_type(&args, false)}.ptr_type(Default::default()).into())
                },
                b => if b.size(ctx) == Static(0) {Some(PointerType(ctx.null_type.ptr_type(Default::default())))} else {Some(PointerType(b.llvm_type(ctx)?.ptr_type(Default::default())))}
            },
            Borrow(b) => b.llvm_type(ctx),
            Tuple(v) => {
                let mut vec = Vec::with_capacity(v.len());
                for t in v {vec.push(t.llvm_type(ctx)?);}
                Some(ctx.context.struct_type(&vec, false).into())
            },
            Nominal(n) => ctx.nominals.borrow()[n].0.llvm_type(ctx)
        }
    }
    pub fn register<'ctx>(&self, ctx: &CompCtx<'ctx>) -> bool {
        match self {
            IntLiteral | Int(_, _) | Float16 | Float32 | Float64 | Float128 | Null | Function(..) | Pointer(..) | Reference(..) | BoundMethod(..) => true,
            Borrow(b) => b.register(ctx),
            Tuple(v) => v.iter().all(|x| x.register(ctx)),
            Nominal(n) => ctx.nominals.borrow()[n].0.register(ctx),
            _ => false
        }
    }
    pub fn copyable<'ctx>(&self, ctx: &CompCtx<'ctx>) -> bool {
        match self {
            IntLiteral | Int(_, _) | Float16 | Float32 | Float64 | Float128 | Null | Function(..) | Pointer(..) | Reference(..) | Borrow(_) | BoundMethod(..) => true,
            Tuple(v) => v.iter().all(|x| x.copyable(ctx)),
            Nominal(n) => ctx.nominals.borrow()[n].0.copyable(ctx),
            _ => false
        }
    }
    fn can_be_compiled<'ctx>(&self, v: &InterData<'ctx>, ctx: &CompCtx<'ctx>) -> bool {
        match self {
            Type::IntLiteral | Type::Int(..) => matches!(v, InterData::Int(..)),
            Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 => matches!(v, InterData::Float(..)),
            Type::Tuple(vs) => if let InterData::Array(is) = v {vs.len() == is.len() && vs.iter().zip(is).all(|(t, v)| t.can_be_compiled(v, ctx))} else {false},
            Type::Nominal(n) => ctx.nominals.borrow()[n].0.can_be_compiled(v, ctx),
            _ => false
        }
    }
    pub fn into_compiled<'ctx>(&self, v: &InterData<'ctx>, ctx: &CompCtx<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        if ctx.is_const.get() {return None}
        match self {
            Type::IntLiteral => if let InterData::Int(v) = v {Some(ctx.context.i64_type().const_int(*v as u64, true).into())} else {None},
            Type::Int(s, u) => if let InterData::Int(v) = v {Some(ctx.context.custom_width_int_type(*s as u32).const_int(*v as u64, *u).into())} else {None},
            Type::Float16 => if let InterData::Float(v) = v {Some(ctx.context.f16_type().const_float(*v).into())} else {None},
            Type::Float32 => if let InterData::Float(v) = v {Some(ctx.context.f32_type().const_float(*v).into())} else {None},
            Type::Float64 => if let InterData::Float(v) = v {Some(ctx.context.f64_type().const_float(*v).into())} else {None},
            Type::Float128 => if let InterData::Float(v) = v {Some(ctx.context.f128_type().const_float(*v).into())} else {None},
            Type::Tuple(vs) => {
                if !self.can_be_compiled(v, ctx) {return None}
                if let InterData::Array(is) = v {
                    let (vals, types): (Vec<_>, Vec<_>) = vs.iter().zip(is).map(|(t, v)| {
                        let llv = t.into_compiled(v, ctx).unwrap();
                        (llv, llv.get_type())
                    }).unzip();
                    let ty = ctx.context.struct_type(&types, false);
                    vals.into_iter().enumerate().try_fold(ty.get_undef(), |v, (n, val)| ctx.builder.build_insert_value(v, val, n as u32, "").map(|v| v.into_struct_value())).map(From::from)
                }
                else {unreachable!()}
            },
            Type::Nominal(n) => ctx.nominals.borrow()[n].0.into_compiled(v, ctx),
            _ => None
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
            Intrinsic(s) => {
                out.write_all(&[2])?;
                out.write_all(s.as_bytes())?;
                out.write_all(&[0])
            },
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
            },
            Module => out.write_all(&[19]),
            Tuple(v) => {
                let mut buf = [20u8, 0, 0, 0, 0];
                buf[1..].copy_from_slice(&(v.len() as u32).to_be_bytes());
                out.write_all(&buf)?;
                v.iter().try_for_each(|t| t.save(out))
            },
            BoundMethod(b, r, p, m) => {
                out.write_all(&[21])?;
                out.write_all(&(p.len() as u16).to_be_bytes())?; // # of params
                b.save(out)?;
                r.save(out)?;
                out.write_all(&[u8::from(*m)])?;
                for (par, c) in p {
                    par.save(out)?;
                    out.write_all(&[u8::from(*c)])?; // param is const
                }
                Ok(())
            },
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
            2 => {
                let mut vec = vec![];
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {vec.pop();}
                Type::Intrinsic(String::from_utf8(vec).expect("intrinsic name should be valid UTF-8"))
            },
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
            20 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                let mut count = u32::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(count as usize);
                while count > 0 {
                    vec.push(Type::load(buf)?);
                    count -= 1;
                }
                Type::Tuple(vec)
            },
            21 => {
                let mut bytes = [0; 2];
                buf.read_exact(&mut bytes)?;
                let v = u16::from_be_bytes(bytes);
                let base = Type::load(buf)?;
                let ret = Type::load(buf)?;
                buf.read_exact(&mut bytes[..1])?;
                let mut vec = Vec::with_capacity(v as usize);
                for _ in 0..v {
                    let t = Type::load(buf)?;
                    buf.read_exact(std::slice::from_mut(&mut c))?;
                    vec.push((t, c != 0));
                }
                Type::BoundMethod(Box::new(base), Box::new(ret), vec, bytes[0] != 0)
            }
            x => panic!("read type value expecting value in 1..=21, got {x}")
        })
    }
}
pub fn tuple_type<'ctx>(v: &[Type], ctx: &CompCtx<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    let mut vec = Vec::with_capacity(v.len());
    for t in v {vec.push(t.llvm_type(ctx)?);}
    Some(ctx.context.struct_type(&vec, false).into())
}
pub mod utils;