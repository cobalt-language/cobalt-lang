use crate::*;
use cobalt_llvm::inkwell::values::FunctionValue;
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType,
    BasicTypeEnum::{self, *},
};
use inkwell::values::BasicValueEnum;
use std::borrow::Cow;
use std::fmt::*;
use std::io::{self, BufRead, Read, Write};
use SizeType::*;
use Type::{Error, *};
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum SizeType {
    Static(u32),
    Dynamic,
    Meta,
}
impl SizeType {
    pub fn is_static(self) -> bool {
        matches!(self, Static(_))
    }
    pub fn is_dynamic(self) -> bool {
        self == Dynamic
    }
    pub fn is_meta(self) -> bool {
        self == Meta
    }
    pub fn as_static(self) -> Option<u32> {
        if let Static(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn map_static<F: FnOnce(u32) -> u32>(self, f: F) -> SizeType {
        if let Static(x) = self {
            Static(f(x))
        } else {
            self
        }
    }
}
impl Display for SizeType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            SizeType::Static(size) => write!(f, "{size}"),
            SizeType::Dynamic => write!(f, "dynamic"),
            SizeType::Meta => write!(f, "meta"),
        }
    }
}
impl std::ops::Add for SizeType {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (SizeType::Meta, _) | (_, SizeType::Meta) => SizeType::Meta,
            (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => SizeType::Dynamic,
            (SizeType::Static(l), SizeType::Static(r)) => SizeType::Static(l + r),
        }
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    IntLiteral,
    Intrinsic(String),
    Int(u16, bool),
    Float16,
    Float32,
    Float64,
    Float128,
    Pointer(Box<Type>),
    Reference(Box<Type>),
    Mut(Box<Type>),
    Null,
    Module,
    TypeData,
    InlineAsm(Box<Type>),
    Array(Box<Type>, Option<u32>),
    Function(Box<Type>, Vec<(Type, bool)>),
    BoundMethod(Box<Type>, Vec<(Type, bool)>),
    Nominal(String),
    Tuple(Vec<Type>),
    Error,
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
            Pointer(x) => write!(f, "*{x}"),
            Reference(x) => write!(f, "&{x}"),
            Mut(x) => write!(f, "mut {x}"),
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
                    write!(
                        f,
                        "{}{}",
                        match ty {
                            true => "const ",
                            false => "",
                        },
                        arg
                    )?;
                    if len > 1 {
                        write!(f, ", ")?
                    }
                    len -= 1;
                }
                write!(f, "): {}", *ret)
            }
            BoundMethod(ret, args) => {
                write!(
                    f,
                    "{}{}.fn (",
                    if args[0].1 { "const " } else { "" },
                    args[0].0
                )?;
                let mut len = args.len();
                for (arg, ty) in args[1..].iter() {
                    write!(
                        f,
                        "{}{}",
                        match ty {
                            true => "const ",
                            false => "",
                        },
                        arg
                    )?;
                    if len > 1 {
                        write!(f, ", ")?
                    }
                    len -= 1;
                }
                write!(f, "): {}", *ret)
            }
            Nominal(n) => write!(f, "{n}"),
            Tuple(v) => {
                write!(f, "(")?;
                let mut it = v.iter().peekable();
                while let Some(v) = it.next() {
                    write!(f, "{v}")?;
                    if it.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Error => write!(f, "<error>"),
        }
    }
}
impl Type {
    pub fn size(&self, ctx: &CompCtx) -> SizeType {
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
            Pointer(b) | Reference(b) => match **b {
                Type::Array(_, None) => Static(ctx.flags.word_size as u32 * 2),
                _ => Static(ctx.flags.word_size as u32),
            },
            Mut(b) => b.size(ctx),
            Tuple(v) => v.iter().fold(Static(0), |v, t| {
                if let Static(bs) = v {
                    let n = t.size(ctx);
                    if let Static(ns) = n {
                        let a = t.align(ctx) as u32;
                        if a == 0 || a == 1 {
                            Static(bs + ns)
                        } else {
                            Static(((bs + a - 1) / a) * a + ns)
                        }
                    } else {
                        n
                    }
                } else {
                    v
                }
            }),
            Nominal(n) => ctx.nominals.borrow()[n].0.size(ctx),
        }
    }
    pub fn align(&self, ctx: &CompCtx) -> u16 {
        match self {
            IntLiteral => 8,
            Int(size, _) => match size {
                0..=8 => 1,
                9..=16 => 2,
                17..=32 => 4,
                33.. => 8,
            },
            Float16 => 2,
            Float32 => 4,
            Float64 | Float128 => 8,
            Null => 1,
            Array(b, _) => b.align(ctx),
            Function(..) | Module | TypeData | InlineAsm(_) | Intrinsic(_) | Error => 0,
            Pointer(_) | Reference(_) | BoundMethod(..) => ctx.flags.word_size,
            Mut(b) => b.align(ctx),
            Tuple(v) => v.iter().map(|x| x.align(ctx)).max().unwrap_or(1),
            Nominal(n) => ctx.nominals.borrow()[n].0.align(ctx),
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
            BoundMethod(ret, params) => Some(
                ctx.context
                    .struct_type(
                        &[
                            params[0].0.llvm_type(ctx)?,
                            Type::Pointer(Box::new(Type::Function(ret.clone(), params.clone())))
                                .llvm_type(ctx)?,
                        ],
                        false,
                    )
                    .into(),
            ),
            Array(b, Some(n)) => Some(b.llvm_type(ctx)?.array_type(*n).into()),
            Array(_, None) => None,
            Pointer(b) | Reference(b) => match &**b {
                Type::Array(b, None) => Some(
                    ctx.context
                        .struct_type(
                            &[
                                b.llvm_type(ctx)?.ptr_type(Default::default()).into(),
                                ctx.context.i64_type().into(),
                            ],
                            false,
                        )
                        .into(),
                ),
                Type::Array(b, Some(_)) => {
                    Some(b.llvm_type(ctx)?.ptr_type(Default::default()).into())
                }
                Type::Function(r, p) => {
                    let mut args = Vec::<BasicMetadataTypeEnum>::with_capacity(p.len());
                    for (p, c) in p {
                        if !c {
                            args.push(p.llvm_type(ctx)?.into());
                        }
                    }
                    Some(
                        if r.size(ctx) == Static(0) {
                            ctx.context.void_type().fn_type(&args, false)
                        } else {
                            r.llvm_type(ctx)?.fn_type(&args, false)
                        }
                        .ptr_type(Default::default())
                        .into(),
                    )
                }
                Type::Mut(b) => b
                    .llvm_type(ctx)
                    .map(|t| t.ptr_type(Default::default()).into()),
                b => {
                    if b.size(ctx) == Static(0) {
                        Some(PointerType(ctx.null_type.ptr_type(Default::default())))
                    } else {
                        Some(PointerType(b.llvm_type(ctx)?.ptr_type(Default::default())))
                    }
                }
            },
            Mut(b) => b
                .llvm_type(ctx)
                .map(|t| t.ptr_type(Default::default()).into()),
            Tuple(v) => {
                let mut vec = Vec::with_capacity(v.len());
                for t in v {
                    vec.push(t.llvm_type(ctx)?);
                }
                Some(ctx.context.struct_type(&vec, false).into())
            }
            Nominal(n) => ctx.nominals.borrow()[n].0.llvm_type(ctx),
        }
    }
    pub fn has_dtor(&self, ctx: &CompCtx) -> bool {
        match self {
            Type::Nominal(n) => {
                let b = ctx.nominals.borrow();
                let info = &b[n];
                info.3.dtor.is_some() || (!info.3.no_auto_drop && info.0.has_dtor(ctx))
            }
            Type::Array(b, _) | Type::Mut(b) => b.has_dtor(ctx),
            Type::Tuple(v) => v.iter().any(|t| t.has_dtor(ctx)),
            Type::BoundMethod(_, a) => a.get(0).map_or(false, |t| t.0.has_dtor(ctx)),
            _ => false,
        }
    }
    fn can_be_compiled<'ctx>(&self, v: &InterData<'ctx>, ctx: &CompCtx<'ctx>) -> bool {
        match self {
            Type::IntLiteral | Type::Int(..) => matches!(v, InterData::Int(..)),
            Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 => {
                matches!(v, InterData::Float(..))
            }
            Type::Array(b, Some(n)) => {
                if let InterData::Array(v) = v {
                    v.len() == *n as _ && v.iter().all(|v| b.can_be_compiled(v, ctx))
                } else {
                    false
                }
            }
            Type::Tuple(vs) => {
                if let InterData::Array(is) = v {
                    vs.len() == is.len()
                        && vs.iter().zip(is).all(|(t, v)| t.can_be_compiled(v, ctx))
                } else {
                    false
                }
            }
            Type::Nominal(n) => ctx.nominals.borrow()[n].0.can_be_compiled(v, ctx),
            Type::Mut(b) => b.can_be_compiled(v, ctx),
            _ => false,
        }
    }
    pub fn into_compiled<'ctx>(
        &self,
        v: &InterData<'ctx>,
        ctx: &CompCtx<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        if ctx.is_const.get() {
            return None;
        }
        match self {
            Type::IntLiteral => {
                if let InterData::Int(v) = v {
                    Some(ctx.context.i64_type().const_int(*v as u64, true).into())
                } else {
                    None
                }
            }
            Type::Int(s, u) => {
                if let InterData::Int(v) = v {
                    Some(
                        ctx.context
                            .custom_width_int_type(*s as u32)
                            .const_int(*v as u64, *u)
                            .into(),
                    )
                } else {
                    None
                }
            }
            Type::Float16 => {
                if let InterData::Float(v) = v {
                    Some(ctx.context.f16_type().const_float(*v).into())
                } else {
                    None
                }
            }
            Type::Float32 => {
                if let InterData::Float(v) = v {
                    Some(ctx.context.f32_type().const_float(*v).into())
                } else {
                    None
                }
            }
            Type::Float64 => {
                if let InterData::Float(v) = v {
                    Some(ctx.context.f64_type().const_float(*v).into())
                } else {
                    None
                }
            }
            Type::Float128 => {
                if let InterData::Float(v) = v {
                    Some(ctx.context.f128_type().const_float(*v).into())
                } else {
                    None
                }
            }
            Type::Array(b, Some(n)) => {
                if let InterData::Array(v) = v {
                    (v.len() == *n as _).then_some(())?;
                    let ty = b.llvm_type(ctx)?;
                    let arr = ty.array_type(*n);
                    let val = arr.get_undef();
                    v.iter()
                        .enumerate()
                        .try_fold(val, |val, (n, v)| {
                            b.into_compiled(v, ctx)
                                .and_then(|v| ctx.builder.build_insert_value(val, v, n as _, ""))
                                .map(|v| v.into_array_value())
                        })
                        .map(From::from)
                } else {
                    None
                }
            }
            Type::Tuple(vs) => {
                self.can_be_compiled(v, ctx).then_some(())?;
                if let InterData::Array(is) = v {
                    let (vals, types): (Vec<_>, Vec<_>) = vs
                        .iter()
                        .zip(is)
                        .map(|(t, v)| {
                            let llv = t.into_compiled(v, ctx).unwrap();
                            (llv, llv.get_type())
                        })
                        .unzip();
                    let ty = ctx.context.struct_type(&types, false);
                    vals.into_iter()
                        .enumerate()
                        .try_fold(ty.get_undef(), |v, (n, val)| {
                            ctx.builder
                                .build_insert_value(v, val, n as u32, "")
                                .map(|v| v.into_struct_value())
                        })
                        .map(From::from)
                } else {
                    unreachable!()
                }
            }
            Type::Nominal(n) => ctx.nominals.borrow()[n].0.into_compiled(v, ctx),
            Type::Mut(b) => b.into_compiled(v, ctx),
            _ => None,
        }
    }
    pub fn contains_nominal(&self) -> bool {
        match self {
            Type::Nominal(_) => true,
            Type::Reference(b)
            | Type::Pointer(b)
            | Type::Mut(b)
            | Type::Array(b, _)
            | Type::InlineAsm(b) => b.contains_nominal(),
            Type::Tuple(v) => v.iter().any(|t| t.contains_nominal()),
            Type::Function(r, a) => {
                r.contains_nominal() || a.iter().any(|t| t.0.contains_nominal())
            }
            Type::BoundMethod(r, a) => {
                r.contains_nominal() || a.iter().any(|t| t.0.contains_nominal())
            }
            _ => false,
        }
    }
    pub fn unwrapped(&self, ctx: &CompCtx) -> Cow<Self> {
        match self {
            Type::Nominal(n) => Cow::Owned(ctx.nominals.borrow()[n].0.clone()),
            Type::Reference(b) => {
                if b.contains_nominal() {
                    Cow::Owned(Type::Reference(Box::new(b.unwrapped(ctx).into_owned())))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Pointer(b) => {
                if b.contains_nominal() {
                    Cow::Owned(Type::Pointer(Box::new(b.unwrapped(ctx).into_owned())))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Array(b, s) => {
                if b.contains_nominal() {
                    Cow::Owned(Type::Array(Box::new(b.unwrapped(ctx).into_owned()), *s))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::InlineAsm(b) => {
                if b.contains_nominal() {
                    Cow::Owned(Type::InlineAsm(Box::new(b.unwrapped(ctx).into_owned())))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Tuple(v) => {
                if v.iter().any(|t| t.contains_nominal()) {
                    Cow::Owned(Type::Tuple(
                        v.iter().map(|t| t.unwrapped(ctx).into_owned()).collect(),
                    ))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Function(r, a) => {
                if r.contains_nominal() || a.iter().any(|t| t.0.contains_nominal()) {
                    Cow::Owned(Type::Function(
                        Box::new(r.unwrapped(ctx).into_owned()),
                        a.iter()
                            .map(|(t, c)| (t.unwrapped(ctx).into_owned(), *c))
                            .collect(),
                    ))
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::BoundMethod(r, a) => {
                if r.contains_nominal() || a.iter().any(|t| t.0.contains_nominal()) {
                    Cow::Owned(Type::BoundMethod(
                        Box::new(r.unwrapped(ctx).into_owned()),
                        a.iter()
                            .map(|(t, c)| (t.unwrapped(ctx).into_owned(), *c))
                            .collect(),
                    ))
                } else {
                    Cow::Borrowed(self)
                }
            }
            _ => Cow::Borrowed(self),
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            IntLiteral => panic!("There shouldn't be an int literal in a variable!"),
            Int(s, u) => {
                out.write_all(&[1])?;
                let mut v = *s as i16;
                if *u {
                    v = -v;
                }
                out.write_all(&v.to_be_bytes())
            }
            Intrinsic(s) => {
                out.write_all(&[2])?;
                out.write_all(s.as_bytes())?;
                out.write_all(&[0])
            }
            Float16 => out.write_all(&[3]),
            Float32 => out.write_all(&[4]),
            Float64 => out.write_all(&[5]),
            Float128 => out.write_all(&[6]),
            Null => out.write_all(&[7]),
            Pointer(b) => {
                out.write_all(&[8])?;
                b.save(out)
            }
            Reference(b) => {
                out.write_all(&[10])?;
                b.save(out)
            }
            Mut(b) => {
                out.write_all(&[11])?;
                b.save(out)
            }
            Function(b, p) => {
                out.write_all(&[13])?;
                out.write_all(&(p.len() as u16).to_be_bytes())?; // # of params
                b.save(out)?;
                for (par, c) in p {
                    par.save(out)?;
                    out.write_all(&[u8::from(*c)])?; // param is const
                }
                Ok(())
            }
            InlineAsm(b) => {
                out.write_all(&[14])?;
                b.save(out)
            }
            Error => panic!("error values shouldn't be serialized!"),
            Array(b, None) => {
                out.write_all(&[15])?;
                b.save(out)
            }
            Array(b, Some(s)) => {
                let mut data = [16u8, 0, 0, 0, 0];
                data[1..].copy_from_slice(&s.to_be_bytes());
                out.write_all(&data)?;
                b.save(out)
            }
            TypeData => out.write_all(&[17]),
            Nominal(n) => {
                out.write_all(&[18])?;
                out.write_all(n.as_bytes())?;
                out.write_all(&[0])
            }
            Module => out.write_all(&[19]),
            Tuple(v) => {
                let mut buf = [20u8, 0, 0, 0, 0];
                buf[1..].copy_from_slice(&(v.len() as u32).to_be_bytes());
                out.write_all(&buf)?;
                v.iter().try_for_each(|t| t.save(out))
            }
            BoundMethod(r, p) => {
                out.write_all(&[21])?;
                out.write_all(&(p.len() as u16).to_be_bytes())?; // # of params
                r.save(out)?;
                for (par, c) in p {
                    par.save(out)?;
                    out.write_all(&[u8::from(*c)])?; // param is const
                }
                Ok(())
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
            }
            2 => {
                let mut vec = vec![];
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {
                    vec.pop();
                }
                Type::Intrinsic(
                    String::from_utf8(vec).expect("intrinsic name should be valid UTF-8"),
                )
            }
            3 => Type::Float16,
            4 => Type::Float32,
            5 => Type::Float64,
            6 => Type::Float128,
            7 => Type::Null,
            8 => Type::Pointer(Box::new(Type::load(buf)?)),
            10 => Type::Reference(Box::new(Type::load(buf)?)),
            11 => Type::Mut(Box::new(Type::load(buf)?)),
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
            }
            17 => Type::TypeData,
            18 => {
                let mut vec = Vec::<u8>::new();
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {
                    vec.pop();
                }
                Type::Nominal(String::from_utf8(vec).expect("Type names should be valid UTF-8!"))
            }
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
            }
            21 => {
                let mut bytes = [0; 2];
                buf.read_exact(&mut bytes)?;
                let v = u16::from_be_bytes(bytes);
                let ret = Type::load(buf)?;
                buf.read_exact(&mut bytes[..1])?;
                let mut vec = Vec::with_capacity(v as usize);
                for _ in 0..v {
                    let t = Type::load(buf)?;
                    buf.read_exact(std::slice::from_mut(&mut c))?;
                    vec.push((t, c != 0));
                }
                Type::BoundMethod(Box::new(ret), vec)
            }
            x => panic!("read type value expecting value in 1..=21, got {x}"),
        })
    }
    /// Ensure that any symbols reachable from this type are exported
    pub fn export(&self, ctx: &CompCtx) {
        match self {
            Type::Nominal(n) => ctx.nominals.borrow_mut().get_mut(n).unwrap().1 = true,
            Type::Reference(b)
            | Type::Pointer(b)
            | Type::Mut(b)
            | Type::Array(b, _)
            | Type::InlineAsm(b) => b.export(ctx),
            Type::Tuple(v) => v.iter().for_each(|t| t.export(ctx)),
            Type::Function(r, a) => {
                r.export(ctx);
                a.iter().for_each(|t| t.0.export(ctx));
            }
            Type::BoundMethod(r, a) => {
                r.export(ctx);
                a.iter().for_each(|t| t.0.export(ctx));
            }
            _ => {}
        }
    }
}
pub fn tuple_type<'ctx>(v: &[Type], ctx: &CompCtx<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    let mut vec = Vec::with_capacity(v.len());
    for t in v {
        vec.push(t.llvm_type(ctx)?);
    }
    Some(ctx.context.struct_type(&vec, false).into())
}
#[derive(Debug, Clone, Default)]
pub struct NominalInfo<'ctx> {
    pub dtor: Option<FunctionValue<'ctx>>,
    pub no_auto_drop: bool,
}
impl<'ctx> NominalInfo<'ctx> {
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        if let Some(fv) = self.dtor {
            out.write_all(fv.get_name().to_bytes())?;
        }
        out.write_all(&[0])?;
        out.write_all(&[u8::from(self.no_auto_drop)])
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'ctx>) -> io::Result<Self> {
        let mut vec = vec![];
        buf.read_until(0, &mut vec)?;
        if vec.last() == Some(&0) {
            vec.pop();
        }
        let dtor = if vec.is_empty() {
            None
        } else {
            let name = String::from_utf8(vec).expect("value should be valid UTF-8!");
            let fv = ctx.module.get_function(&name);
            Some(fv.unwrap_or_else(|| {
                ctx.module.add_function(
                    &name,
                    ctx.context
                        .void_type()
                        .fn_type(&[ctx.null_type.ptr_type(Default::default()).into()], false),
                    None,
                )
            }))
        };
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        let no_auto_drop = c != 0;
        Ok(Self { dtor, no_auto_drop })
    }
}
