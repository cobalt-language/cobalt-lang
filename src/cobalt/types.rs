use inkwell::types::{BasicType, BasicTypeEnum::{self, *}};
use crate::CompCtx;
use Type::*;
use SizeType::*;
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum SizeType {
    Static(u64),
    Dynamic,
    Meta
}
impl SizeType {
    pub fn is_static(self) -> bool {if let Static(_) = self {true} else {false}}
    pub fn is_dynamic(self) -> bool {self == Dynamic}
    pub fn is_meta(self) -> bool {self == Meta}
    pub fn as_static(self) -> Option<u64> {if let Static(x) = self {Some(x)} else {None}}
    pub fn map_static<F: FnOnce(u64) -> u64>(self, f: F) -> SizeType {if let Static(x) = self {Static(f(x))} else {self}}
}
#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    IntLiteral, Char,
    Int(u64, bool),
    Float16, Float32, Float64, Float128,
    Pointer(Box<Type>), Reference(Box<Type>), Borrow(Box<Type>),
    Null, Module, TypeData, Array(Box<Type>, Option<u64>),
    Function(Box<Type>, Vec<Type>)
}
impl Type {
    pub fn size(&self) -> SizeType {
        match self {
            IntLiteral => Static(8),
            Int(size, _) => Static((size + 7) / 8),
            Char => Static(4),
            Float16 => Static(2),
            Float32 => Static(4),
            Float64 => Static(8),
            Float128 => Static(16),
            Null => Static(0),
            Array(b, Some(s)) => b.size().map_static(|x| x * s),
            Array(_, None) => Dynamic,
            Function(_, _) | Module | TypeData => Meta,
            Pointer(_) | Reference(_) => Static(8),
            Borrow(b) => b.size()
        }
    }
    pub fn align(&self) -> u64 {
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
            Function(_, _) | Module | TypeData => 0,
            Pointer(_) | Reference(_) => 8,
            Borrow(b) => b.align()
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
            Null | Function(_, _) | Module | TypeData => None,
            Array(_, Some(_)) => todo!("arrays aren't implemented yet"),
            Array(_, None) => todo!("arrays aren't implemented yet"),
            Pointer(b) | Reference(b) => Some(PointerType(b.llvm_type(ctx)?.ptr_type(inkwell::AddressSpace::Generic))),
            Borrow(b) => b.llvm_type(ctx)
        }
    }
    pub fn register(&self) -> bool {
        match self {
            IntLiteral | Int(_, _) | Char | Float16 | Float32 | Float64 | Float128 | Null | Function(_, _) | Pointer(_) | Reference(_) => true,
            Borrow(b) => b.register(),
            _ => false
        }
    }
    pub fn copyable(&self) -> bool {
        match self {
            IntLiteral | Int(_, _) | Char | Float16 | Float32 | Float64 | Float128 | Null | Function(_, _) | Pointer(_) | Reference(_) | Borrow(_) => true,
            _ => false
        }
    }
}
