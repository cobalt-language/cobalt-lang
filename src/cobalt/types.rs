use inkwell::types::*;
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
pub enum Type {
    IntLiteral,
    Int(u64, bool),
    Float16, Float32, Float64, Float128,
    Null, Module, TypeData, Array(Box<Type>, Option<u64>),
    Function(Box<Type>, Vec<Type>)
}
impl Type {
    pub fn size(&self) -> SizeType {
        match self {
            IntLiteral => Static(8),
            Int(size, _) => Static((size + 7) / 8),
            Float16 => Static(2),
            Float32 => Static(4),
            Float64 => Static(8),
            Float128 => Static(16),
            Null => Static(0),
            Array(b, Some(s)) => b.size().map_static(|x| x * s),
            Array(_, None) => Dynamic,
            Function(_, _) | Module | TypeData => Meta
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
            }
            Float16 => 2,
            Float32 => 4,
            Float64 | Float128 => 8,
            Null => 1,
            Array(b, _) => b.align(),
            Function(_, _) | Module | TypeData => 0
        }
    }
    pub fn llvm_type<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> Option<AnyTypeEnum> {
        panic!("LLVM code generation isn't implemented here")
    }
}
