use crate::{DottedName, AST};
use std::fmt::*;
pub use ParsedType::*;
pub enum ParsedType {
    Null, Bool,
    Int(u64),
    UInt(u64),
    ISize, USize,
    F16, F32, F64, F128,
    Pointer(Box<ParsedType>),
    Reference(Box<ParsedType>),
    Borrow(Box<ParsedType>),
    UnsizedArray(Box<ParsedType>),
    SizedArray(Box<ParsedType>, Box<dyn AST>),
    TypeOf(Box<dyn AST>),
    Other(DottedName),
}
impl Debug for ParsedType { // Debug isn't implemented for Box
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Null => write!(f, "Null"),
            Bool => write!(f, "Bool"),
            Int(size) => write!(f, "Int{size}"),
            UInt(size) => write!(f, "UInt{size}"),
            ISize => write!(f, "ISize"),
            USize => write!(f, "USize"),
            F16 => write!(f, "F16"),
            F32 => write!(f, "F32"),
            F64 => write!(f, "F64"),
            F128 => write!(f, "F128"),
            Pointer(base) => write!(f, "Pointer({})", *base),
            Reference(base) => write!(f, "Reference({})", *base),
            Borrow(base) => write!(f, "Borrow({})", *base),
            UnsizedArray(base) => write!(f, "UnsizedArray({})", *base),
            SizedArray(base, ast) => write!(f, "SizedArray({base}, {})", ast.to_code()),
            TypeOf(value) => write!(f, "TypeOf({})", value.to_code()),
            Other(name) => write!(f, "Other({name})")
        }
    }
}
impl Display for ParsedType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Null => write!(f, "null"),
            Bool => write!(f, "bool"),
            Int(size) => write!(f, "i{size}"),
            UInt(size) => write!(f, "u{size}"),
            ISize => write!(f, "isize"),
            USize => write!(f, "usize"),
            F16 => write!(f, "f16"),
            F32 => write!(f, "f32"),
            F64 => write!(f, "f64"),
            F128 => write!(f, "f128"),
            Pointer(base) => write!(f, "{}*", *base),
            Reference(base) => write!(f, "{}&", *base),
            Borrow(base) => write!(f, "{}^", *base),
            UnsizedArray(base) => write!(f, "{}[]", *base),
            SizedArray(base, ast) => write!(f, "{base}[{}]", ast.to_code()),
            TypeOf(value) => write!(f, "typeof {}", value.to_code()),
            Other(name) => write!(f, "{name}")
        }
    }
}
