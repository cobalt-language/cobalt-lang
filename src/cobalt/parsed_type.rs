use crate::*;
use ParsedType::*;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntoTypeError {
    NotAnInt(String, Location),
    NotCompileTime(Location),
    NotAModule(String, Location),
    DoesNotExist(String, Location)
}
pub enum ParsedType {
    Error,
    Null, Bool,
    Int(u16),
    UInt(u16),
    ISize, USize,
    F16, F32, F64, F128,
    Pointer(Box<ParsedType>, bool),
    Reference(Box<ParsedType>, bool),
    Borrow(Box<ParsedType>),
    UnsizedArray(Box<ParsedType>),
    SizedArray(Box<ParsedType>, Box<dyn AST>),
    TypeOf(Box<dyn AST>),
    Other(DottedName),
}
impl ParsedType {
    pub fn into_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Result<Type, IntoTypeError>, Vec<Diagnostic>) {
        (match self {
            Error => Ok(Type::Error),
            Null => Ok(Type::Null),
            Bool => Ok(Type::Int(1, false)),
            Int(size) => Ok(Type::Int(*size, false)),
            UInt(size) => Ok(Type::Int(*size, true)),
            ISize => Ok(Type::Int(ctx.flags.word_size * 8, false)),
            USize => Ok(Type::Int(ctx.flags.word_size * 8, true)),
            F16 => Ok(Type::Float16),
            F32 => Ok(Type::Float32),
            F64 => Ok(Type::Float64),
            F128 => Ok(Type::Float128),
            Borrow(base) => {
                let (base, errs) = base.into_type(ctx);
                return (base.map(Box::new).map(Type::Borrow), errs)
            },
            Pointer(base, m) => {
                let (base, errs) = base.into_type(ctx);
                return (base.map(|b| Type::Pointer(Box::new(b), *m)), errs)
            },
            Reference(base, m) => {
                let (base, errs) = base.into_type(ctx);
                return (base.map(|b| Type::Reference(Box::new(b), *m)), errs)
            },
            UnsizedArray(base) => {
                let (base, errs) = match base.into_type(ctx) {
                    (Ok(base), errs) => (base, errs),
                    (Err(err), errs) => return (Err(err), errs)
                };
                return (Ok(Type::Array(Box::new(base), None)), errs);
            },
            SizedArray(base, size) => {
                let (base, mut errs) = match base.into_type(ctx) {
                    (Ok(base), errs) => (base, errs),
                    (Err(err), errs) => return (Err(err), errs)
                };
                let old_const = ctx.is_const.replace(true);
                let (var, mut es) = size.codegen(ctx);
                errs.append(&mut es);
                let err = format!("{}", var.data_type);
                let var = types::utils::impl_convert(var, Type::Int(64, false), ctx);
                ctx.is_const.set(old_const);
                return (if var.is_none() {Err(IntoTypeError::NotAnInt(err, size.loc()))}
                else if let Some(InterData::Int(val)) = var.unwrap().inter_val {Ok(Type::Array(Box::new(base), Some(val as u32)))}
                else {Err(IntoTypeError::NotCompileTime(size.loc()))}, errs);
            },
            TypeOf(expr) => {
                let old_const = ctx.is_const.replace(true);
                let (var, errs) = expr.codegen(ctx);
                ctx.is_const.set(old_const);
                return (Ok(var.data_type), errs);
            },
            Other(name) => Err(IntoTypeError::DoesNotExist(format!("{}", name), name.ids.get(0).map_or((0, 0..0), |(_, loc)| loc.clone())))
        }, vec![])
    }
}
impl std::fmt::Debug for ParsedType { // Debug isn't implemented for Box
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error => write!(f, "Error"),
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
            Pointer(base, m) => write!(f, "Pointer({}, {m})", *base),
            Reference(base, m) => write!(f, "Reference({}, {m})", *base),
            Borrow(base) => write!(f, "Borrow({})", *base),
            UnsizedArray(base) => write!(f, "UnsizedArray({})", *base),
            SizedArray(base, ast) => write!(f, "SizedArray({base}, {})", ast.to_code()),
            TypeOf(value) => write!(f, "TypeOf({})", value.to_code()),
            Other(name) => write!(f, "Other({name})")
        }
    }
}
impl std::fmt::Display for ParsedType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error => write!(f, "<error-type>"),
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
            Pointer(base, m) => write!(f, "{} {}*", *base, if *m {"mut"} else {"const"}),
            Reference(base, m) => write!(f, "{} {}&", *base, if *m {"mut"} else {"const"}),
            Borrow(base) => write!(f, "{}^", *base),
            UnsizedArray(base) => write!(f, "{}[]", *base),
            SizedArray(base, ast) => write!(f, "{base}[{}]", ast.to_code()),
            TypeOf(value) => write!(f, "typeof {}", value.to_code()),
            Other(name) => write!(f, "{name}")
        }
    }
}
