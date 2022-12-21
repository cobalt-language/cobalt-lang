use crate::*;
use inkwell::values::BasicValueEnum::*;
pub struct IntLiteralAST {
    loc: Location,
    pub val: i128,
    pub suffix: Option<String>
}
impl IntLiteralAST {
    pub fn new(loc: Location, val: i128, suffix: Option<String>) -> Self {IntLiteralAST {loc, val, suffix}}
}
impl AST for IntLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("") => Type::IntLiteral,
            Some("isize") => Type::Int(64, false),
            Some(x) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some("usize") => Type::Int(64, true),
            Some(x) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val), Type::IntLiteral), vec![]),
            Some("isize") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(64, false)), vec![]),
            Some(x) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => {
                let size: u64 = x[1..].parse().unwrap_or(0);
                (Variable::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(size, false)), vec![])
            },
            Some("usize") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(64, true)), vec![]),
            Some(x) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => {
                let size: u64 = x[1..].parse().unwrap_or(0);
                (Variable::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(size, true)), vec![])
            },
            Some(x) => (Variable::error(), vec![Error::new(self.loc.clone(), 390, format!("unknown suffix {x} for integer literal"))])
        }
    }
    fn to_code(&self) -> String {
        if let Some(ref suf) = self.suffix {
            format!("{}{}", self.val, suf)
        }
        else {
            self.val.to_string()
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "int: {}", self.val)?;
        if let Some(ref s) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct FloatLiteralAST {
    loc: Location,
    pub val: f64,
    pub suffix: Option<String>
}
impl FloatLiteralAST {
    pub fn new(loc: Location, val: f64, suffix: Option<String>) -> Self {FloatLiteralAST {loc, val, suffix}}
}
impl AST for FloatLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("f64") => Type::Float64,
            Some("f16") => Type::Float16,
            Some("f32") => Type::Float32,
            Some("f128") => Type::Float64,
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("f64") => (Variable::interpreted(FloatValue(ctx.context.f64_type().const_float(self.val)), InterData::Float(self.val), Type::Float64), vec![]),
            Some("f16") => (Variable::interpreted(FloatValue(ctx.context.f16_type().const_float(self.val)), InterData::Float(self.val), Type::Float16), vec![]),
            Some("f32") => (Variable::interpreted(FloatValue(ctx.context.f32_type().const_float(self.val)), InterData::Float(self.val), Type::Float32), vec![]),
            Some("f128") => (Variable::interpreted(FloatValue(ctx.context.f128_type().const_float(self.val)), InterData::Float(self.val), Type::Float128), vec![]),
            Some(x) => (Variable::error(), vec![Error::new(self.loc.clone(), 390, format!("unknown suffix {x} for floating-point literal"))])
        }
    }
    fn to_code(&self) -> String {
        if let Some(ref suf) = self.suffix {
            format!("{}{}", self.val, suf)
        }
        else {
            self.val.to_string()
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "float: {}", self.val)?;
        if let Some(ref s) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct CharLiteralAST {
    loc: Location,
    pub val: char,
    pub suffix: Option<String>
}
impl CharLiteralAST {
    pub fn new(loc: Location, val: char, suffix: Option<String>) -> Self {CharLiteralAST {loc, val, suffix}}
}
impl AST for CharLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("") => Type::Char,
            Some("isize") => Type::Int(64, false),
            Some(x) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some("usize") => Type::Int(64, true),
            Some(x) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match self.suffix.as_ref().map(|x| x.as_str()) {
            None | Some("") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Char), vec![]),
            Some("isize") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(64, false)), vec![]),
            Some(x) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => {
                let size: u64 = x[1..].parse().unwrap_or(0);
                (Variable::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(size, false)), vec![])
            },
            Some("usize") => (Variable::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(64, true)), vec![]),
            Some(x) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => {
                let size: u64 = x[1..].parse().unwrap_or(0);
                (Variable::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(size, true)), vec![])
            },
            Some(x) => (Variable::error(), vec![Error::new(self.loc.clone(), 390, format!("unknown suffix {x} for character literal"))])
        }
    }
    fn to_code(&self) -> String {
        if let Some(ref suf) = self.suffix {
            format!("{:?}{}", self.val, suf)
        }
        else {
            format!("{:?}", self.val)
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "char: {:?}", self.val)?;
        if let Some(ref s) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct StringLiteralAST {
    loc: Location,
    pub val: String,
    pub suffix: Option<String>
}
impl StringLiteralAST {
    pub fn new(loc: Location, val: String, suffix: Option<String>) -> Self {StringLiteralAST {loc, val, suffix}}
}
impl AST for StringLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix {
            None => Type::Pointer(Box::new(Type::Char)),
            Some(_) => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match self.suffix {
            None => (Variable::interpreted(PointerValue(ctx.builder.build_global_string_ptr(self.val.as_str(), "__internals.str").as_pointer_value()), InterData::Str(self.val.clone()), Type::Pointer(Box::new(Type::Char))), vec![]),
            Some(ref x) => (Variable::error(), vec![Error::new(self.loc.clone(), 390, format!("unknown suffix {x} for string literal"))])
        }
    }
    fn to_code(&self) -> String {
        if let Some(ref suf) = self.suffix {
            format!("{:?}{}", self.val, suf)
        }
        else {
            format!("{:?}", self.val)
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "string: {:?}", self.val)?;
        if let Some(ref s) = self.suffix {write!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
