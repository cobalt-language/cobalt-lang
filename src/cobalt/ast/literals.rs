use crate::*;
use std::any::Any;
use inkwell::values::AnyValueEnum;
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
    fn res_type(&self, ctx: &mut BaseCtx) -> TypeRef {panic!("code generation has not been implemented")}
    fn codegen<'ctx>(&self, ctx: &mut CompCtx<'ctx>) -> (AnyValueEnum<'ctx>, TypeRef) {panic!("code generation has not been implemented")}
    fn eval(&self, ctx: &mut BaseCtx) -> (Box<dyn Any>, TypeRef) {panic!("code generation has not been implemented")}
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
