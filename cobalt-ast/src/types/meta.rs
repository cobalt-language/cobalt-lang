use super::*;
#[derive(Debug, Display)]
#[display(fmt = "type")]
pub struct TypeData(());
impl TypeData {
    pub fn new() -> &'static Self {
        static SELF: TypeData = Self(());
        &SELF
    }
}
impl Type for TypeData {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("type")
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
}
#[derive(Debug, Display)]
#[display(fmt = "module")]
pub struct Module(());
impl Module {
    pub fn new() -> &'static Self {
        static SELF: Module = Self(());
        &SELF
    }
}
impl Type for Module {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("module")
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<error>")]
pub struct Error(());
impl Error {
    pub fn new() -> &'static Self {
        static SELF: Error = Self(());
        &SELF
    }
}
impl Type for Error {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("error")
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
}
#[derive(Debug, Display)]
#[display(fmt = "null")]
pub struct Null(());
impl Null {
    pub fn new() -> &'static Self {
        static SELF: Null = Self(());
        &SELF
    }
}
impl Type for Null {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("null")
    }
    fn size(&self) -> SizeType {
        SizeType::Static(0)
    }
    fn align(&self) -> u16 {
        0
    }
}
