use super::*;
#[derive(Debug, Display)]
#[display(fmt = "type")]
pub struct TypeData(());
impl TypeData {
    pub const KIND: NonZeroU64 = make_id(b"type");
    pub fn new() -> &'static Self {
        static SELF: TypeData = Self(());
        &SELF
    }
}
impl Type for TypeData {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "module")]
pub struct Module(());
impl Module {
    pub const KIND: NonZeroU64 = make_id(b"module");
    pub fn new() -> &'static Self {
        static SELF: Module = Self(());
        &SELF
    }
}
impl Type for Module {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<error>")]
pub struct Error(());
impl Error {
    pub const KIND: NonZeroU64 = make_id(b"error");
    pub fn new() -> &'static Self {
        static SELF: Error = Self(());
        &SELF
    }
}
impl Type for Error {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "null")]
pub struct Null(());
impl Null {
    pub const KIND: NonZeroU64 = make_id(b"null");
    pub fn new() -> &'static Self {
        static SELF: Null = Self(());
        &SELF
    }
}
impl Type for Null {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(0)
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
