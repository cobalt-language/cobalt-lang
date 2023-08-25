use super::*;
#[derive(Debug, PartialEq, Eq, Hash, Display, RefCastCustom)]
#[display(fmt = "{}{}", r#"if _0.1 {"u"} else {"i"}"#, "_0.0")]
#[repr(transparent)]
pub struct Int((u16, bool));
impl Int {
    pub const KIND: NonZeroU64 = make_id(b"int");
    #[ref_cast_custom]
    fn from_ref(val: &(u16, bool)) -> &Self;
    pub fn new(bits: u16, unsigned: bool) -> &'static Self {
        static INTERN: Interner<(u16, bool)> = Interner::new();
        Self::from_ref(INTERN.intern((bits, unsigned)))
    }
    pub fn signed(bits: u16) -> &'static Self {
        Self::new(bits, false)
    }
    pub fn unsigned(bits: u16) -> &'static Self {
        Self::new(bits, true)
    }
    pub fn isize(ctx: &CompCtx) -> &'static Self {
        Self::new(ctx.flags.word_size, false)
    }
    pub fn usize(ctx: &CompCtx) -> &'static Self {
        Self::new(ctx.flags.word_size, false)
    }
    pub fn bool() -> &'static Self {
        Self::new(1, true)
    }
    pub fn bits(&self) -> u16 {
        self.0 .0
    }
    pub fn is_signed(&self) -> bool {
        !self.0 .1
    }
    pub fn is_unsigned(&self) -> bool {
        self.0 .1
    }
}
impl Type for Int {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(((self.0 .0 + 7) / 8) as _)
    }
    fn align(&self) -> u16 {
        1 << std::cmp::min(16 - self.0 .0.leading_zeros(), 6)
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        Some(ctx.context.custom_width_int_type(self.bits() as _).into())
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(&self.0 .0.to_be_bytes())?;
        out.write_all(std::slice::from_ref(&u8::from(self.0 .1)))
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let mut arr = [0u8; 2];
        buf.read_exact(&mut arr)?;
        let bits = u16::from_be_bytes(arr);
        buf.read_exact(&mut arr[..1])?;
        Ok(Self::new(bits, arr[0] != 0))
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<int literal>")]
pub struct IntLiteral(());
impl IntLiteral {
    pub const KIND: NonZeroU64 = make_id(b"intlit");
    pub fn new() -> &'static Self {
        static SELF: IntLiteral = Self(());
        &SELF
    }
}
impl Type for IntLiteral {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn decay(&self) -> TypeRef {
        Int::signed(64)
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}