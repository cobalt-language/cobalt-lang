use super::*;
#[derive(Debug, PartialEq, Eq, Hash, Display, RefCastCustom)]
#[display(fmt = "{}{}", r#"if _0.1 {"u"} else {"i"}"#, "_0.0")]
#[repr(transparent)]
pub struct Int((u16, bool));
impl Int {
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
}
impl Type for Int {
    fn kind() -> usize {
        make_id("int")
    }
    fn size(&self) -> SizeType {
        SizeType::Static(((self.0 .0 + 7) / 8) as _)
    }
    fn align(&self) -> u16 {
        1 << std::cmp::min(16 - self.0 .0.leading_zeros(), 6)
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<int literal>")]
pub struct IntLiteral(());
impl IntLiteral {
    pub fn new() -> &'static Self {
        static SELF: IntLiteral = Self(());
        &SELF
    }
}
impl Type for IntLiteral {
    fn kind() -> usize {
        make_id("intlit")
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
}
