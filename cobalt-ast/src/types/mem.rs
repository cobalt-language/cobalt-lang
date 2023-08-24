use super::*;
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "&{}", _0)]
#[repr(transparent)]
pub struct Reference(TypeRef);
impl Reference {
    pub const KIND: NonZeroU64 = make_id(b"ref");
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl Type for Reference {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn decay(&self) -> TypeRef {
        self.base().decay()
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "*{}", _0)]
#[repr(transparent)]
pub struct Pointer(TypeRef);
impl Pointer {
    pub const KIND: NonZeroU64 = make_id(b"ptr");
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl Type for Pointer {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "mut {}", _0)]
#[repr(transparent)]
pub struct Mut(TypeRef);
impl Mut {
    pub const KIND: NonZeroU64 = make_id(b"mut");
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl Type for Mut {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        self.0.size()
    }
    fn align(&self) -> u16 {
        self.0.align()
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn decay(&self) -> TypeRef {
        self.base().decay()
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
