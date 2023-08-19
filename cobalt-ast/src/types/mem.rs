use super::*;
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "&{}", _0)]
#[repr(transparent)]
pub struct Reference(TypeRef);
impl Reference {
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
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("ref")
    }
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "*{}", _0)]
#[repr(transparent)]
pub struct Pointer(TypeRef);
impl Pointer {
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
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("ptr")
    }
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "mut {}", _0)]
#[repr(transparent)]
pub struct Mut(TypeRef);
impl Mut {
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
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("mut")
    }
    fn size(&self) -> SizeType {
        self.0.size()
    }
    fn align(&self) -> u16 {
        self.0.align()
    }
}
