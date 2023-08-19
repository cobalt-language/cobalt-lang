use super::*;
static CUSTOM_INTERN: Interner<Box<str>> = Interner::new();
static CUSTOM_DATA: dashmap::DashMap<&'static str, (TypeRef, bool, usize, usize)> =
    Default::default();
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
pub struct Custom(Box<str>);
impl Custom {
    #[ref_cast_custom]
    fn from_ref(types: &Box<str>) -> &Self;
    pub fn new(types: Box<str>) -> &'static Self {
        Self::from_ref(CUSTOM_INTERN.intern(types))
    }
    pub fn new_ref(types: &str) -> &'static Self {
        Self::from_ref(CUSTOM_INTERN.intern_ref(types))
    }
}
impl Type for Custom {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("custom")
    }
    fn size(&self) -> SizeType {
        CUSTOM_DATA.get(&*self.0).unwrap().0.size()
    }
    fn align(&self) -> u16 {
        CUSTOM_DATA.get(&*self.0).unwrap().0.align()
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        let keys = CUSTOM_DATA.get(&*self.0).unwrap();
        let borrow = ctx.nom_info.borrow();
        let info = &borrow[keys.3];
        info.dtor.is_some() || (!info.is_linear_type && keys.0.has_dtor(ctx))
    }
}
