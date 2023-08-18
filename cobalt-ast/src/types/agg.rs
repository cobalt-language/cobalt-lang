use crate::*;
#[derive(Debug, Display)]
#[display(
    fmt = "({})",
    r#"_0.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")"#
)]
pub struct Tuple(Vec<TypeRef>);
impl Tuple {
    pub fn new(types: &[TypeRef]) -> &'static Self {
        unsafe { std::mem::transmute(TUPLE_INTERN.intern_ref(types)) }
    }
}
static TUPLE_INTERN: Interner<Vec<TypeRef>> = Interner::new();
