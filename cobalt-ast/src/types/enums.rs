use super::*;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};


#[derive(Debug, ConstIdentify, PartialEq, Eq, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct EnumOrUnion((Box<[TypeRef]>, bool));
impl EnumOrUnion {
    #[ref_cast_custom]
    fn from_ref(variants: &(Box<[TypeRef]>, bool)) -> &Self;

    pub fn sort_types(lhs: &TypeRef, rhs: &TypeRef) -> Ordering {
        todo!()
    }

    pub fn new<V: Into<Box<[TypeRef]>>>(variants: V, sorted: bool) -> &'static Self {
        static INTERN: Interner<(Box<[TypeRef]>, bool)> = Interner::new();
        let mut vars = variants.into();
        if sorted {
            vars.sort_by(Self::sort_types);
        }
        Self::from_ref(INTERN.intern((vars, sorted)))
    }

    pub fn variants(&self) -> &[TypeRef] {
        &self.0.0
    }

    pub fn is_sorted(&self) -> bool {
        self.0.1
    }
}
impl Display for EnumOrUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(if self.0.1 {"@union"} else {"@enum"})?;
        for item in self.0.0.iter() {
            f.write_str(" | ")?;
            Display::fmt(&item, f)?;
        }
        Ok(())
    }
}

#[doc(hidden)]
#[derive(Serialize, Deserialize)]
pub struct EoUShim {
    sorted: bool,
    variants: Cow<'static, [TypeRef]>,
}

impl TypeSerde for EnumOrUnion {
    no_type_header!();
    impl_type_proxy!(EoUShim, this => EoUShim {sorted: this.is_sorted(), variants: this.variants().into()}, EoUShim {sorted, variants} => Self::new(variants, sorted));
}
impl Type for EnumOrUnion {
    fn size(&self) -> SizeType {
        tuple_size(self.variants())
    }
    fn align(&self) -> u16 {
        self.variants()
            .iter()
            .map(|v| v.align())
            .fold(1, |old, new| {
                if old == 0 || new == 0 {
                    0
                } else {
                    std::cmp::max(old, new)
                }
            })
    }
}

pub type Enum = EnumOrUnion;
pub type Union = EnumOrUnion;
submit_types!(EnumOrUnion);
