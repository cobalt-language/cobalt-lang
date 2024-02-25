use super::*;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

#[inline(always)]
pub fn union_align(tag: Option<TypeRef>, types: &[TypeRef]) -> u16 {
    types
        .iter()
        .map(|v| v.align())
        .try_fold(tag.map_or(1, |t| t.align()), |old, new| {
            if new == 0 {
                None
            } else {
                Some(std::cmp::max(old, new))
            }
        })
        .unwrap_or(0)
}
#[inline(always)]
pub fn union_size(types: &[TypeRef], align: u32) -> SizeType {
    types
        .iter()
        .try_fold(None, |old, new| {
            let sz = new.size();
            if sz == SizeType::Meta {
                None
            } else {
                Some(Some(if let Some(old) = old {
                    match (old, sz) {
                        (SizeType::Static(l), SizeType::Static(r)) => {
                            SizeType::Static(std::cmp::max(l, r))
                        }
                        (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => SizeType::Dynamic,
                        (SizeType::Meta, _) | (_, SizeType::Meta) => unreachable!(),
                    }
                } else {
                    sz
                }))
            }
        })
        .flatten()
        .unwrap_or(SizeType::Meta)
        .map_static(|mut s| {
            s *= align;
            s += align - 1;
            s /= align;
            s
        })
}
pub fn union_type<'ctx>(
    tag: Option<TypeRef>,
    types: &[TypeRef],
    ctx: &CompCtx<'_, 'ctx>,
) -> Option<BasicTypeEnum<'ctx>> {
    let mut found = false;
    let mut max_align = 0;
    let mut max_size = 0;
    let mut max_aligned = ctx.null_type;
    for ty in types {
        let size = ty.size().as_static()?;
        let align = ty.align();
        let Some(llt) = ty.llvm_type(ctx) else {
            if size == 0 {
                continue;
            } else {
                return None;
            }
        };
        if align > max_align {
            max_align = align;
            max_size = size;
            max_aligned = llt;
            found = true;
        }
    }
    found.then_some(())?;
    let total_size = union_size(types, max_align as _).as_static()?;
    let body = if total_size == max_size {
        max_aligned
    } else {
        ctx.context
            .struct_type(
                &[
                    max_aligned,
                    ctx.context
                        .i8_type()
                        .array_type(total_size - max_size)
                        .into(),
                ],
                false,
            )
            .into()
    };
    if let Some(llt) = tag.and_then(|tag| tag.llvm_type(ctx)) {
        Some(ctx.context.struct_type(&[llt, body], false).into())
    } else {
        Some(body)
    }
}

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
        &self.0 .0
    }

    pub fn is_sorted(&self) -> bool {
        self.0 .1
    }

    pub fn tag_type(&self) -> &'static types::Int {
        types::Int::unsigned((usize::BITS - self.variants().len().leading_zeros() as u32) as u16)
    }
}
impl Display for EnumOrUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(if self.0 .1 { "@union" } else { "@enum" })?;
        for item in self.0 .0.iter() {
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
    fn size(&'static self) -> SizeType {
        let tag = self.tag_type();
        if self.variants().is_empty() {
            tag.size()
        } else {
            let align = union_align(Some(tag), self.variants()) as u32;
            let (mut raw_size, mut tag_size) =
                match (union_size(self.variants(), align as _), tag.size()) {
                    (SizeType::Static(l), SizeType::Static(r)) => (l, r),
                    (SizeType::Meta, _) | (_, SizeType::Meta) => return SizeType::Meta,
                    (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => return SizeType::Dynamic,
                };
            raw_size *= align;
            raw_size += align - 1;
            raw_size /= align;
            tag_size *= align;
            tag_size += align - 1;
            tag_size -= align;
            SizeType::Static(raw_size + tag_size)
        }
    }
    fn align(&'static self) -> u16 {
        union_align(Some(self.tag_type()), self.variants())
    }
    fn llvm_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        union_type(Some(self.tag_type()), self.variants(), ctx)
    }
}

pub type Enum = EnumOrUnion;
pub type Union = EnumOrUnion;
submit_types!(EnumOrUnion);
