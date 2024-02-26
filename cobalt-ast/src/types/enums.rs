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
    if let Some(llt) = tag
        .and_then(|tag| tag.size().is_c().then(|| tag.llvm_type(ctx)))
        .flatten()
    {
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

    pub fn tag_type(&self) -> TypeRef {
        if self.variants().is_empty() {
            types::Null::new() // TODO: switch this to a never type
        } else {
            types::Int::unsigned(
                (usize::BITS - self.variants().len().leading_zeros() as u32 - 1) as u16,
            )
        }
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
        match self.variants() {
            [] => SizeType::Meta,
            [v] => v.size(),
            _ => {
                let align = union_align(Some(tag), self.variants()) as u32;
                let (mut raw_size, mut tag_size) =
                    match (union_size(self.variants(), align as _), tag.size()) {
                        (SizeType::Static(l), SizeType::Static(r)) => (l, r),
                        (SizeType::Meta, _) | (_, SizeType::Meta) => return SizeType::Meta,
                        (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => {
                            return SizeType::Dynamic
                        }
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
    }
    fn align(&'static self) -> u16 {
        union_align(Some(self.tag_type()), self.variants())
    }
    fn llvm_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        union_type(Some(self.tag_type()), self.variants(), ctx)
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        self.variants().iter().any(|v| v.has_dtor(ctx))
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        if ctx.is_const.get() {
            return;
        }
        let mut dtors_iter = self
            .variants()
            .iter()
            .enumerate()
            .filter(|v| v.1.has_dtor(ctx));
        let Some((fi, &ft)) = dtors_iter.by_ref().next() else {
            return;
        };
        if self.variants().len() == 1 {
            ft.ins_dtor(val, ctx);
            return;
        }
        let llt = self.llvm_type(ctx).unwrap();
        let Some(eptr) = val.addr(ctx) else { return };
        let ptr = ctx
            .builder
            .build_struct_gep(llt, eptr, 1, "enum.body")
            .unwrap();
        if let Some(InterData::Array(arr)) = &val.inter_val {
            if let [InterData::Int(dsc), iv] = &arr[..] {
                let ty = self.variants()[*dsc as usize];
                let Some(llt) = ty.llvm_type(ctx) else { return };
                let val = ctx.builder.build_load(llt, ptr, "").unwrap();
                Value::with_addr(Some(val), Some(iv.clone()), ty, ptr).ins_dtor(ctx);
                return;
            }
        }
        let Some(start) = ctx.builder.get_insert_block() else {
            return;
        };
        let Some(f) = start.get_parent() else {
            return;
        };
        let dsc = ctx
            .builder
            .build_extract_value(val.value(ctx).unwrap().into_struct_value(), 0, "enum.disc")
            .unwrap()
            .into_int_value();
        let it = dsc.get_type();
        if dtors_iter.next().is_none() {
            let then = ctx.context.append_basic_block(f, "enum.single.dtor");
            let merge = ctx.context.append_basic_block(f, "enum.dtor.merge");
            let check = ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    dsc,
                    it.const_int(fi as _, false),
                    "",
                )
                .unwrap();
            ctx.builder
                .build_conditional_branch(check, then, merge)
                .unwrap();
            ctx.builder.position_at_end(then);
            let val = ctx
                .builder
                .build_load(ft.llvm_type(ctx).unwrap(), ptr, "")
                .unwrap();
            Value::with_addr(Some(val), None, ft, ptr).ins_dtor(ctx);
            ctx.builder.build_unconditional_branch(merge).unwrap();
            ctx.builder.position_at_end(merge);
            return;
        }
        let merge = ctx.context.append_basic_block(f, "enum.dtor.merge");
        let cases = self
            .variants()
            .iter()
            .enumerate()
            .filter_map(|(n, &t)| {
                t.has_dtor(ctx)
                    .then(|| t.llvm_type(ctx))
                    .flatten()
                    .map(|llt| (n, t, llt))
            })
            .map(|(n, t, llt)| {
                let blk = ctx
                    .context
                    .prepend_basic_block(merge, &format!("enum.dtor.{n}"));
                ctx.builder.position_at_end(blk);
                let val = ctx.builder.build_load(llt, ptr, "").unwrap();
                Value::with_addr(Some(val), None, t, ptr).ins_dtor(ctx);
                ctx.builder.build_unconditional_branch(merge).unwrap();
                (it.const_int(n as _, false), blk)
            })
            .collect::<Vec<_>>();
        ctx.builder.position_at_end(start);
        ctx.builder.build_switch(dsc, merge, &cases).unwrap();
        ctx.builder.position_at_end(merge);
    }
}

pub type Enum = EnumOrUnion;
pub type Union = EnumOrUnion;
submit_types!(EnumOrUnion);
