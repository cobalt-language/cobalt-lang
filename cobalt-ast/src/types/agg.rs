use super::*;
use std::collections::BTreeMap;

pub(super) fn tuple_size(types: &[TypeRef]) -> SizeType {
    let mut out = 0;
    let mut overall_align = 1;
    for (n, ty) in types.iter().enumerate() {
        match ty.size() {
            SizeType::Static(size) => {
                let align = ty.align() as u32;
                out += align - 1;
                out /= align;
                out += size;
                if align > overall_align {
                    overall_align = align
                }
            }
            SizeType::Dynamic => {
                return if types[n..].iter().any(|i| i.size() == SizeType::Meta) {
                    SizeType::Meta
                } else {
                    SizeType::Dynamic
                }
            }
            SizeType::Meta => return SizeType::Meta,
        }
    }
    out += overall_align - 1;
    out /= overall_align;
    out *= overall_align;
    SizeType::Static(out)
}

static TUPLE_INTERN: Interner<Box<[TypeRef]>> = Interner::new();
#[derive(Debug, ConstIdentify, Display, RefCastCustom)]
#[display(
    fmt = "({})",
    r#"_0.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")"#
)]
#[repr(transparent)]
pub struct Tuple(Box<[TypeRef]>);
impl Tuple {
    #[ref_cast_custom]
    #[inline(always)]
    #[allow(clippy::borrowed_box)]
    fn from_ref(types: &Box<[TypeRef]>) -> &Self;
    pub fn new(types: impl Into<Box<[TypeRef]>>) -> &'static Self {
        Self::from_ref(TUPLE_INTERN.intern(types.into()))
    }
    pub fn new_ref(types: &[TypeRef]) -> &'static Self {
        Self::from_ref(TUPLE_INTERN.intern_ref(types))
    }
    #[inline(always)]
    pub fn types(&self) -> &[TypeRef] {
        &self.0
    }
}
impl TypeSerde for Tuple {
    no_type_header!();
    impl_type_proxy!(
        Cow<'static, [TypeRef]>,
        |this: &'static Self| this.types().into(),
        Self::new
    );
}
impl Type for Tuple {
    fn align(&self) -> u16 {
        self.0.iter().map(|v| v.align()).fold(1, |old, new| {
            if old == 0 || new == 0 {
                0
            } else {
                std::cmp::max(old, new)
            }
        })
    }
    fn size(&self) -> SizeType {
        tuple_size(&self.0)
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.types()
            .iter()
            .map(|t| t.llvm_type(ctx))
            .collect::<Option<Box<_>>>()
            .map(|f| ctx.context.struct_type(&f, false).into())
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        self.types().iter().any(|v| v.has_dtor(ctx))
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        if let Some(BasicValueEnum::StructValue(sv)) = val.comp_val {
            for n in 0..self.types().len() {
                let v = ctx.builder.build_extract_value(sv, n as _, "").unwrap();
                Value::with_addr(
                    Some(v),
                    None,
                    self.types()[n],
                    ctx.builder
                        .build_struct_gep(sv.get_type(), val.addr(ctx).unwrap(), n as _, "")
                        .unwrap(),
                )
                .ins_dtor(ctx);
            }
        }
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.types().iter().any(|ty| ty.is_linear(ctx))
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is::<types::TypeData>()
            && self
                .types()
                .iter()
                .all(|t| t.impl_convertible(types::TypeData::new(), ctx))
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is::<types::TypeData>() {
            if let Some(InterData::Array(v)) = val.inter_val {
                let types = v
                    .into_iter()
                    .zip(self.types())
                    .map(|(v, &t)| Value::metaval(v, t).into_type(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::make_type(Self::new(types)))
            } else {
                Ok(Value::error())
            }
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
}
static STRUCT_INTERN: Interner<(Box<[TypeRef]>, BTreeMap<Box<str>, usize>)> = Interner::new();

#[derive(Debug, ConstIdentify, RefCastCustom)]
#[repr(transparent)]
pub struct Struct((Box<[TypeRef]>, BTreeMap<Box<str>, usize>));
impl Struct {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(inner: &(Box<[TypeRef]>, BTreeMap<Box<str>, usize>)) -> &Self;
    pub fn new(fields: impl IntoIterator<Item = (impl Into<Box<str>>, TypeRef)>) -> &'static Self {
        let mut vec = fields
            .into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect::<Vec<_>>();
        vec.sort_by(|(ln, lt), (rn, rt)| Self::sort_fields((&**ln, *lt), (&**rn, *rt)));
        let (types, fields): (Vec<_>, _) = vec
            .into_iter()
            .enumerate()
            .map(|(n, (name, ty))| (ty, (name, n)))
            .unzip();
        Self::from_ref(STRUCT_INTERN.intern((types.into(), fields)))
    }
    /// Create a struct assuming the fields are already in the correct order
    /// # Safety
    /// the fields must be in the correct order
    pub unsafe fn new_arranged(
        types: impl Into<Box<[TypeRef]>>,
        fields: BTreeMap<Box<str>, usize>,
    ) -> &'static Self {
        Self::from_ref(STRUCT_INTERN.intern((types.into(), fields)))
    }
    #[inline(always)]
    pub fn types(&self) -> &[TypeRef] {
        &self.0 .0
    }
    #[inline(always)]
    pub fn fields(&self) -> &BTreeMap<Box<str>, usize> {
        &self.0 .1
    }
    pub fn field_iterator(
        &self,
    ) -> impl DoubleEndedIterator<Item = (&str, TypeRef)> + ExactSizeIterator {
        self.fields().iter().map(|(k, n)| (&**k, self.types()[*n]))
    }
    pub fn sort_fields((ln, lt): (&str, TypeRef), (rn, rt): (&str, TypeRef)) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match lt.align().cmp(&rt.align()) {
            Ordering::Less => Ordering::Greater,
            Ordering::Greater => Ordering::Less,
            Ordering::Equal => match (lt.size(), rt.size()) {
                (SizeType::Meta, _)
                | (_, SizeType::Meta)
                | (SizeType::Dynamic, SizeType::Dynamic) => ln.cmp(rn),
                (SizeType::Static(ls), SizeType::Static(rs)) => ls.cmp(&rs),
                (SizeType::Static(_), SizeType::Dynamic) => Ordering::Less,
                (SizeType::Dynamic, SizeType::Static(_)) => Ordering::Greater,
            },
        }
    }
}
impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut rev_lookup = ["<error>"].repeat(self.0 .0.len());
        for (name, &idx) in self.fields() {
            rev_lookup[idx] = name;
        }
        f.write_str("{")?;
        let mut rem = self.fields().len();
        for (n, ty) in self.types().iter().enumerate() {
            write!(f, "{}: {ty}", rev_lookup[n])?;
            rem -= 1;
            if rem > 0 {
                f.write_str(", ")?;
            }
        }
        f.write_str("}")
    }
}
impl TypeSerde for Struct {
    no_type_header!();
    impl_type_proxy!(
        hashbrown::HashMap<Cow<'static, str>, TypeRef>,
        |this: &'static Struct| this.field_iterator().map(|(k, v)| (k.into(), v)).collect(),
        Self::new
    );
}
impl Type for Struct {
    fn align(&self) -> u16 {
        self.0 .0.iter().map(|v| v.align()).max().unwrap_or(1)
    }
    fn size(&self) -> SizeType {
        tuple_size(&self.0 .0)
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.types()
            .iter()
            .map(|t| t.llvm_type(ctx))
            .collect::<Option<Box<_>>>()
            .map(|f| ctx.context.struct_type(&f, false).into())
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        self.types().iter().any(|v| v.has_dtor(ctx))
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        if let Some(BasicValueEnum::StructValue(sv)) = val.comp_val {
            for n in 0..self.types().len() {
                let v = ctx.builder.build_extract_value(sv, n as _, "").unwrap();
                Value::with_addr(
                    Some(v),
                    None,
                    self.types()[n],
                    ctx.builder
                        .build_struct_gep(sv.get_type(), val.addr(ctx).unwrap(), n as _, "")
                        .unwrap(),
                )
                .ins_dtor(ctx);
            }
        }
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.types().iter().any(|ty| ty.is_linear(ctx))
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let vt = val.data_type;
        let vl = val.loc;
        let ty = val.into_type(ctx)?;
        match op {
            "&" => Ok(Value::make_type(types::Reference::new(ty))),
            "*" => Ok(Value::make_type(types::Pointer::new(ty))),
            "mut" => Ok(Value::make_type(types::Mut::new(ty))),
            _ => Err(invalid_preop(
                &Value {
                    data_type: vt,
                    loc: vl,
                    ..Value::null()
                },
                op,
                oloc,
            )),
        }
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let vt = val.data_type;
        let vl = val.loc;
        let ty = val.into_type(ctx)?;
        match vt.kind() {
            types::Int::KIND | types::IntLiteral::KIND => {
                if let Some(InterData::Int(v)) = idx.inter_val {
                    Ok(Value::make_type(types::SizedArray::new(ty, v as _)))
                } else {
                    Err(CobaltError::NotCompileTime { loc: idx.loc })
                }
            }
            types::Null::KIND => Ok(Value::make_type(types::UnsizedArray::new(ty))),
            _ => Err(invalid_sub(
                &Value {
                    data_type: vt,
                    loc: vl,
                    ..Value::null()
                },
                &idx,
            )),
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is::<types::TypeData>()
            && self
                .types()
                .iter()
                .all(|t| t.impl_convertible(types::TypeData::new(), ctx))
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is::<types::TypeData>() {
            if let Some(InterData::Array(v)) = val.inter_val {
                let types = v
                    .into_iter()
                    .zip(self.types())
                    .map(|(v, &t)| Value::metaval(v, t).into_type(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                unsafe {
                    Ok(Value::make_type(Self::new_arranged(
                        types,
                        self.fields().clone(),
                    )))
                }
            } else {
                Ok(Value::error())
            }
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(&idx) = self.fields().get(&*attr.0) {
            Ok(Value::new(
                if let Some(BasicValueEnum::StructValue(v)) = val.value(ctx) {
                    ctx.builder.build_extract_value(v, idx as _, "").ok()
                } else {
                    None
                },
                if let Some(InterData::Array(mut v)) = val.inter_val {
                    Some(v.swap_remove(idx))
                } else {
                    None
                },
                self.types()[idx],
            ))
        } else {
            Err(invalid_attr(&val, attr.0, attr.1))
        }
    }
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        self.fields().contains_key(attr)
    }
    fn _has_mut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        self.fields().contains_key(attr)
    }
    fn _has_refmut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        self.fields().contains_key(attr)
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(&idx) = self.fields().get(&*attr.0) {
            Ok(Value::new(
                if let Some(BasicValueEnum::PointerValue(v)) = val.value(ctx) {
                    ctx.builder
                        .build_struct_gep(self.llvm_type(ctx).unwrap(), v, idx as _, "")
                        .ok()
                        .map(From::from)
                } else {
                    None
                },
                if let Some(InterData::Array(mut v)) = val.inter_val {
                    Some(v.swap_remove(idx))
                } else {
                    None
                },
                self.types()[idx].add_ref(false),
            ))
        } else {
            Err(invalid_attr(&val, attr.0, attr.1))
        }
    }
    fn _mut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(&idx) = self.fields().get(&*attr.0) {
            Ok(Value::new(
                if let Some(BasicValueEnum::PointerValue(v)) = val.value(ctx) {
                    ctx.builder
                        .build_struct_gep(self.llvm_type(ctx).unwrap(), v, idx as _, "")
                        .ok()
                        .map(From::from)
                } else {
                    None
                },
                if let Some(InterData::Array(mut v)) = val.inter_val {
                    Some(v.swap_remove(idx))
                } else {
                    None
                },
                types::Mut::new(self.types()[idx]),
            ))
        } else {
            Err(invalid_attr(&val, attr.0, attr.1))
        }
    }
    fn _refmut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(&idx) = self.fields().get(&*attr.0) {
            Ok(Value::new(
                if let Some(BasicValueEnum::PointerValue(v)) = val.value(ctx) {
                    ctx.builder
                        .build_struct_gep(self.llvm_type(ctx).unwrap(), v, idx as _, "")
                        .ok()
                        .map(From::from)
                } else {
                    None
                },
                if let Some(InterData::Array(mut v)) = val.inter_val {
                    Some(v.swap_remove(idx))
                } else {
                    None
                },
                self.types()[idx].add_ref(true),
            ))
        } else {
            Err(invalid_attr(&val, attr.0, attr.1))
        }
    }
}

#[derive(Debug, ConstIdentify, Display, RefCastCustom)]
#[repr(transparent)]
#[display(fmt = "{_0}[]")]
pub struct UnsizedArray(TypeRef);
impl UnsizedArray {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(inner: &TypeRef) -> &Self;
    pub fn new(elem: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(elem))
    }
    pub fn elem(&self) -> TypeRef {
        self.0
    }
}
impl TypeSerde for UnsizedArray {
    no_type_header!();
    impl_type_proxy!(TypeRef, Self::elem, Self::new);
}
impl Type for UnsizedArray {
    fn size(&self) -> SizeType {
        SizeType::Dynamic
    }
    fn align(&self) -> u16 {
        self.0.align()
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        Some(
            ctx.context
                .struct_type(
                    &[self.elem().ptr_type(ctx)?, ctx.context.i64_type().into()],
                    false,
                )
                .into(),
        )
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.elem().is_linear(ctx)
    }
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        matches!(attr, "ptr" | "len")
    }
    fn _has_refmut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        matches!(attr, "ptr" | "len")
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match &*attr.0 {
            "ptr" => Ok(Value::new(
                if let Some(BasicValueEnum::StructValue(v)) = val.value(ctx) {
                    ctx.builder.build_extract_value(v, 0, "").ok()
                } else {
                    None
                },
                None,
                types::Pointer::new(self.elem()),
            )),
            "len" => Ok(Value::new(
                if let Some(BasicValueEnum::StructValue(v)) = val.value(ctx) {
                    ctx.builder.build_extract_value(v, 1, "").ok()
                } else {
                    None
                },
                None,
                types::Int::unsigned(64),
            )),
            _ => Err(CobaltError::AttrNotDefined {
                val: self.to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            }),
        }
    }
    fn _refmut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match &*attr.0 {
            "ptr" => Ok(Value::new(
                if let Some(BasicValueEnum::StructValue(v)) = val.value(ctx) {
                    ctx.builder.build_extract_value(v, 0, "").ok()
                } else {
                    None
                },
                None,
                types::Pointer::new(types::Mut::new(self.elem())),
            )),
            "len" => Ok(Value::new(
                if let Some(BasicValueEnum::StructValue(v)) = val.value(ctx) {
                    ctx.builder.build_extract_value(v, 1, "").ok()
                } else {
                    None
                },
                None,
                types::Int::unsigned(64),
            )),
            _ => Err(CobaltError::AttrNotDefined {
                val: self.to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            }),
        }
    }
}
#[derive(Debug, ConstIdentify, Display, RefCastCustom)]
#[repr(transparent)]
#[display(fmt = "{}[{}]", "_0.0", "_0.1")]
pub struct SizedArray((TypeRef, u32));
impl SizedArray {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(inner: &(TypeRef, u32)) -> &Self;
    pub fn new(elem: TypeRef, len: u32) -> &'static Self {
        static INTERN: Interner<(TypeRef, u32)> = Interner::new();
        Self::from_ref(INTERN.intern((elem, len)))
    }
    pub fn elem(&self) -> TypeRef {
        self.0 .0
    }
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.0 .1
    }
}

#[doc(hidden)]
#[derive(Serialize, Deserialize)]
pub struct SAProxy {
    elem: TypeRef,
    len: u32,
}
impl TypeSerde for SizedArray {
    no_type_header!();
    impl_type_proxy!(SAProxy, this => SAProxy { elem: this.elem(), len: this.len() }, SAProxy { elem, len } => Self::new(elem, len));
}
impl Type for SizedArray {
    fn size(&self) -> SizeType {
        self.elem().size().map_static(|l| l * self.len())
    }
    fn align(&self) -> u16 {
        self.elem().align()
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        Some(self.elem().llvm_type(ctx)?.array_type(self.len()).into())
    }
    fn has_dtor(&'static self, ctx: &CompCtx) -> bool {
        self.elem().has_dtor(ctx)
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        let Some(ib) = ctx.builder.get_insert_block() else {
            return;
        };
        let Some(f) = ib.get_parent() else { return };
        let Some(pv) = val.addr(ctx) else { return };
        let at = self.llvm_type(ctx).unwrap();
        let bb = ctx.context.append_basic_block(f, "arr.dtor.loop");
        let ex = ctx.context.append_basic_block(f, "arr.dtor.exit");
        ctx.builder.build_unconditional_branch(bb).unwrap();
        ctx.builder.position_at_end(bb);
        let i64t = ctx.context.i64_type();
        let phi = ctx.builder.build_phi(i64t, "").unwrap();
        let phiv = phi.as_basic_value().into_int_value();
        let pv = unsafe {
            ctx.builder
                .build_in_bounds_gep(at, pv, &[i64t.const_zero(), phiv], "")
        }
        .unwrap();
        Value::with_addr(
            ctx.builder.build_load(at, pv, "").ok(),
            None,
            self.elem(),
            pv,
        )
        .ins_dtor(ctx);
        phi.add_incoming(&[(&i64t.const_zero(), ib)]);
        let next = ctx
            .builder
            .build_int_add(phiv, i64t.const_int(1, false), "")
            .unwrap();
        phi.add_incoming(&[(&next, bb)]);
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.elem().is_linear(ctx)
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is_and::<types::Pointer>(|r| r.base() == self.elem())
            || other.is_and::<types::Reference>(|b| {
                b.base()
                    .is_and::<types::UnsizedArray>(|a| a.elem() == self.elem())
            })
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target
            .0
            .is_and::<types::Pointer>(|r| r.base() == self.elem())
        {
            Ok(Value {
                data_type: target.0,
                comp_val: val.addr(ctx).map(From::from),
                ..val
            })
        } else if target.0.is_and::<types::Reference>(|b| {
            b.base()
                .is_and::<types::UnsizedArray>(|a| a.elem() == self.elem())
        }) {
            Ok(Value::new(
                val.addr(ctx).map(|v| {
                    ctx.context
                        .const_struct(
                            &[
                                v.into(),
                                ctx.context
                                    .i64_type()
                                    .const_int(self.len() as _, false)
                                    .into(),
                            ],
                            false,
                        )
                        .into()
                }),
                val.inter_val,
                target.0,
            ))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _ref_iconv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target
            .0
            .is_and::<types::Pointer>(|r| r.base() == self.elem())
        {
            Ok(Value {
                data_type: target.0,
                ..val
            })
        } else if target.0.is_and::<types::Reference>(|b| {
            b.base()
                .is_and::<types::UnsizedArray>(|a| a.elem() == self.elem())
        }) {
            Ok(Value::new(
                val.comp_val.map(|v| {
                    ctx.context
                        .const_struct(
                            &[
                                v,
                                ctx.context
                                    .i64_type()
                                    .const_int(self.len() as _, false)
                                    .into(),
                            ],
                            false,
                        )
                        .into()
                }),
                val.inter_val,
                target.0,
            ))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _refmut_iconv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is_and::<types::Pointer>(|r| {
            r.base() == self.elem() || r.base().is_and::<types::Mut>(|m| m.base() == self.elem())
        }) {
            Ok(Value {
                data_type: target.0,
                ..val
            })
        } else if target.0.is_and::<types::Reference>(|b| {
            b.base().is_and::<types::Mut>(|m| {
                m.base()
                    .is_and::<types::UnsizedArray>(|a| a.elem() == self.elem())
            })
        }) {
            Ok(Value::new(
                val.comp_val.map(|v| {
                    ctx.context
                        .const_struct(
                            &[
                                v,
                                ctx.context
                                    .i64_type()
                                    .const_int(self.len() as _, false)
                                    .into(),
                            ],
                            false,
                        )
                        .into()
                }),
                val.inter_val,
                target.0,
            ))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _can_ref_iconv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        target.is_and::<types::Pointer>(|r| r.base() == self.elem())
    }
    fn _can_refmut_iconv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        target.is_and::<types::Pointer>(|r| {
            r.base() == self.elem() || r.base().is_and::<types::Mut>(|m| m.base() == self.elem())
        })
    }
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        matches!(attr, "ptr" | "len")
    }
    fn _has_refmut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        matches!(attr, "ptr" | "len")
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match &*attr.0 {
            "ptr" => Ok(Value {
                data_type: types::Pointer::new(self.elem()),
                ..val
            }),
            "len" => Ok(Value::metaval(
                InterData::Int(self.len() as _),
                types::IntLiteral::new(),
            )),
            _ => Err(CobaltError::AttrNotDefined {
                val: self.to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            }),
        }
    }
    fn _refmut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match &*attr.0 {
            "ptr" => Ok(Value {
                data_type: types::Pointer::new(types::Mut::new(self.elem())),
                ..val
            }),
            "len" => Ok(Value::metaval(
                InterData::Int(self.len() as _),
                types::IntLiteral::new(),
            )),
            _ => Err(CobaltError::AttrNotDefined {
                val: self.to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            }),
        }
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        mut idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        loop {
            match idx.data_type.kind() {
                types::Reference::KIND | types::Mut::KIND => idx = idx.decay(ctx),
                types::Int::KIND => {
                    return Ok(Value::new(
                        val.addr(ctx).and_then(|pv| {
                            if let (Some(llt), Some(BasicValueEnum::IntValue(iv))) =
                                (self.elem().llvm_type(ctx), idx.value(ctx))
                            {
                                unsafe { ctx.builder.build_gep(llt, pv, &[iv], "") }
                                    .ok()
                                    .map(From::from)
                            } else {
                                None
                            }
                        }),
                        if let (Some(InterData::Array(arr)), Some(InterData::Int(idx))) =
                            (val.inter_val, idx.inter_val)
                        {
                            arr.get(idx as usize).cloned()
                        } else {
                            None
                        },
                        self.elem().add_ref(false),
                    ))
                }
                types::IntLiteral::KIND => {
                    return Ok(Value::new(
                        val.addr(ctx).and_then(|pv| {
                            if let (Some(llt), Some(InterData::Int(iv))) =
                                (self.elem().llvm_type(ctx), &idx.inter_val)
                            {
                                unsafe {
                                    ctx.builder.build_gep(
                                        llt,
                                        pv,
                                        &[ctx.context.i64_type().const_int(*iv as _, true)],
                                        "",
                                    )
                                }
                                .ok()
                                .map(From::from)
                            } else {
                                None
                            }
                        }),
                        if let (Some(InterData::Array(arr)), Some(InterData::Int(idx))) =
                            (val.inter_val, idx.inter_val)
                        {
                            arr.get(idx as usize).cloned()
                        } else {
                            None
                        },
                        self.elem().add_ref(false),
                    ))
                }
                _ => return Err(invalid_sub(&val, &idx)),
            }
        }
    }
    fn _mut_subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        mut idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        loop {
            match idx.data_type.kind() {
                types::Reference::KIND | types::Mut::KIND => idx = idx.decay(ctx),
                types::Int::KIND => {
                    return Ok(Value::new(
                        val.addr(ctx).and_then(|pv| {
                            if let (Some(llt), Some(BasicValueEnum::IntValue(iv))) =
                                (self.elem().llvm_type(ctx), idx.value(ctx))
                            {
                                unsafe { ctx.builder.build_gep(llt, pv, &[iv], "") }
                                    .ok()
                                    .map(From::from)
                            } else {
                                None
                            }
                        }),
                        if let (Some(InterData::Array(arr)), Some(InterData::Int(idx))) =
                            (val.inter_val, idx.inter_val)
                        {
                            arr.get(idx as usize).cloned()
                        } else {
                            None
                        },
                        self.elem().add_ref(false),
                    ))
                }
                types::IntLiteral::KIND => {
                    return Ok(Value::new(
                        if let (
                            Some(llt),
                            Some(BasicValueEnum::PointerValue(pv)),
                            Some(InterData::Int(iv)),
                        ) = (self.elem().llvm_type(ctx), val.value(ctx), &idx.inter_val)
                        {
                            unsafe {
                                ctx.builder.build_gep(
                                    llt,
                                    pv,
                                    &[ctx.context.i64_type().const_int(*iv as _, true)],
                                    "",
                                )
                            }
                            .ok()
                            .map(From::from)
                        } else {
                            None
                        },
                        if let (Some(InterData::Array(arr)), Some(InterData::Int(idx))) =
                            (val.inter_val, idx.inter_val)
                        {
                            arr.get(idx as usize).cloned()
                        } else {
                            None
                        },
                        self.elem().add_ref(false),
                    ))
                }
                _ => return Err(invalid_sub(&val, &idx)),
            }
        }
    }
}
submit_types!(Tuple, Struct);
