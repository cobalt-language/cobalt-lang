use super::*;
use std::collections::BTreeMap;

fn tuple_size(types: &[TypeRef]) -> SizeType {
    let mut out = 0;
    let mut overall_align = 1;
    for ty in types {
        let size = ty.size();
        if let SizeType::Static(size) = size {
            let align = ty.align() as u32;
            out += align - 1;
            out /= align;
            out += size;
            if align > overall_align {
                overall_align = align
            }
        } else {
            return size;
        }
    }
    out += overall_align - 1;
    out /= overall_align;
    SizeType::Static(out)
}

static TUPLE_INTERN: Interner<Box<[TypeRef]>> = Interner::new();
#[derive(Debug, Display, RefCastCustom)]
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
impl ConcreteType for Tuple {
    const KIND: NonZeroU64 = make_id(b"tuple");
}
impl Type for Tuple {
    fn align(&self) -> u16 {
        self.0.iter().map(|v| v.align()).max().unwrap_or(1)
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
    fn ins_dtor<'ctx>(&'static self, comp_val: BasicValueEnum<'ctx>, ctx: &CompCtx<'_, 'ctx>) {
        if let BasicValueEnum::StructValue(sv) = comp_val {
            for n in 0..self.types().len() {
                let v = ctx.builder.build_extract_value(sv, n as _, "").unwrap();
                self.types()[n].ins_dtor(v, ctx);
            }
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
                Ok(Value::make_type(Self::new(types)))
            } else {
                Ok(Value::error())
            }
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        self.0.iter().try_for_each(|&ty| save_type(out, ty))?;
        out.write_all(&[0])
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new(
            std::iter::from_fn(|| load_type_opt(buf).transpose()).collect::<Result<Vec<_>, _>>()?,
        ))
    }
}
static STRUCT_INTERN: Interner<(Box<[TypeRef]>, BTreeMap<Box<str>, usize>)> = Interner::new();

#[derive(Debug, RefCastCustom)]
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
impl ConcreteType for Struct {
    const KIND: NonZeroU64 = make_id(b"struct");
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
    fn ins_dtor<'ctx>(&'static self, comp_val: BasicValueEnum<'ctx>, ctx: &CompCtx<'_, 'ctx>) {
        if let BasicValueEnum::StructValue(sv) = comp_val {
            for n in 0..self.types().len() {
                let v = ctx.builder.build_extract_value(sv, n as _, "").unwrap();
                self.types()[n].ins_dtor(v, ctx);
            }
        }
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
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(&self.types().len().to_be_bytes())?;
        for &ty in self.types() {
            save_type(out, ty)?;
        }
        let mut rev_lookup = ["<error>"].repeat(self.types().len());
        for (name, idx) in &self.0 .1 {
            rev_lookup[*idx] = name;
        }
        for key in rev_lookup {
            serial_utils::save_str(out, key)?;
        }
        Ok(())
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let mut arr = [0u8; 8];
        buf.read_exact(&mut arr)?;
        let len = u64::from_be_bytes(arr);
        let mut types = Vec::with_capacity(len as _);
        for _ in 0..len {
            types.push(load_type(buf)?);
        }
        let mut fields = BTreeMap::new();
        for n in 0..len {
            fields.insert(serial_utils::load_str(buf)?.into(), n as _);
        }
        unsafe { Ok(Self::new_arranged(types, fields)) }
    }
}

#[derive(Debug, Display, RefCastCustom)]
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
impl ConcreteType for UnsizedArray {
    const KIND: NonZeroU64 = make_id(b"uarr");
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
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new(load_type(buf)?))
    }
}
#[derive(Debug, Display, RefCastCustom)]
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
impl ConcreteType for SizedArray {
    const KIND: NonZeroU64 = make_id(b"iarr");
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
        } else {
            Err(cant_econv(&val, target.0, target.1))
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
        } else {
            Err(cant_econv(&val, target.0, target.1))
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
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.elem())?;
        out.write_all(&self.len().to_be_bytes())
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let elem = load_type(buf)?;
        let mut arr = [0u8; 4];
        buf.read_exact(&mut arr)?;
        Ok(Self::new(elem, u32::from_be_bytes(arr)))
    }
}
submit_types!(Tuple, Struct);
