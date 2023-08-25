use super::*;
#[derive(Debug, Display)]
#[display(fmt = "type")]
pub struct TypeData(());
impl TypeData {
    pub const KIND: NonZeroU64 = make_id(b"type");
    pub fn new() -> &'static Self {
        static SELF: TypeData = Self(());
        &SELF
    }
}
impl Type for TypeData {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn attr<'src, 'ctx>(
        &self,
        val: &Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(InterData::Type(t)) = val.inter_val {
            t.static_attr(&attr.0, ctx)
                .ok_or_else(|| CobaltError::VariableDoesNotExist {
                    name: attr.0.clone(),
                    module: t.to_string(),
                    container: "type",
                    loc: attr.1,
                })
        } else {
            Err(CobaltError::AttrNotDefined {
                val: "type".to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            })
        }
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "module")]
pub struct Module(());
impl Module {
    pub const KIND: NonZeroU64 = make_id(b"module");
    pub fn new() -> &'static Self {
        static SELF: Module = Self(());
        &SELF
    }
}
impl Type for Module {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn attr<'src, 'ctx>(
        &self,
        val: &Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(InterData::Module(s, i, n)) = &val.inter_val {
            ctx.with_vars(|v| VarMap::lookup_in_mod((s, i), &attr.0, v.root()))
                .map_or_else(
                    || {
                        Err(CobaltError::VariableDoesNotExist {
                            name: attr.0.clone(),
                            module: n.clone(),
                            container: "module",
                            loc: attr.1,
                        })
                    },
                    |Symbol(x, d)| {
                        if d.init {
                            Ok(x.clone())
                        } else {
                            Err(CobaltError::UninitializedGlobal {
                                name: attr.0.clone(),
                                loc: attr.1,
                            })
                        }
                    },
                )
        } else {
            Err(CobaltError::AttrNotDefined {
                val: "module".to_string(),
                attr: attr.0,
                vloc: val.loc,
                aloc: attr.1,
            })
        }
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<error>")]
pub struct Error(());
impl Error {
    pub const KIND: NonZeroU64 = make_id(b"error");
    pub fn new() -> &'static Self {
        static SELF: Error = Self(());
        &SELF
    }
}
impl Type for Error {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
#[derive(Debug, Display)]
#[display(fmt = "null")]
pub struct Null(());
impl Null {
    pub const KIND: NonZeroU64 = make_id(b"null");
    pub fn new() -> &'static Self {
        static SELF: Null = Self(());
        &SELF
    }
}
impl Type for Null {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(0)
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}

static INTRINSIC_INTERN: Interner<Box<str>> = Interner::new();
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
#[display(fmt = "@{_0}")]
pub struct Intrinsic(Box<str>);
impl Intrinsic {
    pub const KIND: NonZeroU64 = make_id(b"intrin");
    #[ref_cast_custom]
    #[inline(always)]
    #[allow(clippy::borrowed_box)]
    fn from_ref(name: &Box<str>) -> &Self;
    pub fn new(name: impl Into<Box<str>>) -> &'static Self {
        Self::from_ref(INTRINSIC_INTERN.intern(name.into()))
    }
    pub fn new_ref(name: &str) -> &'static Self {
        Self::from_ref(INTRINSIC_INTERN.intern_ref(name))
    }
    pub fn name(&self) -> &str {
        &self.0
    }
}
impl Type for Intrinsic {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        serial_utils::save_str(out, self.name())
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        serial_utils::load_str(buf).map(|s| Self::new(s) as _)
    }
}
