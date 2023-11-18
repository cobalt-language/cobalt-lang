use super::*;
use bstr::ByteSlice;
use std::str::Utf8Error;

#[derive(Debug, Display)]
#[display(fmt = "type")]
pub struct TypeData(());
impl TypeData {
    pub fn new() -> &'static Self {
        static SELF: TypeData = Self(());
        &SELF
    }
}
impl ConcreteType for TypeData {
    const KIND: NonZeroU64 = make_id(b"type");
}
impl Type for TypeData {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn attr<'src, 'ctx>(
        &self,
        val: Value<'src, 'ctx>,
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
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match op {
            "&" => Ok(if let Some(InterData::Type(t)) = val.inter_val {
                Value::make_type(types::Reference::new(if t.is::<types::Mut>() {
                    t
                } else {
                    t.decay()
                }))
            } else {
                Value::error()
            }),
            "*" => Ok(if let Some(InterData::Type(t)) = val.inter_val {
                Value::make_type(types::Pointer::new(if t.is::<types::Mut>() {
                    t
                } else {
                    t.decay()
                }))
            } else {
                Value::error()
            }),
            "mut" => Ok(if let Some(InterData::Type(t)) = val.inter_val {
                Value::make_type(types::Mut::new(t.decay()))
            } else {
                Value::error()
            }),
            _ => Err(invalid_preop(&val, op, oloc)),
        }
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match val.data_type.kind() {
            types::Int::KIND | types::IntLiteral::KIND => {
                if let Some(InterData::Int(v)) = idx.inter_val {
                    Ok(if let Some(InterData::Type(t)) = val.inter_val {
                        Value::make_type(types::SizedArray::new(t.decay(), v as _))
                    } else {
                        Value::error()
                    })
                } else {
                    Err(CobaltError::NotCompileTime { loc: idx.loc })
                }
            }
            types::Null::KIND => Ok(if let Some(InterData::Type(t)) = val.inter_val {
                Value::make_type(types::UnsizedArray::new(t.decay()))
            } else {
                Value::error()
            }),
            _ => Err(invalid_sub(&val, &idx)),
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
    pub fn new() -> &'static Self {
        static SELF: Module = Self(());
        &SELF
    }
}
impl ConcreteType for Module {
    const KIND: NonZeroU64 = make_id(b"module");
}
impl Type for Module {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn attr<'src, 'ctx>(
        &self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        use varmap::Symbol;
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
    pub fn new() -> &'static Self {
        static SELF: Error = Self(());
        &SELF
    }
}
impl ConcreteType for Error {
    const KIND: NonZeroU64 = make_id(b"error");
}
impl Type for Error {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn post_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn _has_bin_lhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        true
    }
    fn _has_bin_rhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        true
    }
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        true
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        true
    }
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::error())
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
    pub fn new() -> &'static Self {
        static SELF: Null = Self(());
        &SELF
    }
}
impl ConcreteType for Null {
    const KIND: NonZeroU64 = make_id(b"null");
}
impl Type for Null {
    fn size(&self) -> SizeType {
        SizeType::Static(0)
    }
    fn align(&self) -> u16 {
        0
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match op {
            "&" => Ok(Value::make_type(types::Reference::new(types::Null::new()))),
            "*" => Ok(Value::make_type(types::Pointer::new(types::Null::new()))),
            "mut" => Ok(Value::make_type(types::Mut::new(types::Null::new()))),
            _ => Err(invalid_preop(&val, op, oloc)),
        }
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match val.data_type.kind() {
            types::Int::KIND | types::IntLiteral::KIND => {
                if let Some(InterData::Int(v)) = idx.inter_val {
                    Ok(Value::make_type(types::SizedArray::new(
                        types::Null::new(),
                        v as _,
                    )))
                } else {
                    Err(CobaltError::NotCompileTime { loc: idx.loc })
                }
            }
            types::Null::KIND => Ok(Value::make_type(types::UnsizedArray::new(
                types::Null::new(),
            ))),
            _ => Err(invalid_sub(&val, &idx)),
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is::<TypeData>()
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        matches!(
            other.kind(),
            types::Pointer::KIND | types::Int::KIND | types::Float::KIND
        )
    }
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        true
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is::<TypeData>() {
            Ok(Value::make_type(types::Null::new()))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match target.0.kind() {
            types::Int::KIND => Ok(Value::metaval(InterData::Int(0), target.0)),
            types::Float::KIND => Ok(Value::interpreted(
                target.0.llvm_type(ctx).unwrap().const_zero(),
                InterData::Float(0.),
                target.0,
            )),
            types::Pointer::KIND => Ok(Value::compiled(
                target.0.llvm_type(ctx).unwrap().const_zero(),
                target.0,
            )),
            _ => Err(cant_econv(&val, target.0, target.1)),
        }
    }
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Ok(Value::null())
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}

#[derive(Debug, RefCastCustom)]
#[repr(transparent)]
pub struct Symbol(Box<[u8]>);
impl Symbol {
    #[ref_cast_custom]
    #[allow(clippy::borrowed_box)]
    fn from_ref(this: &Box<[u8]>) -> &Self;
    pub fn new_ref<S: AsRef<[u8]>>(name: S) -> &'static Self {
        Self::from_ref(SYMBOL_INTERN.intern_ref(name.as_ref()))
    }
    pub fn new<S: Into<Box<[u8]>>>(name: S) -> &'static Self {
        Self::from_ref(SYMBOL_INTERN.intern(name.into()))
    }
    pub fn value(&self) -> &[u8] {
        &self.0
    }
    pub fn value_str(&self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(&self.0)
    }
}
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("$")?;
        if let Ok(s) = self.0.to_str() {
            if s.chars().all(|c| c == '_' || c.is_alphanumeric()) {
                f.write_str(s)?;
                return Ok(());
            }
        }
        Debug::fmt(self.0.as_bstr(), f)
    }
}
impl ConcreteType for Symbol {
    const KIND: NonZeroU64 = make_id(b"symbol");
}
impl Type for Symbol {
    fn size(&'static self) -> SizeType {
        SizeType::Static(0)
    }
    fn align(&'static self) -> u16 {
        1
    }
    fn save(&'static self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(&(self.0.len() as u64).to_be_bytes())?;
        out.write_all(&self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef>
    where
        Self: ConcreteType,
    {
        let mut arr = [0; 8];
        buf.read_exact(&mut arr)?;
        let len = u64::from_be_bytes(arr);
        let mut vec = vec![0; len as _];
        buf.read_exact(&mut vec)?;
        Ok(Self::new(vec))
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is_and::<types::Reference>(|r| {
            r.base()
                .is_and::<types::UnsizedArray>(|a| a.elem() == types::Int::unsigned(8))
        }) || other.is_and::<types::Pointer>(|p| p.base() == types::Int::unsigned(8))
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is_and::<types::Reference>(|r| {
            r.base()
                .is_and::<types::UnsizedArray>(|a| a.elem() == types::Int::unsigned(8))
        }) {
            let arr = ctx.context.const_string(self.value(), true);
            let gv = ctx.module.add_global(arr.get_type(), None, "cobalt.str");
            gv.set_initializer(&arr);
            gv.set_linkage(inkwell::module::Linkage::Private);
            let sv = ctx.context.const_struct(
                &[
                    gv.as_pointer_value().into(),
                    ctx.context
                        .i64_type()
                        .const_int(self.value().len() as _, false)
                        .into(),
                ],
                false,
            );
            Ok(Value::interpreted(
                sv.into(),
                InterData::Array(
                    self.value()
                        .iter()
                        .map(|&v| InterData::Int(v as _))
                        .collect(),
                ),
                target.0,
            ))
        } else if target
            .0
            .is_and::<types::Pointer>(|p| p.base() == types::Int::unsigned(8))
        {
            let arr = ctx.context.const_string(self.value(), true);
            let gv = ctx.module.add_global(arr.get_type(), None, "cobalt.str");
            gv.set_initializer(&arr);
            gv.set_linkage(inkwell::module::Linkage::Private);
            Ok(Value::compiled(gv.as_pointer_value().into(), target.0))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
}
static SYMBOL_INTERN: Interner<Box<[u8]>> = Interner::new();

submit_types!(TypeData, Module, Error, Null, Symbol);
