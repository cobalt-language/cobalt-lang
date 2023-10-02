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
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0.is::<TypeData>() {
            Ok(Value::make_type(types::Null::new()))
        } else {
            Err(cant_econv(&val, target.0, target.1))
        }
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}

submit_types!(TypeData, Module, Error, Null);
