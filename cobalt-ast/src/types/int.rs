use super::*;
#[derive(Debug, PartialEq, Eq, Hash, Display, RefCastCustom)]
#[display(fmt = "{}{}", r#"if _0.1 {"u"} else {"i"}"#, "_0.0")]
#[repr(transparent)]
pub struct Int((u16, bool));
impl Int {
    #[ref_cast_custom]
    fn from_ref(val: &(u16, bool)) -> &Self;
    pub fn new(bits: u16, unsigned: bool) -> &'static Self {
        static INTERN: Interner<(u16, bool)> = Interner::new();
        Self::from_ref(INTERN.intern((bits, unsigned)))
    }
    pub fn signed(bits: u16) -> &'static Self {
        Self::new(bits, false)
    }
    pub fn unsigned(bits: u16) -> &'static Self {
        Self::new(bits, true)
    }
    pub fn isize(ctx: &CompCtx) -> &'static Self {
        Self::new(ctx.flags.word_size, false)
    }
    pub fn usize(ctx: &CompCtx) -> &'static Self {
        Self::new(ctx.flags.word_size, false)
    }
    pub fn bool() -> &'static Self {
        Self::new(1, true)
    }
    pub fn bits(&self) -> u16 {
        self.0 .0
    }
    pub fn is_signed(&self) -> bool {
        !self.0 .1
    }
    pub fn is_unsigned(&self) -> bool {
        self.0 .1
    }
}
impl ConcreteType for Int {
    const KIND: NonZeroU64 = make_id(b"int");
}
impl Type for Int {
    fn size(&self) -> SizeType {
        SizeType::Static(((self.0 .0 + 7) / 8) as _)
    }
    fn align(&self) -> u16 {
        1 << std::cmp::min(16 - self.0 .0.leading_zeros(), 6)
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        Some(ctx.context.custom_width_int_type(self.bits() as _).into())
    }
    fn compiled<'src, 'ctx>(
        &'static self,
        inter_val: &InterData<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        if let InterData::Int(v) = inter_val {
            Some(
                ctx.context
                    .custom_width_int_type(self.bits() as _)
                    .const_int(*v as _, self.is_signed())
                    .into(),
            )
        } else {
            None
        }
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(&self.0 .0.to_be_bytes())?;
        out.write_all(std::slice::from_ref(&u8::from(self.0 .1)))
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let mut arr = [0u8; 2];
        buf.read_exact(&mut arr)?;
        let bits = u16::from_be_bytes(arr);
        buf.read_exact(&mut arr[..1])?;
        Ok(Self::new(bits, arr[0] != 0))
    }
}
#[derive(Debug, Display)]
#[display(fmt = "<int literal>")]
pub struct IntLiteral(());
impl IntLiteral {
    pub fn new() -> &'static Self {
        static SELF: IntLiteral = Self(());
        &SELF
    }
}
impl ConcreteType for IntLiteral {
    const KIND: NonZeroU64 = make_id(b"intlit");
}
impl Type for IntLiteral {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn decay(&self) -> TypeRef {
        Int::signed(64)
    }
    fn compiled<'src, 'ctx>(
        &'static self,
        inter_val: &InterData<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        if let InterData::Int(v) = inter_val {
            Some(
                ctx.context
                    .custom_width_int_type(64)
                    .const_int(*v as _, true)
                    .into(),
            )
        } else {
            None
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is::<types::Int>() || other.is::<types::Float>()
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match target.0.kind() {
            types::Int::KIND => Ok(Value {
                data_type: target.0,
                ..val
            }),
            types::Float::KIND => Ok(Value::new(
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(
                        target
                            .0
                            .llvm_type(ctx)
                            .unwrap()
                            .into_float_type()
                            .const_float(v as _)
                            .into(),
                    )
                } else {
                    None
                },
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Float(v as _))
                } else {
                    None
                },
                target.0,
            )),
            _ => Err(cant_iconv(&val, target.0, target.1)),
        }
    }
    fn save(&self, _out: &mut dyn Write) -> io::Result<()> {
        Ok(())
    }
    fn load(_buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        Ok(Self::new())
    }
}
submit_types!(Int, IntLiteral);
