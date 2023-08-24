use super::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum FPType {
    #[display(fmt = "f16")]
    F16,
    #[display(fmt = "f32")]
    F32,
    #[display(fmt = "f64")]
    F64,
    #[display(fmt = "f128")]
    F128,
}
static F16: Float = Float(FPType::F16);
static F32: Float = Float(FPType::F32);
static F64: Float = Float(FPType::F64);
static F128: Float = Float(FPType::F128);
#[derive(Debug, Display)]
pub struct Float(FPType);
impl Float {
    pub const KIND: NonZeroU64 = make_id(b"float");
    pub fn new(val: FPType) -> &'static Self {
        match val {
            FPType::F16 => &F16,
            FPType::F32 => &F32,
            FPType::F64 => &F64,
            FPType::F128 => &F128,
        }
    }
    pub fn f16() -> &'static Self {
        &F16
    }
    pub fn f32() -> &'static Self {
        &F32
    }
    pub fn f64() -> &'static Self {
        &F64
    }
    pub fn f128() -> &'static Self {
        &F128
    }
    pub fn kind(&self) -> FPType {
        self.0
    }
}
impl Type for Float {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Static(match self.0 {
            FPType::F16 => 2,
            FPType::F32 => 4,
            FPType::F64 => 8,
            FPType::F128 => 16,
        })
    }
    fn align(&self) -> u16 {
        match self.0 {
            FPType::F16 => 2,
            FPType::F32 => 4,
            _ => 8,
        }
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        Some(
            match self.kind() {
                FPType::F16 => ctx.context.f16_type(),
                FPType::F32 => ctx.context.f32_type(),
                FPType::F64 => ctx.context.f64_type(),
                FPType::F128 => ctx.context.f128_type(),
            }
            .into(),
        )
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(std::slice::from_ref(&match self.0 {
            FPType::F16 => 0,
            FPType::F32 => 1,
            FPType::F64 => 2,
            FPType::F128 => 3,
        }))
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        Ok(Self::new(match c {
            0 => FPType::F16,
            1 => FPType::F32,
            2 => FPType::F64,
            3 => FPType::F128,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("expected 0, 1, 2, or 3 for float type, got {c}"),
                ))
            }
        }))
    }
}
