use crate::*;
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
}
impl Type for Float {
    fn kind() -> usize {
        make_id("float")
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
}
