use crate::*;
use once_cell::sync::Lazy;

#[derive(Debug, Clone, Copy)]
pub struct ValueIntrinsic {
    pub name: &'static str,
    pub wraps: ValueCallType,
    pub ret: fn() -> TypeRef,
}
impl ValueIntrinsic {
    pub const fn new(name: &'static str, wraps: ValueCallType, ret: fn() -> TypeRef) -> Self {
        Self { name, wraps, ret }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionIntrinsic {
    pub name: &'static str,
    pub wraps: FunctionCallType,
}
impl FunctionIntrinsic {
    pub const fn new(name: &'static str, wraps: FunctionCallType) -> Self {
        Self { name, wraps }
    }
}

type ValueCallType = for<'src, 'ctx> fn(ctx: &CompCtx<'src, 'ctx>) -> Value<'src, 'ctx>;
type FunctionCallType = for<'src, 'ctx> fn(
    loc: SourceSpan,
    cparent: SourceSpan,
    args: Vec<Value<'src, 'ctx>>,
    ctx: &CompCtx<'src, 'ctx>,
) -> Result<Value<'src, 'ctx>, CobaltError<'src>>;

inventory::collect!(ValueIntrinsic);
inventory::collect!(FunctionIntrinsic);

pub static VALUE_INTRINSICS: Lazy<flurry::HashMap<&'static str, ValueIntrinsic>> =
    Lazy::new(|| {
        inventory::iter::<ValueIntrinsic>()
            .map(|&i| (i.name, i))
            .collect()
    });
pub static FUNCTION_INTRINSICS: Lazy<flurry::HashMap<&'static str, FunctionIntrinsic>> =
    Lazy::new(|| {
        inventory::iter::<FunctionIntrinsic>()
            .map(|&i| (i.name, i))
            .collect()
    });

pub mod misc;
pub mod types;
pub mod version;
