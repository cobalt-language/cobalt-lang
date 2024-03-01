use super::*;
use crate::types;
static VERSION_MAJOR: Lazy<usize> = Lazy::new(|| env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap());
static VERSION_MINOR: Lazy<usize> = Lazy::new(|| env!("CARGO_PKG_VERSION_MINOR").parse().unwrap());
static VERSION_PATCH: Lazy<usize> = Lazy::new(|| env!("CARGO_PKG_VERSION_PATCH").parse().unwrap());

inventory::submit! {
    ValueIntrinsic::new(
        "version_major",
        |_| Value::metaval(
            InterData::Int(*VERSION_MAJOR as _),
            types::IntLiteral::new(),
        ),
        || types::IntLiteral::new(),
        true
    )
}
inventory::submit! {
    ValueIntrinsic::new(
        "version_minor",
        |_| Value::metaval(
            InterData::Int(*VERSION_MINOR as _),
            types::IntLiteral::new(),
        ),
        || types::IntLiteral::new(),
        true
    )
}
inventory::submit! {
    ValueIntrinsic::new(
        "version_patch",
        |_| Value::metaval(
            InterData::Int(*VERSION_PATCH as _),
            types::IntLiteral::new(),
        ),
        || types::IntLiteral::new(),
        true
    )
}
inventory::submit! {
    ValueIntrinsic::new(
        "version_string",
        |ctx| Value::make_str(env!("CARGO_PKG_VERSION"), ctx),
        || types::SizedArray::new(types::Int::unsigned(8), env!("CARGO_PKG_VERSION").len() as _),
        true
    )
}
inventory::submit! {
    ValueIntrinsic::new(
        "version",
        make_version,
        || unsafe {
            types::Struct::new_arranged(
                [types::Int::unsigned(64) as _; 3],
                [
                    ("major".into(), 0),
                    ("minor".into(), 1),
                    ("patch".into(), 2),
                ]
                .into_iter()
                .collect(),
            )
        },
        true
    )
}
fn make_version<'src, 'ctx>(ctx: &CompCtx<'src, 'ctx>) -> Value<'src, 'ctx> {
    let i64t = ctx.context.i64_type();
    let u64t = types::Int::unsigned(64) as _;
    Value::interpreted(
        ctx.context
            .const_struct(
                &[
                    i64t.const_int(*VERSION_MAJOR as _, false).into(),
                    i64t.const_int(*VERSION_MINOR as _, false).into(),
                    i64t.const_int(*VERSION_PATCH as _, false).into(),
                ],
                false,
            )
            .into(),
        InterData::Array(vec![
            InterData::Int(*VERSION_MAJOR as _),
            InterData::Int(*VERSION_MINOR as _),
            InterData::Int(*VERSION_PATCH as _),
        ]),
        unsafe {
            types::Struct::new_arranged(
                [u64t; 3],
                [
                    ("major".into(), 0),
                    ("minor".into(), 1),
                    ("patch".into(), 2),
                ]
                .into_iter()
                .collect(),
            )
        },
    )
}
