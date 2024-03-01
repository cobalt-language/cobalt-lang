use super::*;
inventory::submit! {
    FunctionIntrinsic::new("type", |v, c, a, _| {
        extract_single("@type", v, c, a).map(|v| Value::make_type(v.data_type))
    }, true)
}
inventory::submit! {
    FunctionIntrinsic::new("size", |v, c, a, ctx| {
        extract_single("@size", v, c, a).and_then(|v| {
            v.into_type(ctx).map(|t| {
                Value::metaval(
                    InterData::Int(t.size().as_static().unwrap_or(0) as _),
                    crate::types::IntLiteral::new(),
                )
            })
        })
    }, true)
}
inventory::submit! {
    FunctionIntrinsic::new("sized", |v, c, a, ctx| {
        extract_single("@sized", v, c, a).and_then(|v| {
            v.into_type(ctx).map(|t| {
                Value::interpreted(
                    ctx.context
                        .bool_type()
                        .const_int(t.size().is_static() as _, false)
                        .into(),
                    InterData::Int(t.size().is_static() as _),
                    crate::types::Int::unsigned(1),
                )
            })
        })
    }, true)
}
inventory::submit! {
    FunctionIntrinsic::new("align", |v, c, a, ctx| {
        extract_single("@align", v, c, a).and_then(|v| {
            v.into_type(ctx).map(|t| {
                Value::metaval(
                    InterData::Int(t.align() as _),
                    crate::types::IntLiteral::new(),
                )
            })
        })
    }, true)
}
inventory::submit! {
    FunctionIntrinsic::new("typename", |v, c, a, ctx| {
        extract_single("@typename", v, c, a).and_then(|v| {
            v.into_type(ctx)
                .map(|t| Value::make_str(t.to_string(), ctx))
        })
    }, true)
}
fn extract_single<'src, 'ctx>(
    name: &'static str,
    v: SourceSpan,
    c: SourceSpan,
    a: Vec<Value<'src, 'ctx>>,
) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
    match a.len() {
        0 => Err(CobaltError::CannotCallWithArgs {
            val: name.to_string(),
            loc: v,
            args: vec![],
            aloc: Some(c),
            nargs: vec![ArgError::WrongNumArgs {
                found: 0,
                expected: 1,
                loc: c,
            }],
        }),
        1 => Ok(a.into_iter().next().unwrap()),
        n => Err(CobaltError::CannotCallWithArgs {
            val: name.to_string(),
            loc: v,
            args: a.into_iter().map(|v| v.data_type.to_string()).collect(),
            aloc: Some(c),
            nargs: vec![ArgError::WrongNumArgs {
                found: n,
                expected: 1,
                loc: c,
            }],
        }),
    }
}
