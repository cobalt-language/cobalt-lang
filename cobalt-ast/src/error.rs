use crate::*;
pub fn cant_iconv<'src>(
    val: &Value<'src, '_>,
    to: TypeRef,
    tloc: Option<SourceSpan>,
) -> CobaltError<'src> {
    CobaltError::InvalidConversion {
        is_expl: false,
        val: val.data_type.to_string(),
        ty: to.to_string(),
        vloc: remove_unreachable(val.loc),
        tloc,
    }
}
pub fn cant_econv<'src>(
    val: &Value<'src, '_>,
    to: TypeRef,
    tloc: Option<SourceSpan>,
) -> CobaltError<'src> {
    CobaltError::InvalidConversion {
        is_expl: true,
        val: val.data_type.to_string(),
        ty: to.to_string(),
        vloc: remove_unreachable(val.loc),
        tloc,
    }
}
pub fn invalid_preop<'src>(
    val: &Value<'src, '_>,
    op: &'static str,
    oloc: SourceSpan,
) -> CobaltError<'src> {
    CobaltError::PreOpNotDefined {
        val: val.data_type.to_string(),
        op,
        vloc: val.loc,
        oloc,
    }
}
pub fn invalid_postop<'src>(
    val: &Value<'src, '_>,
    op: &'static str,
    oloc: SourceSpan,
) -> CobaltError<'src> {
    CobaltError::PostOpNotDefined {
        val: val.data_type.to_string(),
        op,
        vloc: val.loc,
        oloc,
    }
}
pub fn invalid_binop<'src>(
    lhs: &Value<'src, '_>,
    rhs: &Value<'src, '_>,
    op: &'static str,
    oloc: SourceSpan,
) -> CobaltError<'src> {
    CobaltError::BinOpNotDefined {
        lhs: lhs.data_type.to_string(),
        rhs: rhs.data_type.to_string(),
        op,
        lloc: lhs.loc,
        rloc: rhs.loc,
        oloc,
    }
}
pub fn invalid_sub<'src>(val: &Value<'src, '_>, idx: &Value<'src, '_>) -> CobaltError<'src> {
    CobaltError::SubscriptNotDefined {
        val: val.data_type.to_string(),
        sub: idx.data_type.to_string(),
        vloc: val.loc,
        sloc: idx.loc,
    }
}
pub fn invalid_call<'src: 'a, 'ctx: 'a, 'a, I: IntoIterator<Item = &'a Value<'src, 'ctx>>>(
    val: &Value<'src, 'ctx>,
    cparen: Option<SourceSpan>,
    args: I,
) -> CobaltError<'src> {
    let mut aloc = None;
    CobaltError::CannotCallWithArgs {
        val: val.data_type.to_string(),
        loc: cparen.map_or(val.loc, |cp| merge_spans(val.loc, cp)),
        args: args
            .into_iter()
            .map(|a| {
                aloc = Some(if let Some(l) = aloc {
                    merge_spans(l, a.loc)
                } else {
                    a.loc
                });
                a.data_type.to_string()
            })
            .collect(),
        aloc,
        nargs: vec![],
    }
}
