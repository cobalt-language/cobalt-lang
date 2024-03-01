use super::*;
use crate::types::{self, *};

static ENUM_AGG: EnumAggregator = EnumAggregator(false);
static UNION_AGG: EnumAggregator = EnumAggregator(true);

#[derive(Debug, Display, ConstIdentify, PartialEq, Eq, Hash)]
#[display("<{} aggregator>", if _0 {"union"} else {"enum"})]
struct EnumAggregator(bool);
impl TypeSerde for EnumAggregator {
    no_type_header!();
    impl_type_proxy!(bool, this => this.0, s => if s {&UNION_AGG} else {&ENUM_AGG});
}
impl Type for EnumAggregator {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn decay(&self) -> TypeRef {
        types::TypeData::new()
    }
    fn _can_iconv_to(&self, other: TypeRef, _ctx: &CompCtx) -> bool {
        other == types::TypeData::new()
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        _ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if target.0 == types::TypeData::new() {
            if let Some(InterData::Array(vec)) = val.inter_val {
                let res = vec
                    .into_iter()
                    .map(|t| {
                        let InterData::Spanned(_, i) = t else {
                            unreachable!()
                        };
                        let InterData::Type(t) = *i else {
                            unreachable!()
                        };
                        t
                    })
                    .collect::<Vec<_>>();
                Ok(Value::make_type(EnumOrUnion::new(res, self.0)))
            } else {
                unreachable!()
            }
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _has_bin_lhs(
        &self,
        other: TypeRef,
        op: &str,
        ctx: &CompCtx,
        _move_left: bool,
        _move_right: bool,
    ) -> bool {
        op == "|" && other.impl_convertible(types::TypeData::new(), ctx)
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        _move_left: bool,
        _move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if op.0 == "|" && rhs.data_type.impl_convertible(types::TypeData::new(), ctx) {
            let curr = rhs.loc;
            let rt = rhs.into_type(ctx)?;
            let Some(InterData::Array(vec)) = &mut lhs.inter_val else {
                unreachable!()
            };
            if let Some(prev) = vec.iter().filter_map(|i| {
                let InterData::Spanned(s, t) = i else {
                    unreachable!()
                };
                let InterData::Type(t) = &**t else {
                    unreachable!()
                };
                (rt == *t).then_some(*s)
            }).next() {
                Err(CobaltError::DuplicateEnumVariant {
                    is_union: self.0,
                    prev, curr,
                    ty: rt.to_string(),
                })
            } else {
                vec.push(InterData::Spanned(curr, Box::new(InterData::Type(rt))));
                Ok(lhs)
            }
        } else {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
        }
    }
}

inventory::submit! {
    ValueIntrinsic::new("enum", |_| Value::metaval(InterData::Array(vec![]), &ENUM_AGG), || &ENUM_AGG, true)
}
inventory::submit! {
    ValueIntrinsic::new("union", |_| Value::metaval(InterData::Array(vec![]), &UNION_AGG), || &UNION_AGG, true)
}

submit_types!(EnumAggregator);
