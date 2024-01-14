use super::*;
use inkwell::IntPredicate::*;
#[derive(Debug, ConstIdentify, PartialEq, Eq, Hash, Display, RefCastCustom)]
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
impl TypeSerde for Int {
    no_type_header!();
    impl_type_proxy!(
        deranged::RangedI32<{-(u16::MAX as i32)}, {u16::MAX as _}>,
        this => ((this.bits() as i32) * if this.is_unsigned() {-1} else {1}).try_into().unwrap(),
        this => Self::new(this.get().unsigned_abs() as _, this.get() < 0)
    );
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
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match op {
            "+" => Ok(val),
            "-" => Ok(Value {
                comp_val: if let Some(BasicValueEnum::IntValue(v)) = val.value(ctx) {
                    Some(ctx.builder.build_int_neg(v, "").into())
                } else {
                    None
                },
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int(-v))
                } else {
                    None
                },
                ..val
            }),
            "~" => Ok(Value {
                comp_val: if let Some(BasicValueEnum::IntValue(v)) = val.value(ctx) {
                    Some(ctx.builder.build_not(v, "").into())
                } else {
                    None
                },
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int(!v))
                } else {
                    None
                },
                ..val
            }),
            "!" => Ok(Value::new(
                if let Some(BasicValueEnum::IntValue(v)) = val.value(ctx) {
                    Some(
                        ctx.builder
                            .build_int_compare(EQ, v, v.get_type().const_zero(), "")
                            .into(),
                    )
                } else {
                    None
                },
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int((v == 0) as _))
                } else {
                    None
                },
                types::Int::bool(),
            )),
            _ => Err(invalid_preop(&val, op, oloc)),
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        if let Some(ty) = other.downcast::<types::Int>() {
            ty.bits() >= self.bits()
        } else {
            other.is::<types::Float>()
        }
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        other.is::<types::Int>()
            || other.is_and::<types::Pointer>(|p| {
                matches!(p.llvm_type(ctx), Some(BasicTypeEnum::PointerType(_)))
            })
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(ty) = target.0.downcast::<types::Int>() {
            if ty.bits() >= self.bits() {
                Ok(Value::new(
                    if ty.bits() == self.bits() {
                        val.comp_val
                    } else if let Some(BasicValueEnum::IntValue(v)) = val.comp_val {
                        let ty = target.0.llvm_type(ctx).unwrap().into_int_type();
                        if self.is_signed() {
                            Some(ctx.builder.build_int_s_extend(v, ty, "").into())
                        } else {
                            Some(ctx.builder.build_int_z_extend(v, ty, "").into())
                        }
                    } else {
                        None
                    },
                    if let Some(InterData::Int(v)) = val.inter_val {
                        Some(InterData::Float(v as _))
                    } else {
                        None
                    },
                    target.0,
                ))
            } else {
                Err(CobaltError::NarrowingIntConversion {
                    sbits: self.bits(),
                    dbits: ty.bits(),
                    sloc: val.loc,
                    dloc: target.1.unwrap_or(val.loc),
                })
            }
        } else if target.0.is::<types::Float>() {
            Ok(Value::new(
                if let Some(BasicValueEnum::IntValue(v)) = val.comp_val {
                    let ty = target.0.llvm_type(ctx).unwrap().into_float_type();
                    if self.is_signed() {
                        Some(ctx.builder.build_signed_int_to_float(v, ty, "").into())
                    } else {
                        Some(ctx.builder.build_unsigned_int_to_float(v, ty, "").into())
                    }
                } else {
                    None
                },
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Float(v as _))
                } else {
                    None
                },
                target.0,
            ))
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
        if let Some(ty) = target.0.downcast::<types::Int>() {
            Ok(Value::new(
                if let Some(BasicValueEnum::IntValue(v)) = val.comp_val {
                    let it = target.0.llvm_type(ctx).unwrap().into_int_type();
                    use std::cmp::Ordering;
                    match ty.bits().cmp(&self.bits()) {
                        Ordering::Greater => {
                            if self.is_signed() {
                                Some(ctx.builder.build_int_s_extend(v, it, "").into())
                            } else {
                                Some(ctx.builder.build_int_z_extend(v, it, "").into())
                            }
                        }
                        Ordering::Less => Some(ctx.builder.build_int_truncate(v, it, "").into()),
                        Ordering::Equal => Some(v.into()),
                    }
                } else {
                    None
                },
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Float(v as _))
                } else {
                    None
                },
                target.0,
            ))
        } else if target.0.is::<types::Pointer>() {
            Ok(Value::new(
                if let Some(BasicValueEnum::IntValue(v)) = val.comp_val {
                    let ty = target.0.llvm_type(ctx).unwrap().into_pointer_type();
                    Some(ctx.builder.build_int_to_ptr(v, ty, "").into())
                } else {
                    None
                },
                None,
                target.0,
            ))
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _has_bin_lhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(other.decay().kind(), types::Int::KIND)
            && [
                "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "<", ">", "<=", ">=", "==",
                "!=",
            ]
            .contains(&op)
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        other.is::<types::IntLiteral>()
            && [
                "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "<", ">", "<=", ">=", "==",
                "!=",
            ]
            .contains(&op)
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        mut rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        use std::cmp::Ordering;
        let mut last = None;
        loop {
            if last == Some(rhs.data_type) {
                return Err(invalid_binop(&lhs, &rhs, op.0, op.1));
            }
            match rhs.data_type.kind() {
                types::Reference::KIND | types::Mut::KIND => {
                    last = Some(rhs.data_type);
                    rhs = rhs.decay(ctx)
                }
                types::Int::KIND => {
                    return {
                        let ty = rhs.data_type.downcast::<types::Int>().unwrap();
                        let res = match self.bits().cmp(&ty.bits()) {
                            Ordering::Less => {
                                lhs.comp_val = if let Some(BasicValueEnum::IntValue(v)) =
                                    lhs.comp_val
                                {
                                    Some(
                                        if self.is_signed() {
                                            ctx.builder.build_int_s_extend(
                                                v,
                                                ctx.context.custom_width_int_type(ty.bits() as _),
                                                "",
                                            )
                                        } else {
                                            ctx.builder.build_int_z_extend(
                                                v,
                                                ctx.context.custom_width_int_type(ty.bits() as _),
                                                "",
                                            )
                                        }
                                        .into(),
                                    )
                                } else {
                                    None
                                };
                                ty
                            }
                            Ordering::Greater => {
                                rhs.comp_val = if let Some(BasicValueEnum::IntValue(v)) =
                                    rhs.comp_val
                                {
                                    Some(
                                        ctx.builder
                                            .build_int_z_extend(
                                                v,
                                                ctx.context.custom_width_int_type(self.bits() as _),
                                                "",
                                            )
                                            .into(),
                                    )
                                } else {
                                    None
                                };
                                self
                            }
                            Ordering::Equal => {
                                Self::new(self.bits(), self.is_unsigned() || ty.is_unsigned())
                            }
                        };
                        match op.0 {
                            "+" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_int_add(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l + r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "-" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_int_sub(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l - r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "*" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_int_mul(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l + r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "/" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        if self.is_signed() || ty.is_signed() {
                                            ctx.builder.build_int_signed_div(l, r, "")
                                        } else {
                                            ctx.builder.build_int_unsigned_div(l, r, "")
                                        }
                                        .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l + r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "%" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        if self.is_signed() || ty.is_signed() {
                                            ctx.builder.build_int_signed_div(l, r, "")
                                        } else {
                                            ctx.builder.build_int_unsigned_div(l, r, "")
                                        }
                                        .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l + r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "&" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_and(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l & r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "|" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_or(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l | r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "^" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_xor(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l ^ r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "<<" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_left_shift(l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l << r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            ">>" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        ctx.builder
                                            .build_right_shift(l, r, res.is_signed(), "")
                                            .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(l >> r))
                                } else {
                                    None
                                },
                                res,
                            )),
                            "<" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        ctx.builder
                                            .build_int_compare(
                                                if res.is_signed() { SLT } else { ULT },
                                                l,
                                                r,
                                                "",
                                            )
                                            .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l < r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        ctx.builder
                                            .build_int_compare(
                                                if res.is_signed() { SGT } else { UGT },
                                                l,
                                                r,
                                                "",
                                            )
                                            .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l > r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "<=" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        ctx.builder
                                            .build_int_compare(
                                                if res.is_signed() { SLE } else { ULE },
                                                l,
                                                r,
                                                "",
                                            )
                                            .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l <= r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">=" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(
                                        ctx.builder
                                            .build_int_compare(
                                                if res.is_signed() { SGE } else { UGE },
                                                l,
                                                r,
                                                "",
                                            )
                                            .into(),
                                    )
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l >= r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "==" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_int_compare(EQ, l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l == r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "!=" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::IntValue(l)),
                                    Some(BasicValueEnum::IntValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    Some(ctx.builder.build_int_compare(NE, l, r, "").into())
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l != r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                        }
                    }
                }
                types::IntLiteral::KIND => {
                    return match op.0 {
                        "+" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_add(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l + r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "-" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_sub(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l - r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "*" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_mul(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l + r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "/" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                let r = ctx
                                    .context
                                    .custom_width_int_type(self.bits() as _)
                                    .const_int(*r as _, self.is_signed());
                                Some(
                                    if self.is_signed() {
                                        ctx.builder.build_int_signed_div(l, r, "")
                                    } else {
                                        ctx.builder.build_int_unsigned_div(l, r, "")
                                    }
                                    .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l + r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "%" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                let r = ctx
                                    .context
                                    .custom_width_int_type(self.bits() as _)
                                    .const_int(*r as _, self.is_signed());
                                Some(
                                    if self.is_signed() {
                                        ctx.builder.build_int_signed_rem(l, r, "")
                                    } else {
                                        ctx.builder.build_int_unsigned_rem(l, r, "")
                                    }
                                    .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l + r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "&" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_and(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l & r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "|" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_or(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l | r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "^" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_xor(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l ^ r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "<<" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_left_shift(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l << r))
                            } else {
                                None
                            },
                            self,
                        )),
                        ">>" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_right_shift(
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            self.is_signed(),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int(l >> r))
                            } else {
                                None
                            },
                            self,
                        )),
                        "<" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            if self.is_signed() { SLT } else { ULT },
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l < r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        ">" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            if self.is_signed() { SGT } else { UGT },
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l > r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        "<=" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            if self.is_signed() { SLE } else { ULE },
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l <= r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        ">=" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            if self.is_signed() { SGE } else { UGE },
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l >= r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        "==" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            EQ,
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l == r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        "!=" => Ok(Value::new(
                            if let (Some(BasicValueEnum::IntValue(l)), Some(InterData::Int(r))) =
                                (lhs.value(ctx), &rhs.inter_val)
                            {
                                Some(
                                    ctx.builder
                                        .build_int_compare(
                                            NE,
                                            l,
                                            ctx.context
                                                .custom_width_int_type(self.bits() as _)
                                                .const_int(*r as _, self.is_signed()),
                                            "",
                                        )
                                        .into(),
                                )
                            } else {
                                None
                            },
                            if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                                (lhs.inter_val, rhs.inter_val)
                            {
                                Some(InterData::Int((l != r) as _))
                            } else {
                                None
                            },
                            types::Int::bool(),
                        )),
                        _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                    }
                }
                _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
            }
        }
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
        if lhs.data_type.is::<types::IntLiteral>() {
            match op.0 {
                "+" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_add(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l + r))
                    } else {
                        None
                    },
                    self,
                )),
                "-" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_sub(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l - r))
                    } else {
                        None
                    },
                    self,
                )),
                "*" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_mul(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l + r))
                    } else {
                        None
                    },
                    self,
                )),
                "/" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        let l = ctx
                            .context
                            .custom_width_int_type(self.bits() as _)
                            .const_int(*l as _, self.is_signed());
                        Some(
                            if self.is_signed() {
                                ctx.builder.build_int_signed_div(l, r, "")
                            } else {
                                ctx.builder.build_int_unsigned_div(l, r, "")
                            }
                            .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l + r))
                    } else {
                        None
                    },
                    self,
                )),
                "%" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        let l = ctx
                            .context
                            .custom_width_int_type(self.bits() as _)
                            .const_int(*l as _, self.is_signed());
                        Some(
                            if self.is_signed() {
                                ctx.builder.build_int_signed_rem(l, r, "")
                            } else {
                                ctx.builder.build_int_unsigned_rem(l, r, "")
                            }
                            .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l + r))
                    } else {
                        None
                    },
                    self,
                )),
                "&" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_and(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l & r))
                    } else {
                        None
                    },
                    self,
                )),
                "|" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_or(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l | r))
                    } else {
                        None
                    },
                    self,
                )),
                "^" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_xor(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l ^ r))
                    } else {
                        None
                    },
                    self,
                )),
                "<<" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_left_shift(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l << r))
                    } else {
                        None
                    },
                    self,
                )),
                ">>" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_right_shift(
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    self.is_signed(),
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int(l >> r))
                    } else {
                        None
                    },
                    self,
                )),
                "<" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    if self.is_signed() { SLT } else { ULT },
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l < r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                ">" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    if self.is_signed() { SGT } else { UGT },
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l > r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                "<=" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    if self.is_signed() { SLE } else { ULE },
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l <= r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                ">=" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    if self.is_signed() { SGE } else { UGE },
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l >= r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                "==" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    EQ,
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l == r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                "!=" => Ok(Value::new(
                    if let (Some(InterData::Int(l)), Some(BasicValueEnum::IntValue(r))) =
                        (&lhs.inter_val, rhs.value(ctx))
                    {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    NE,
                                    ctx.context
                                        .custom_width_int_type(self.bits() as _)
                                        .const_int(*l as _, self.is_signed()),
                                    r,
                                    "",
                                )
                                .into(),
                        )
                    } else {
                        None
                    },
                    if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                        (lhs.inter_val, rhs.inter_val)
                    {
                        Some(InterData::Int((l != r) as _))
                    } else {
                        None
                    },
                    types::Int::bool(),
                )),
                _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
            }
        } else {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
        }
    }
    fn _has_mut_pre_op(&'static self, op: &'static str, ctx: &CompCtx) -> bool {
        op == "++" || op == "--"
    }
    fn _mut_pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match op {
            "++" => Ok(Value::new(
                if let Some(BasicValueEnum::PointerValue(pv)) = val.comp_val {
                    let it = self.llvm_type(ctx).unwrap().into_int_type();
                    let v1 = ctx.builder.build_load(it, pv, "").into_int_value();
                    let v2 = ctx.builder.build_int_add(v1, it.const_int(1, false), "");
                    ctx.builder.build_store(pv, v2);
                    val.comp_val
                } else {
                    None
                },
                None,
                self.add_ref(true),
            )),
            "--" => Ok(Value::new(
                if let Some(BasicValueEnum::PointerValue(pv)) = val.comp_val {
                    let it = self.llvm_type(ctx).unwrap().into_int_type();
                    let v1 = ctx.builder.build_load(it, pv, "").into_int_value();
                    let v2 = ctx.builder.build_int_sub(v1, it.const_int(1, false), "");
                    ctx.builder.build_store(pv, v2);
                    val.comp_val
                } else {
                    None
                },
                None,
                self.add_ref(true),
            )),
            _ => Err(invalid_preop(&val, op, oloc)),
        }
    }
    fn _has_mut_bin_lhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(other.kind(), types::Int::KIND | types::IntLiteral::KIND)
            && ["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="].contains(&op)
    }
    fn _mut_bin_lhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if rhs.data_type.is::<types::IntLiteral>() {
            if !ctx.is_const.get() {
                if let (Some(BasicValueEnum::PointerValue(lv)), Some(InterData::Int(v))) =
                    (lhs.value(ctx), &rhs.inter_val)
                {
                    let lty = ctx.context.custom_width_int_type(self.bits() as _);
                    let rv = lty.const_int(*v as _, self.is_signed());
                    match op.0 {
                        "+=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "-=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "*=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_mul(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "/=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = if self.is_signed() {
                                ctx.builder.build_int_signed_div(v1, rv, "")
                            } else {
                                ctx.builder.build_int_unsigned_div(v1, rv, "")
                            };
                            ctx.builder.build_store(lv, v2);
                        }
                        "%=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = if self.is_signed() {
                                ctx.builder.build_int_signed_rem(v1, rv, "")
                            } else {
                                ctx.builder.build_int_unsigned_rem(v1, rv, "")
                            };
                            ctx.builder.build_store(lv, v2);
                        }
                        "&=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_and(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "|=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_or(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "^=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_xor(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "<<=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_left_shift(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        ">>=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_right_shift(v1, rv, self.is_signed(), "");
                            ctx.builder.build_store(lv, v2);
                        }
                        _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                    }
                } else {
                    self._has_mut_bin_lhs(rhs.data_type, op.0, ctx, move_left, move_right)
                        .then_some(())
                        .ok_or_else(|| invalid_binop(&lhs, &rhs, op.0, op.1))?
                }
            } else {
                self._has_mut_bin_lhs(rhs.data_type, op.0, ctx, move_left, move_right)
                    .then_some(())
                    .ok_or_else(|| invalid_binop(&lhs, &rhs, op.0, op.1))?
            }
            lhs.data_type = self.add_ref(true);
            Ok(lhs)
        } else if let Some(rt) = rhs.data_type.downcast::<types::Int>() {
            if !ctx.is_const.get() {
                if let (
                    Some(BasicValueEnum::PointerValue(lv)),
                    Some(BasicValueEnum::IntValue(mut rv)),
                ) = (lhs.value(ctx), rhs.value(ctx))
                {
                    use std::cmp::Ordering;
                    let lty = ctx.context.custom_width_int_type(self.bits() as _);
                    match self.bits().cmp(&rt.bits()) {
                        Ordering::Less => {
                            return Err(CobaltError::NarrowingIntConversion {
                                sbits: self.bits(),
                                dbits: rt.bits(),
                                sloc: lhs.loc,
                                dloc: rhs.loc,
                            })
                        }
                        Ordering::Greater => {
                            rv = if rt.is_signed() {
                                ctx.builder.build_int_s_extend(rv, lty, "")
                            } else {
                                ctx.builder.build_int_z_extend(rv, lty, "")
                            }
                        }
                        Ordering::Equal => {}
                    }
                    match op.0 {
                        "+=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "-=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "*=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_int_mul(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "/=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = if self.is_signed() {
                                ctx.builder.build_int_signed_div(v1, rv, "")
                            } else {
                                ctx.builder.build_int_unsigned_div(v1, rv, "")
                            };
                            ctx.builder.build_store(lv, v2);
                        }
                        "%=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = if self.is_signed() {
                                ctx.builder.build_int_signed_rem(v1, rv, "")
                            } else {
                                ctx.builder.build_int_unsigned_rem(v1, rv, "")
                            };
                            ctx.builder.build_store(lv, v2);
                        }
                        "&=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_and(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "|=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_or(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "^=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_xor(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        "<<=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_left_shift(v1, rv, "");
                            ctx.builder.build_store(lv, v2);
                        }
                        ">>=" => {
                            let v1 = ctx.builder.build_load(lty, lv, "").into_int_value();
                            let v2 = ctx.builder.build_right_shift(v1, rv, self.is_signed(), "");
                            ctx.builder.build_store(lv, v2);
                        }
                        _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                    }
                } else {
                    self._has_mut_bin_lhs(rhs.data_type, op.0, ctx, move_left, move_right)
                        .then_some(())
                        .ok_or_else(|| invalid_binop(&lhs, &rhs, op.0, op.1))?
                }
            } else {
                self._has_mut_bin_lhs(rhs.data_type, op.0, ctx, move_left, move_right)
                    .then_some(())
                    .ok_or_else(|| invalid_binop(&lhs, &rhs, op.0, op.1))?
            }
            lhs.data_type = self.add_ref(true);
            Ok(lhs)
        } else {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
        }
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
}
#[derive(Debug, ConstIdentify, Display)]
#[display(fmt = "<int literal>")]
pub struct IntLiteral(());
impl IntLiteral {
    pub fn new() -> &'static Self {
        static SELF: IntLiteral = Self(());
        &SELF
    }
}
impl_null_type_with_new!(IntLiteral);
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
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match op {
            "+" => Ok(val),
            "-" => Ok(Value {
                comp_val: if let Some(BasicValueEnum::IntValue(v)) = val.comp_val {
                    Some(ctx.builder.build_int_neg(v, "").into())
                } else {
                    None
                },
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int(-v))
                } else {
                    None
                },
                ..val
            }),
            "~" => Ok(Value {
                comp_val: if let Some(BasicValueEnum::IntValue(v)) = val.value(ctx) {
                    Some(ctx.builder.build_not(v, "").into())
                } else {
                    None
                },
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int(!v))
                } else {
                    None
                },
                ..val
            }),
            "!" => Ok(Value::new(
                if let Some(BasicValueEnum::IntValue(v)) = val.value(ctx) {
                    Some(
                        ctx.builder
                            .build_int_compare(EQ, v, v.get_type().const_zero(), "")
                            .into(),
                    )
                } else {
                    None
                },
                if let Some(InterData::Int(v)) = val.inter_val {
                    Some(InterData::Int((v == 0) as _))
                } else {
                    None
                },
                types::Int::bool(),
            )),
            _ => Err(invalid_preop(&val, op, oloc)),
        }
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
    fn _has_bin_lhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        other.is::<types::IntLiteral>()
            && [
                "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "<", ">", "<=", ">=", "==",
                "!=",
            ]
            .contains(&op)
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
        if !rhs.data_type.is::<types::IntLiteral>() {
            return Err(invalid_binop(&lhs, &rhs, op.0, op.1));
        }
        match op.0 {
            "+" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l + r))
                } else {
                    None
                },
                self,
            )),
            "-" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l - r))
                } else {
                    None
                },
                self,
            )),
            "*" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l * r))
                } else {
                    None
                },
                self,
            )),
            "/" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l / r))
                } else {
                    None
                },
                self,
            )),
            "%" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l % r))
                } else {
                    None
                },
                self,
            )),
            "&" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l & r))
                } else {
                    None
                },
                self,
            )),
            "|" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l | r))
                } else {
                    None
                },
                self,
            )),
            "^" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l ^ r))
                } else {
                    None
                },
                self,
            )),
            "<<" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l << r))
                } else {
                    None
                },
                self,
            )),
            ">>" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int(l >> r))
                } else {
                    None
                },
                self,
            )),
            "<" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l < r) as _))
                } else {
                    None
                },
                self,
            )),
            ">" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l > r) as _))
                } else {
                    None
                },
                self,
            )),
            "<=" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l <= r) as _))
                } else {
                    None
                },
                self,
            )),
            ">=" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l >= r) as _))
                } else {
                    None
                },
                self,
            )),
            "==" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l == r) as _))
                } else {
                    None
                },
                self,
            )),
            "!=" => Ok(Value::new(
                None,
                if let (Some(InterData::Int(l)), Some(InterData::Int(r))) =
                    (lhs.inter_val, rhs.inter_val)
                {
                    Some(InterData::Int((l != r) as _))
                } else {
                    None
                },
                self,
            )),
            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
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
}
submit_types!(Int, IntLiteral);
