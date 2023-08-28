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
            _ => Err(invalid_preop(&val, op, oloc)),
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
        matches!(other.kind(), types::Int::KIND | types::IntLiteral::KIND)
            && ["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>"].contains(&op)
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
            && ["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>"].contains(&op)
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
        match rhs.data_type.kind() {
            types::Int::KIND => {
                let ty = rhs.data_type.downcast::<types::Int>().unwrap();
                let res = match self.bits().cmp(&ty.bits()) {
                    Ordering::Less => {
                        lhs.comp_val = if let Some(BasicValueEnum::IntValue(v)) = lhs.comp_val {
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
                        rhs.comp_val = if let Some(BasicValueEnum::IntValue(v)) = rhs.comp_val {
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
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            types::IntLiteral::KIND => match op.0 {
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
                _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
            },
            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
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
                self.add_ref(false),
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
                self.add_ref(false),
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
