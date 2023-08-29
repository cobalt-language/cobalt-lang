use super::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
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
impl ConcreteType for Float {
    const KIND: NonZeroU64 = make_id(b"float");
}
impl Type for Float {
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
                comp_val: if let Some(BasicValueEnum::FloatValue(v)) = val.value(ctx) {
                    Some(ctx.builder.build_float_neg(v, "").into())
                } else {
                    None
                },
                inter_val: if let Some(InterData::Float(v)) = val.inter_val {
                    Some(InterData::Float(-v))
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
        matches!(
            other.kind(),
            types::Int::KIND | types::IntLiteral::KIND | types::Float::KIND
        ) && ["+", "-", "*", "/"].contains(&op)
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(other.kind(), types::Int::KIND | types::IntLiteral::KIND)
            && ["+", "-", "*", "/"].contains(&op)
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
        match rhs.data_type.kind() {
            types::Int::KIND => {
                let v = if let Some(BasicValueEnum::IntValue(v)) = rhs.value(ctx) {
                    let ft = self.llvm_type(ctx).unwrap().into_float_type();
                    Some(
                        if rhs.data_type.downcast::<types::Int>().unwrap().is_signed() {
                            ctx.builder.build_signed_int_to_float(v, ft, "")
                        } else {
                            ctx.builder.build_unsigned_int_to_float(v, ft, "")
                        },
                    )
                } else {
                    None
                };
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_add(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l + r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "-" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_sub(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l - r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "*" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_mul(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l * r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "/" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_div(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l / r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            types::IntLiteral::KIND => {
                let v = if let Some(InterData::Int(v)) = &rhs.inter_val {
                    Some(
                        self.llvm_type(ctx)
                            .unwrap()
                            .into_float_type()
                            .const_float(*v as _),
                    )
                } else {
                    None
                };
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_add(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l + r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "-" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_sub(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l - r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "*" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_mul(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l * r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    "/" => Ok(Value::new(
                        if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) = (lhs.value(ctx), v)
                        {
                            Some(ctx.builder.build_float_div(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l / r as f64))
                        } else {
                            None
                        },
                        self,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            types::Float::KIND => {
                use std::cmp::Ordering;
                let ty = rhs.data_type.downcast::<types::Float>().unwrap();
                let res = types::Float::new(std::cmp::max(self.kind(), ty.kind()));
                match self.kind().cmp(&ty.kind()) {
                    Ordering::Less => {
                        rhs.comp_val = if let Some(BasicValueEnum::FloatValue(v)) = rhs.comp_val {
                            Some(
                                ctx.builder
                                    .build_float_cast(
                                        v,
                                        self.llvm_type(ctx).unwrap().into_float_type(),
                                        "",
                                    )
                                    .into(),
                            )
                        } else {
                            None
                        };
                    }
                    Ordering::Greater => {
                        lhs.comp_val = if let Some(BasicValueEnum::FloatValue(v)) = lhs.comp_val {
                            Some(
                                ctx.builder
                                    .build_float_cast(
                                        v,
                                        ty.llvm_type(ctx).unwrap().into_float_type(),
                                        "",
                                    )
                                    .into(),
                            )
                        } else {
                            None
                        };
                    }
                    Ordering::Equal => {}
                };
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (
                            Some(BasicValueEnum::FloatValue(l)),
                            Some(BasicValueEnum::FloatValue(r)),
                        ) = (lhs.value(ctx), rhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_add(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l + r))
                        } else {
                            None
                        },
                        res,
                    )),
                    "-" => Ok(Value::new(
                        if let (
                            Some(BasicValueEnum::FloatValue(l)),
                            Some(BasicValueEnum::FloatValue(r)),
                        ) = (lhs.value(ctx), rhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_sub(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l - r))
                        } else {
                            None
                        },
                        res,
                    )),
                    "*" => Ok(Value::new(
                        if let (
                            Some(BasicValueEnum::FloatValue(l)),
                            Some(BasicValueEnum::FloatValue(r)),
                        ) = (lhs.value(ctx), rhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_mul(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l * r))
                        } else {
                            None
                        },
                        res,
                    )),
                    "/" => Ok(Value::new(
                        if let (
                            Some(BasicValueEnum::FloatValue(l)),
                            Some(BasicValueEnum::FloatValue(r)),
                        ) = (lhs.value(ctx), rhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_div(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l / r))
                        } else {
                            None
                        },
                        res,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
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
        match lhs.data_type.kind() {
            types::Int::KIND => {
                let v = if let Some(BasicValueEnum::IntValue(v)) = lhs.value(ctx) {
                    let ft = self.llvm_type(ctx).unwrap().into_float_type();
                    Some(
                        if rhs.data_type.downcast::<types::Int>().unwrap().is_signed() {
                            ctx.builder.build_signed_int_to_float(v, ft, "")
                        } else {
                            ctx.builder.build_unsigned_int_to_float(v, ft, "")
                        },
                    )
                } else {
                    None
                };
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_add(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 + r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "-" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_sub(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 - r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "*" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_mul(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 * r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "/" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_div(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 / r))
                        } else {
                            None
                        },
                        self,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            types::IntLiteral::KIND => {
                let v = if let Some(InterData::Int(v)) = &lhs.inter_val {
                    Some(
                        self.llvm_type(ctx)
                            .unwrap()
                            .into_float_type()
                            .const_float(*v as _),
                    )
                } else {
                    None
                };
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_add(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 + r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "-" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_sub(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 - r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "*" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_mul(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 * r))
                        } else {
                            None
                        },
                        self,
                    )),
                    "/" => Ok(Value::new(
                        if let (Some(l), Some(BasicValueEnum::FloatValue(r))) = (v, lhs.value(ctx))
                        {
                            Some(ctx.builder.build_float_div(l, r, "").into())
                        } else {
                            None
                        },
                        if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                            (lhs.inter_val, rhs.inter_val)
                        {
                            Some(InterData::Float(l as f64 / r))
                        } else {
                            None
                        },
                        self,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
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
                    let ft = self.llvm_type(ctx).unwrap().into_float_type();
                    let v1 = ctx.builder.build_load(ft, pv, "").into_float_value();
                    let v2 = ctx.builder.build_float_add(v1, ft.const_float(1.0), "");
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
                    let ft = self.llvm_type(ctx).unwrap().into_float_type();
                    let v1 = ctx.builder.build_load(ft, pv, "").into_float_value();
                    let v2 = ctx.builder.build_float_sub(v1, ft.const_float(1.0), "");
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
    fn _has_mut_bin_lhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(
            other.kind(),
            types::Float::KIND | types::Int::KIND | types::IntLiteral::KIND
        ) && ["+=", "-=", "*=", "/="].contains(&op)
    }
    fn _mut_bin_lhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        match rhs.data_type.kind() {
            types::IntLiteral::KIND => {
                if !ctx.is_const.get() {
                    let lty = self.llvm_type(ctx).unwrap().into_float_type();
                    if let (Some(BasicValueEnum::PointerValue(lv)), Some(InterData::Int(rv))) =
                        (lhs.value(ctx), &rhs.inter_val)
                    {
                        let rv = lty.const_float(*rv as _);
                        match op.0 {
                            "+=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                        }
                    }
                }
                types::Reference::new(lhs.data_type);
                Ok(lhs)
            }
            types::Int::KIND => {
                let rty = rhs.data_type.downcast::<types::Int>().unwrap();
                if !ctx.is_const.get() {
                    let lty = self.llvm_type(ctx).unwrap().into_float_type();
                    if let (
                        Some(BasicValueEnum::PointerValue(lv)),
                        Some(BasicValueEnum::IntValue(rv)),
                    ) = (lhs.value(ctx), rhs.value(ctx))
                    {
                        let rv = if rty.is_signed() {
                            ctx.builder.build_signed_int_to_float(rv, lty, "")
                        } else {
                            ctx.builder.build_unsigned_int_to_float(rv, lty, "")
                        };
                        match op.0 {
                            "+=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                        }
                    }
                }
                types::Reference::new(lhs.data_type);
                Ok(lhs)
            }
            types::Float::KIND => {
                if !ctx.is_const.get() {
                    let lty = self.llvm_type(ctx).unwrap().into_float_type();
                    if let (
                        Some(BasicValueEnum::PointerValue(lv)),
                        Some(BasicValueEnum::FloatValue(rv)),
                    ) = (lhs.value(ctx), rhs.value(ctx))
                    {
                        let rv = ctx.builder.build_float_cast(rv, lty, "");
                        match op.0 {
                            "+=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx.builder.build_load(lty, lv, "").into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "");
                                ctx.builder.build_store(lv, v2);
                            }
                            _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                        }
                    }
                }
                types::Reference::new(lhs.data_type);
                Ok(lhs)
            }
            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
        }
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
submit_types!(Float);
