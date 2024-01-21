use super::*;
use inkwell::FloatPredicate::*;
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display, Serialize, Deserialize,
)]
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
#[derive(Debug, ConstIdentify, Display)]
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
    pub fn kind(&self) -> FPType {
        self.0
    }
}
impl TypeSerde for Float {
    no_type_header!();
    impl_type_proxy!(FPType, Self::kind, Self::new);
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
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        if let Some(ty) = other.downcast::<types::Float>() {
            ty.kind() >= self.kind()
        } else {
            false
        }
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(ty) = target.0.downcast::<types::Float>() {
            if ty.kind() >= self.kind() {
                return Ok(Value::new(
                    if let Some(BasicValueEnum::FloatValue(v)) = val.comp_val {
                        if self.kind() == ty.kind() {
                            Some(v.into())
                        } else {
                            ctx.builder
                                .build_float_ext(
                                    v,
                                    ty.llvm_type(ctx).unwrap().into_float_type(),
                                    "",
                                )
                                .ok()
                                .map(From::from)
                        }
                    } else {
                        None
                    },
                    val.inter_val,
                    target.0,
                ));
            }
        }
        Err(cant_econv(&val, target.0, target.1))
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(ty) = target.0.downcast::<types::Float>() {
            Ok(Value::new(
                if let Some(BasicValueEnum::FloatValue(v)) = val.comp_val {
                    let ft = ty.llvm_type(ctx).unwrap().into_float_type();
                    use std::cmp::Ordering;
                    match ty.kind().cmp(&self.kind()) {
                        Ordering::Greater => {
                            ctx.builder.build_float_ext(v, ft, "").ok().map(From::from)
                        }
                        Ordering::Less => ctx
                            .builder
                            .build_float_trunc(v, ft, "")
                            .ok()
                            .map(From::from),
                        Ordering::Equal => Some(v.into()),
                    }
                } else {
                    None
                },
                val.inter_val,
                target.0,
            ))
        } else if let Some(ty) = target.0.downcast::<types::Int>() {
            Ok(Value::new(
                if let Some(BasicValueEnum::FloatValue(v)) = val.comp_val {
                    let it = ty.llvm_type(ctx).unwrap().into_int_type();
                    if ty.is_signed() {
                        ctx.builder
                            .build_float_to_signed_int(v, it, "")
                            .ok()
                            .map(From::from)
                    } else {
                        ctx.builder
                            .build_float_to_unsigned_int(v, it, "")
                            .ok()
                            .map(From::from)
                    }
                } else {
                    None
                },
                if let Some(InterData::Float(v)) = val.inter_val {
                    Some(InterData::Int(v as _))
                } else {
                    None
                },
                target.0,
            ))
        } else {
            Err(cant_econv(&val, target.0, target.1))
        }
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
                    ctx.builder.build_float_neg(v, "").ok().map(From::from)
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
        matches!(other.decay().kind(), types::Int::KIND | types::Float::KIND)
            && ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="].contains(&op)
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(other.decay().kind(), types::Int::KIND)
            && ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="].contains(&op)
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
        let mut last = None;
        loop {
            if last == Some(rhs.data_type) {
                return Err(invalid_binop(&lhs, &rhs, op.0, op.1));
            }
            match rhs.data_type.kind() {
                types::Reference::KIND | types::Mut::KIND => {
                    last = Some(rhs.data_type);
                    rhs = rhs.decay(ctx);
                }
                types::Int::KIND => {
                    return {
                        let v = if let Some(BasicValueEnum::IntValue(v)) = rhs.value(ctx) {
                            let ft = self.llvm_type(ctx).unwrap().into_float_type();
                            Some(
                                if rhs.data_type.downcast::<types::Int>().unwrap().is_signed() {
                                    ctx.builder.build_signed_int_to_float(v, ft, "")
                                } else {
                                    ctx.builder.build_unsigned_int_to_float(v, ft, "")
                                }
                                .unwrap(),
                            )
                        } else {
                            None
                        };
                        match op.0 {
                            "+" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_add(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_sub(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_mul(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_div(l, r, "").ok().map(From::from)
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
                            "<" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OLT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l < r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OGT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l > r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "<=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OLE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l <= r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OGE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l >= r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "==" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OEQ, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l == r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "!=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(ONE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l != r as f64) as _))
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
                    return {
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_add(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_sub(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_mul(l, r, "").ok().map(From::from)
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
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder.build_float_div(l, r, "").ok().map(From::from)
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
                            "<" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OLT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l < r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OGT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l > r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "<=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OLE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l <= r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OGE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l >= r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "==" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(OEQ, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l == r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "!=" => Ok(Value::new(
                                if let (Some(BasicValueEnum::FloatValue(l)), Some(r)) =
                                    (lhs.value(ctx), v)
                                {
                                    ctx.builder
                                        .build_float_compare(ONE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Int(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int((l != r as f64) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                        }
                    }
                }
                types::Float::KIND => {
                    return {
                        use std::cmp::Ordering;
                        let ty = rhs.data_type.downcast::<types::Float>().unwrap();
                        let res = types::Float::new(std::cmp::max(self.kind(), ty.kind()));
                        match self.kind().cmp(&ty.kind()) {
                            Ordering::Less => {
                                rhs.comp_val =
                                    if let Some(BasicValueEnum::FloatValue(v)) = rhs.comp_val {
                                        ctx.builder
                                            .build_float_cast(
                                                v,
                                                self.llvm_type(ctx).unwrap().into_float_type(),
                                                "",
                                            )
                                            .ok()
                                            .map(From::from)
                                    } else {
                                        None
                                    };
                            }
                            Ordering::Greater => {
                                lhs.comp_val =
                                    if let Some(BasicValueEnum::FloatValue(v)) = lhs.comp_val {
                                        ctx.builder
                                            .build_float_cast(
                                                v,
                                                ty.llvm_type(ctx).unwrap().into_float_type(),
                                                "",
                                            )
                                            .ok()
                                            .map(From::from)
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
                                    ctx.builder.build_float_add(l, r, "").ok().map(From::from)
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
                                    ctx.builder.build_float_sub(l, r, "").ok().map(From::from)
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
                                    ctx.builder.build_float_mul(l, r, "").ok().map(From::from)
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
                                    ctx.builder.build_float_div(l, r, "").ok().map(From::from)
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
                            "<" => Ok(Value::new(
                                if let (
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OLT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OGT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OLE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OGE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OEQ, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                                    Some(BasicValueEnum::FloatValue(l)),
                                    Some(BasicValueEnum::FloatValue(r)),
                                ) = (lhs.value(ctx), rhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(ONE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Float(l)), Some(InterData::Float(r))) =
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
                _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
            }
        }
    }
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let mut last = None;
        loop {
            if last == Some(rhs.data_type) {
                return Err(invalid_binop(&lhs, &rhs, op.0, op.1));
            }
            match lhs.data_type.kind() {
                types::Reference::KIND | types::Mut::KIND => {
                    last = Some(lhs.data_type);
                    lhs = lhs.decay(ctx);
                }
                types::Int::KIND => {
                    return {
                        let v = if let Some(BasicValueEnum::IntValue(v)) = lhs.value(ctx) {
                            let ft = self.llvm_type(ctx).unwrap().into_float_type();
                            if rhs.data_type.downcast::<types::Int>().unwrap().is_signed() {
                                ctx.builder.build_signed_int_to_float(v, ft, "")
                            } else {
                                ctx.builder.build_unsigned_int_to_float(v, ft, "")
                            }
                            .ok()
                        } else {
                            None
                        };
                        match op.0 {
                            "+" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_add(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_sub(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_mul(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_div(l, r, "").ok().map(From::from)
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
                            "<" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OLT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) < r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OGT, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) > r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "<=" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OLE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) <= r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            ">=" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OGE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) >= r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "==" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(OEQ, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) == r) as _))
                                } else {
                                    None
                                },
                                types::Int::bool(),
                            )),
                            "!=" => Ok(Value::new(
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder
                                        .build_float_compare(ONE, l, r, "")
                                        .ok()
                                        .map(From::from)
                                } else {
                                    None
                                },
                                if let (Some(InterData::Int(l)), Some(InterData::Float(r))) =
                                    (lhs.inter_val, rhs.inter_val)
                                {
                                    Some(InterData::Int(((l as f64) != r) as _))
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
                    return {
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_add(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_sub(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_mul(l, r, "").ok().map(From::from)
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
                                if let (Some(l), Some(BasicValueEnum::FloatValue(r))) =
                                    (v, lhs.value(ctx))
                                {
                                    ctx.builder.build_float_div(l, r, "").ok().map(From::from)
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
                }
                _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
            }
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
                    let v1 = ctx
                        .builder
                        .build_load(ft, pv, "")
                        .unwrap()
                        .into_float_value();
                    let v2 = ctx
                        .builder
                        .build_float_add(v1, ft.const_float(1.0), "")
                        .unwrap();
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
                    let v1 = ctx
                        .builder
                        .build_load(ft, pv, "")
                        .unwrap()
                        .into_float_value();
                    let v2 = ctx
                        .builder
                        .build_float_sub(v1, ft.const_float(1.0), "")
                        .unwrap();
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
        mut lhs: Value<'src, 'ctx>,
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
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "").unwrap();
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
                        }
                        .unwrap();
                        match op.0 {
                            "+=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "").unwrap();
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
            }
            types::Float::KIND => {
                if !ctx.is_const.get() {
                    let lty = self.llvm_type(ctx).unwrap().into_float_type();
                    if let (
                        Some(BasicValueEnum::PointerValue(lv)),
                        Some(BasicValueEnum::FloatValue(rv)),
                    ) = (lhs.value(ctx), rhs.value(ctx))
                    {
                        let rv = ctx.builder.build_float_cast(rv, lty, "").unwrap();
                        match op.0 {
                            "+=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_add(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "-=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_sub(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "*=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_mul(v1, rv, "").unwrap();
                                ctx.builder.build_store(lv, v2);
                            }
                            "/=" => {
                                let v1 = ctx
                                    .builder
                                    .build_load(lty, lv, "")
                                    .unwrap()
                                    .into_float_value();
                                let v2 = ctx.builder.build_float_div(v1, rv, "").unwrap();
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
            }
            _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
        }
    }
}
submit_types!(Float);
