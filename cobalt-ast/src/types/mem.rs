use super::*;
use inkwell::IntPredicate::*;
use std::cell::Cell;
use std::rc::Rc;
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "&{}", _0)]
#[repr(transparent)]
pub struct Reference(TypeRef);
impl Reference {
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl ConcreteType for Reference {
    const KIND: NonZeroU64 = make_id(b"ref");
}
impl Type for Reference {
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn decay(&self) -> TypeRef {
        self.base().decay()
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !(ctx.is_const.get() || self.base().is::<types::Mut>()) {
            val.comp_val = val.comp_val.and_then(|v| {
                Some(ctx.builder.build_load(
                    self.base().llvm_type(ctx)?,
                    v.is_pointer_value().then(|| v.into_pointer_value())?,
                    "",
                ))
            });
        }
        self.base()
            .pre_op(val, op, oloc, ctx, can_move && self.base().copyable(ctx))
    }
    fn _has_bin_lhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        (op == "=" && self.base().is::<types::Mut>() && other.impl_convertible(self, ctx))
            || self.base()._has_bin_lhs(
                other,
                op,
                ctx,
                move_left && self.base().copyable(ctx),
                move_right,
            )
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        self.base()._has_bin_lhs(
            other,
            op,
            ctx,
            move_left,
            move_right && !self.base().copyable(ctx),
        )
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if op.0 == "="
            && self.base().is::<types::Mut>()
            && rhs.data_type.impl_convertible(self.as_type_ref(), ctx)
        {
            let rhs = rhs.impl_convert((self, Some(lhs.loc)), ctx)?;
            if let (Some(BasicValueEnum::PointerValue(pv)), Some(val)) =
                (lhs.comp_val, rhs.comp_val)
            {
                let inst = ctx.builder.build_store(pv, val);
                cfg::mark_move(&rhs, cfg::Location::Inst(inst, 0), ctx, rhs.loc);
            }
            return Ok(lhs);
        }
        if !(self.base().is::<types::Mut>() || ctx.is_const.get()) {
            lhs.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                ctx.is_const.get(),
                self.base().llvm_type(ctx),
                lhs.value(ctx),
            ) {
                lhs.address = Rc::new(Cell::new(Some(pv)));
                Some(ctx.builder.build_load(llt, pv, ""))
            } else {
                None
            };
        }
        self.base()._bin_lhs(
            lhs,
            rhs,
            op,
            ctx,
            move_left && self.base().copyable(ctx),
            move_right,
        )
    }
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        mut rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !(self.base().is::<types::Mut>() || ctx.is_const.get()) {
            rhs.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                ctx.is_const.get(),
                self.base().llvm_type(ctx),
                rhs.value(ctx),
            ) {
                rhs.address = Rc::new(Cell::new(Some(pv)));
                Some(ctx.builder.build_load(llt, pv, ""))
            } else {
                None
            };
        }
        self.base()._bin_rhs(
            lhs,
            rhs,
            op,
            ctx,
            move_left,
            move_right && move_left && !self.base().copyable(ctx),
        )
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(base) = self.base().downcast::<types::Mut>() {
            if base
                .base()
                ._has_refmut_call(&args.iter().map(|t| t.data_type).collect::<Vec<_>>(), ctx)
            {
                return base.base()._refmut_call(val, cparen, args, ctx);
            }
        }
        if self
            .base()
            ._has_ref_call(&args.iter().map(|t| t.data_type).collect::<Vec<_>>(), ctx)
        {
            self.base()._ref_call(val, cparen, args, ctx)
        } else {
            self.base().call(val, cparen, args, ctx)
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        (if let Some(base) = self.base().downcast::<types::Mut>() {
            base.base() == other || base.base()._can_refmut_iconv(other, ctx)
        } else {
            self.base() == other
        }) || self.base()._can_ref_iconv(other, ctx)
            || (self.base().copyable(ctx) && self.base()._can_iconv_to(other, ctx))
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        (if let Some(base) = self.base().downcast::<types::Mut>() {
            base.base()._can_refmut_econv(other, ctx)
        } else {
            false
        }) || self.base()._can_ref_econv(other, ctx)
            || (self.base().copyable(ctx) && self.base()._can_econv_to(other, ctx))
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(base) = self.base().downcast::<types::Mut>() {
            if base.base()._can_refmut_iconv(target.0, ctx) {
                return base.base()._refmut_iconv(val, target, ctx);
            }
            if base.base() == target.0 {
                return if base.base().copyable(ctx) {
                    val.comp_val =
                        if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                            ctx.is_const.get(),
                            base.base().llvm_type(ctx),
                            val.value(ctx),
                        ) {
                            Some(ctx.builder.build_load(llt, pv, ""))
                        } else {
                            None
                        };
                    val.data_type = base.base();
                    Ok(val)
                } else {
                    Err(CobaltError::CantMoveFromReference {
                        loc: val.loc,
                        ty: base.base().to_string(),
                    })
                };
            }
            if let Some(r) = target.0.downcast::<types::Reference>() {
                if r.base() == base.base() {
                    return Ok(Value {
                        data_type: target.0,
                        ..val
                    });
                }
            }
        }
        if self.base() == target.0 {
            return if self.base().copyable(ctx) {
                val.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                    ctx.is_const.get(),
                    self.base().llvm_type(ctx),
                    val.value(ctx),
                ) {
                    val.address = Rc::new(Cell::new(Some(pv)));
                    Some(ctx.builder.build_load(llt, pv, ""))
                } else {
                    None
                };
                val.data_type = self.base();
                Ok(val)
            } else {
                Err(CobaltError::CantMoveFromReference {
                    loc: val.loc,
                    ty: self.base().to_string(),
                })
            };
        }
        if self.base()._can_ref_iconv(target.0, ctx) {
            self.base()._ref_iconv(val, target, ctx)
        } else if self.base().copyable(ctx) {
            if !(ctx.is_const.get() || self.base().is::<types::Mut>()) {
                val.comp_val = val.comp_val.and_then(|v| {
                    Some(ctx.builder.build_load(
                        self.base().llvm_type(ctx)?,
                        v.is_pointer_value().then(|| v.into_pointer_value())?,
                        "",
                    ))
                })
            }
            val.data_type = self.base();
            self.base()._iconv_to(val, target, ctx)
        } else {
            Err(CobaltError::CantMoveFromReference {
                loc: val.loc,
                ty: self.base().to_string(),
            })
        }
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(base) = self.base().downcast::<types::Mut>() {
            if base.base()._can_refmut_econv(target.0, ctx) {
                return base.base()._refmut_econv(val, target, ctx);
            }
        }
        if self.base()._can_ref_econv(target.0, ctx) {
            self.base()._ref_econv(val, target, ctx)
        } else if self.base().copyable(ctx) {
            if !(ctx.is_const.get() || self.base().is::<types::Mut>()) {
                val.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                    ctx.is_const.get(),
                    self.base().llvm_type(ctx),
                    val.value(ctx),
                ) {
                    val.address = Rc::new(Cell::new(Some(pv)));
                    Some(ctx.builder.build_load(llt, pv, ""))
                } else {
                    None
                };
            }
            val.data_type = self.base();
            self.base()._econv_to(val, target, ctx)
        } else {
            Err(CobaltError::CantMoveFromReference {
                loc: val.loc,
                ty: self.base().to_string(),
            })
        }
    }
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        (if let Some(b) = self.base().downcast::<types::Mut>() {
            b.base() == other
        } else {
            false
        }) || self.base() == other
    }
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(b) = self.base().downcast::<types::Mut>() {
            if b.base() == val.data_type {
                return Ok(Value {
                    comp_val: val.addr(ctx).map(From::from),
                    data_type: self,
                    ..val
                });
            }
        }
        if self.base() == val.data_type {
            Ok(Value {
                comp_val: val.addr(ctx).map(From::from),
                data_type: self,
                ..val
            })
        } else {
            Err(cant_iconv(&val, self, target))
        }
    }
    fn attr<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(b) = self.base().downcast::<types::Mut>() {
            if b.base()._has_refmut_attr(&attr.0, ctx) {
                return b.base()._refmut_attr(val, attr, ctx);
            }
        }
        if self.base()._has_ref_attr(&attr.0, ctx) {
            self.base()._ref_attr(val, attr, ctx)
        } else {
            if !(ctx.is_const.get() || self.base().is::<types::Mut>()) {
                val.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                    ctx.is_const.get(),
                    self.base().llvm_type(ctx),
                    val.value(ctx),
                ) {
                    val.address = Rc::new(Cell::new(Some(pv)));
                    Some(ctx.builder.build_load(llt, pv, ""))
                } else {
                    None
                };
            }
            self.base().attr(val, attr, ctx)
        }
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !(ctx.is_const.get() || self.base().is::<types::Mut>()) {
            val.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                ctx.is_const.get(),
                self.base().llvm_type(ctx),
                val.value(ctx),
            ) {
                val.address = Rc::new(Cell::new(Some(pv)));
                Some(ctx.builder.build_load(llt, pv, ""))
            } else {
                None
            };
        }
        val.data_type = self.base();
        self.base().subscript(val, idx, ctx)
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "*{}", _0)]
#[repr(transparent)]
pub struct Pointer(TypeRef);
impl Pointer {
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl ConcreteType for Pointer {
    const KIND: NonZeroU64 = make_id(b"ptr");
}
impl Type for Pointer {
    fn size(&self) -> SizeType {
        SizeType::Static(8)
    }
    fn align(&self) -> u16 {
        8
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if op == "*" {
            Ok(Value {
                data_type: types::Reference::new(self.base()),
                ..val
            })
        } else {
            Err(invalid_preop(&val, op, oloc))
        }
    }
    fn _has_mut_pre_op(&'static self, op: &'static str, ctx: &CompCtx) -> bool {
        self.base()
            .ptr_type(ctx)
            .map_or(false, BasicTypeEnum::is_pointer_type)
            && (op == "++" || op == "--")
    }
    fn _mut_pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self
            .base()
            .ptr_type(ctx)
            .map_or(false, BasicTypeEnum::is_pointer_type)
        {
            match op {
                "++" => Ok(Value::new(
                    if let (Some(llt), Some(BasicValueEnum::PointerValue(pv))) =
                        (val.data_type.llvm_type(ctx), val.comp_val)
                    {
                        if self
                            .base()
                            .ptr_type(ctx)
                            .map_or(false, BasicTypeEnum::is_pointer_type)
                        {
                            let v1 =
                                ctx.builder
                                    .build_load(llt.ptr_type(Default::default()), pv, "");
                            let v2 = unsafe {
                                ctx.builder.build_gep(
                                    llt,
                                    v1.into_pointer_value(),
                                    &[ctx.context.i64_type().const_int(1, false)],
                                    "",
                                )
                            };
                            ctx.builder.build_store(pv, v2);
                            val.comp_val
                        } else {
                            None
                        }
                    } else {
                        None
                    },
                    None,
                    self.add_ref(true),
                )),
                "--" => Ok(Value::new(
                    if let (Some(llt), Some(BasicValueEnum::PointerValue(pv))) =
                        (val.data_type.llvm_type(ctx), val.comp_val)
                    {
                        if self
                            .base()
                            .ptr_type(ctx)
                            .map_or(false, BasicTypeEnum::is_pointer_type)
                        {
                            let v1 =
                                ctx.builder
                                    .build_load(llt.ptr_type(Default::default()), pv, "");
                            let v2 = unsafe {
                                ctx.builder.build_gep(
                                    llt,
                                    v1.into_pointer_value(),
                                    &[ctx.context.i64_type().const_all_ones()],
                                    "",
                                )
                            };
                            ctx.builder.build_store(pv, v2);
                            val.comp_val
                        } else {
                            None
                        }
                    } else {
                        None
                    },
                    None,
                    self.add_ref(true),
                )),
                _ => Err(invalid_preop(&val, op, oloc)),
            }
        } else {
            Err(invalid_preop(&val, op, oloc))
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
            (other.kind(), op),
            (types::Int::KIND | types::IntLiteral::KIND, "+" | "-")
        ) || (other == self && ["-", "<", ">", "<=", ">=", "==", "!="].contains(&op))
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        matches!(other.kind(), types::Int::KIND | types::IntLiteral::KIND) && op == "+"
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
        match rhs.data_type.kind() {
            types::Int::KIND | types::IntLiteral::KIND
                if self.base().size().as_static().map_or(false, |x| x != 0) =>
            {
                match op.0 {
                    "+" => Ok(Value::new(
                        if let (
                            Some(llt),
                            Some(BasicValueEnum::PointerValue(pv)),
                            Some(BasicValueEnum::IntValue(mut v)),
                        ) = (self.base().llvm_type(ctx), lhs.value(ctx), rhs.value(ctx))
                        {
                            let i64ty = ctx.context.i64_type();
                            v = if rhs
                                .data_type
                                .downcast::<types::Int>()
                                .map_or(false, types::Int::is_signed)
                            {
                                ctx.builder.build_int_s_extend_or_bit_cast(v, i64ty, "")
                            } else {
                                ctx.builder.build_int_z_extend_or_bit_cast(v, i64ty, "")
                            };
                            Some(unsafe { ctx.builder.build_gep(llt, pv, &[v], "") }.into())
                        } else {
                            None
                        },
                        None,
                        self,
                    )),
                    "-" => Ok(Value::new(
                        if let (
                            Some(llt),
                            Some(BasicValueEnum::PointerValue(pv)),
                            Some(BasicValueEnum::IntValue(mut v)),
                        ) = (self.base().llvm_type(ctx), lhs.value(ctx), rhs.value(ctx))
                        {
                            let i64ty = ctx.context.i64_type();
                            v = if rhs
                                .data_type
                                .downcast::<types::Int>()
                                .map_or(false, types::Int::is_signed)
                            {
                                ctx.builder.build_int_s_extend_or_bit_cast(v, i64ty, "")
                            } else {
                                ctx.builder.build_int_z_extend_or_bit_cast(v, i64ty, "")
                            };
                            v = ctx.builder.build_int_neg(v, "");
                            Some(unsafe { ctx.builder.build_gep(llt, pv, &[v], "") }.into())
                        } else {
                            None
                        },
                        None,
                        self,
                    )),
                    _ => Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            types::Pointer::KIND if rhs.data_type == self => match op.0 {
                "-" => Ok(Value::new(
                    if let (
                        Some(llt),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_ptr_diff(llt, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                "<" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(ULT, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                ">" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(UGT, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                "<=" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(ULE, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                ">=" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(UGE, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                "==" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(EQ, l, r, "").into())
                    } else {
                        None
                    },
                    None,
                    self,
                )),
                "!=" => Ok(Value::new(
                    if let (
                        Some(_),
                        Some(BasicValueEnum::PointerValue(l)),
                        Some(BasicValueEnum::PointerValue(r)),
                    ) = (self.base().llvm_type(ctx), lhs.comp_val, rhs.comp_val)
                    {
                        Some(ctx.builder.build_int_compare(NE, l, r, "").into())
                    } else {
                        None
                    },
                    None,
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
        if matches!(
            lhs.data_type.kind(),
            types::Int::KIND | types::IntLiteral::KIND
        ) && op.0 == "+"
        {
            Ok(Value::new(
                if let (
                    Some(llt),
                    Some(BasicValueEnum::PointerValue(pv)),
                    Some(BasicValueEnum::IntValue(mut v)),
                ) = (self.base().llvm_type(ctx), rhs.value(ctx), lhs.value(ctx))
                {
                    let i64ty = ctx.context.i64_type();
                    v = if lhs
                        .data_type
                        .downcast::<types::Int>()
                        .map_or(false, types::Int::is_signed)
                    {
                        ctx.builder.build_int_s_extend_or_bit_cast(v, i64ty, "")
                    } else {
                        ctx.builder.build_int_z_extend_or_bit_cast(v, i64ty, "")
                    };
                    Some(unsafe { ctx.builder.build_gep(llt, pv, &[v], "") }.into())
                } else {
                    None
                },
                None,
                self,
            ))
        } else {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
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
            (other.kind(), op),
            (types::Int::KIND | types::IntLiteral::KIND, "+=" | "-=")
        )
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
        if !(self
            .base()
            .ptr_type(ctx)
            .map_or(false, BasicTypeEnum::is_pointer_type)
            && self.base().size().is_static())
        {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
        } else if rhs.data_type.is::<types::IntLiteral>() {
            let lty = ctx.context.i64_type();
            let pt = ctx.null_type.ptr_type(Default::default());
            if let (Some(llt), Some(BasicValueEnum::PointerValue(lv)), Some(InterData::Int(rv))) =
                (self.base().llvm_type(ctx), lhs.value(ctx), &rhs.inter_val)
            {
                let rv = lty.const_int(*rv as _, true);
                match op.0 {
                    "+=" => {
                        let v1 = ctx.builder.build_load(pt, lv, "").into_pointer_value();
                        let v2 = unsafe { ctx.builder.build_gep(llt, v1, &[rv], "") };
                        ctx.builder.build_store(lv, v2);
                    }
                    "-=" => {
                        let v1 = ctx.builder.build_load(pt, lv, "").into_pointer_value();
                        let v2 = ctx.builder.build_int_neg(v1, "");
                        let v3 = unsafe { ctx.builder.build_gep(llt, v2, &[rv], "") };
                        ctx.builder.build_store(lv, v3);
                    }
                    _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            lhs.data_type = self.add_ref(true);
            Ok(lhs)
        } else if let Some(rty) = rhs.data_type.downcast::<types::Int>() {
            let lty = ctx.context.i64_type();
            let pt = ctx.null_type.ptr_type(Default::default());
            if let (
                Some(llt),
                Some(BasicValueEnum::PointerValue(lv)),
                Some(BasicValueEnum::IntValue(mut rv)),
            ) = (self.base().llvm_type(ctx), lhs.value(ctx), rhs.value(ctx))
            {
                if rty.bits() < 64 {
                    rv = if rty.is_signed() {
                        ctx.builder.build_int_s_extend(rv, lty, "")
                    } else {
                        ctx.builder.build_int_z_extend(rv, lty, "")
                    };
                }
                match op.0 {
                    "+=" => {
                        let v1 = ctx.builder.build_load(pt, lv, "").into_pointer_value();
                        let v2 = unsafe { ctx.builder.build_gep(llt, v1, &[rv], "") };
                        ctx.builder.build_store(lv, v2);
                    }
                    "-=" => {
                        let v1 = ctx.builder.build_load(pt, lv, "").into_pointer_value();
                        let v2 = ctx.builder.build_int_neg(v1, "");
                        let v3 = unsafe { ctx.builder.build_gep(llt, v2, &[rv], "") };
                        ctx.builder.build_store(lv, v3);
                    }
                    _ => return Err(invalid_binop(&lhs, &rhs, op.0, op.1)),
                }
            }
            lhs.data_type = self.add_ref(true);
            Ok(lhs)
        } else {
            Err(invalid_binop(&lhs, &rhs, op.0, op.1))
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        if let (Some(b), Some(r)) = (
            self.base().downcast::<types::Mut>(),
            other.downcast::<types::Pointer>(),
        ) {
            b.base() == r.base()
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
        if let (Some(b), Some(r)) = (
            self.base().downcast::<types::Mut>(),
            target.0.downcast::<types::Pointer>(),
        ) {
            if b.base() == r.base() {
                return Ok(Value {
                    data_type: target.0,
                    ..val
                });
            }
        }
        Err(cant_iconv(&val, target.0, target.1))
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
#[derive(Debug, Display, RefCastCustom)]
#[display(fmt = "mut {}", _0)]
#[repr(transparent)]
pub struct Mut(TypeRef);
impl Mut {
    #[ref_cast_custom]
    fn from_ref(base: &TypeRef) -> &Self;
    pub fn new(base: TypeRef) -> &'static Self {
        static INTERN: Interner<TypeRef> = Interner::new();
        Self::from_ref(INTERN.intern(base))
    }
    pub fn base(&self) -> TypeRef {
        self.0
    }
}
impl ConcreteType for Mut {
    const KIND: NonZeroU64 = make_id(b"mut");
}
impl Type for Mut {
    fn size(&self) -> SizeType {
        self.0.size()
    }
    fn align(&self) -> u16 {
        self.0.align()
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn decay(&self) -> TypeRef {
        self.base().decay()
    }
    fn has_dtor(&'static self, ctx: &CompCtx) -> bool {
        self.base().has_dtor(ctx)
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        self.base().ins_dtor(val, ctx)
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.base().is_linear(ctx)
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.base()._has_mut_pre_op(op, ctx) {
            self.base()._mut_pre_op(val, op, oloc, ctx, can_move)
        } else {
            if !ctx.is_const.get() {
                val.comp_val = val.comp_val.and_then(|v| {
                    Some(ctx.builder.build_load(
                        self.base().llvm_type(ctx)?,
                        v.is_pointer_value().then(|| v.into_pointer_value())?,
                        "",
                    ))
                })
            }
            self.base().pre_op(val, op, oloc, ctx, can_move)
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
        (op == "=" && other.impl_convertible(self, ctx))
            || self
                .base()
                ._has_mut_bin_lhs(other, op, ctx, move_left, move_right)
            || self
                .base()
                ._has_bin_lhs(other, op, ctx, move_left, move_right)
    }
    fn _has_bin_rhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        self.base()
            ._has_bin_rhs(other, op, ctx, move_left, move_left)
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        mut lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if op.0 == "=" && rhs.data_type.impl_convertible(self.as_type_ref(), ctx) {
            let rhs = rhs.impl_convert((self, Some(lhs.loc)), ctx)?;
            if let (Some(BasicValueEnum::PointerValue(pv)), Some(val)) =
                (lhs.comp_val, rhs.comp_val)
            {
                let inst = ctx.builder.build_store(pv, val);
                cfg::mark_move(&rhs, cfg::Location::Inst(inst, 0), ctx, rhs.loc);
                cfg::mark_store(&lhs, cfg::Location::Inst(inst, 1), ctx);
            }
            lhs.data_type = self.add_ref(true);
            return Ok(lhs);
        }
        if self
            .base()
            ._has_mut_bin_lhs(rhs.data_type, op.0, ctx, move_left, move_right)
        {
            self.base()
                ._mut_bin_lhs(lhs, rhs, op, ctx, move_left, move_right)
        } else {
            if !ctx.is_const.get() {
                lhs.comp_val = lhs.comp_val.and_then(|v| {
                    Some(ctx.builder.build_load(
                        self.base().llvm_type(ctx)?,
                        v.is_pointer_value().then(|| v.into_pointer_value())?,
                        "",
                    ))
                })
            }
            self.base()
                ._bin_lhs(lhs, rhs, op, ctx, move_left, move_right)
        }
    }
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        mut rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !ctx.is_const.get() {
            rhs.comp_val = rhs.comp_val.and_then(|v| {
                Some(ctx.builder.build_load(
                    self.base().llvm_type(ctx)?,
                    v.is_pointer_value().then(|| v.into_pointer_value())?,
                    "",
                ))
            })
        }
        self.base()
            ._bin_rhs(lhs, rhs, op, ctx, move_left, move_right)
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_iconv_to(other, ctx)
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_econv_to(other, ctx)
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !ctx.is_const.get() {
            if let (Some(llt), Some(BasicValueEnum::PointerValue(pv))) =
                (self.base().llvm_type(ctx), val.value(ctx))
            {
                val.comp_val = Some(ctx.builder.build_load(llt, pv, ""));
            }
        }
        self.base()._iconv_to(val, target, ctx)
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if !ctx.is_const.get() {
            if let (Some(llt), Some(BasicValueEnum::PointerValue(pv))) =
                (self.base().llvm_type(ctx), val.value(ctx))
            {
                val.comp_val = Some(ctx.builder.build_load(llt, pv, ""));
            }
        }
        self.base()._econv_to(val, target, ctx)
    }
    fn _can_ref_iconv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        target == self.base().add_ref(false)
            || self.base()._can_refmut_iconv(target, ctx)
            || self.base()._can_ref_iconv(target, ctx)
    }
    fn _can_ref_econv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_refmut_econv(target, ctx) || self.base()._can_ref_econv(target, ctx)
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.base()._has_mut_subscript(idx.data_type, ctx) {
            self.base()._mut_subscript(val, idx, ctx)
        } else {
            if !ctx.is_const.get() {
                val.comp_val = if let (false, Some(llt), Some(BasicValueEnum::PointerValue(pv))) = (
                    ctx.is_const.get(),
                    self.base().llvm_type(ctx),
                    val.value(ctx),
                ) {
                    val.address = Rc::new(Cell::new(Some(pv)));
                    Some(ctx.builder.build_load(llt, pv, ""))
                } else {
                    None
                };
            }
            self.base().subscript(val, idx, ctx)
        }
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.base()._has_mut_attr(&attr.0, ctx) {
            self.base()._mut_attr(val, attr, ctx)
        } else {
            self.base().attr(val, attr, ctx)
        }
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.base()._has_refmut_attr(&attr.0, ctx) {
            self.base()._refmut_attr(val, attr, ctx)
        } else {
            self.base()._ref_attr(val, attr, ctx)
        }
    }
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        self.base()._has_refmut_attr(attr, ctx)
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
submit_types!(Reference, Pointer, Mut);
