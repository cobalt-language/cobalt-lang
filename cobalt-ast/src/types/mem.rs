use super::*;
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
            .pre_op(val, op, oloc, ctx, can_move && !self.base().has_dtor(ctx))
    }
    fn _has_bin_lhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        self.base()
            ._has_bin_lhs(other, op, ctx, move_left, move_right)
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
        if !(self.base().is::<types::Mut>() || ctx.is_const.get()) {
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
            base.base()._can_refmut_iconv(other, ctx)
        } else {
            false
        }) || self.base()._can_ref_iconv(other, ctx)
            || (!self.base().has_dtor(ctx) && self.base()._can_iconv_to(other, ctx))
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        (if let Some(base) = self.base().downcast::<types::Mut>() {
            base.base()._can_refmut_econv(other, ctx)
        } else {
            false
        }) || self.base()._can_ref_econv(other, ctx)
            || (!self.base().has_dtor(ctx) && self.base()._can_econv_to(other, ctx))
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
        }
        if self.base()._can_ref_iconv(target.0, ctx) {
            self.base()._ref_iconv(val, target, ctx)
        } else if !self.base().has_dtor(ctx) {
            if !ctx.is_const.get() {
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
        } else if !self.base().has_dtor(ctx) {
            if !ctx.is_const.get() {
                val.comp_val = val.comp_val.and_then(|v| {
                    Some(ctx.builder.build_load(
                        self.base().llvm_type(ctx)?,
                        v.is_pointer_value().then(|| v.into_pointer_value())?,
                        "",
                    ))
                })
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
                    if let Some(BasicValueEnum::PointerValue(pv)) = val.comp_val {
                        if self
                            .base()
                            .ptr_type(ctx)
                            .map_or(false, BasicTypeEnum::is_pointer_type)
                        {
                            let pt = ctx.null_type.ptr_type(Default::default());
                            let v1 = ctx.builder.build_load(pt, pv, "");
                            let v2 = unsafe {
                                ctx.builder.build_gep(
                                    pt,
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
                    self.add_ref(false),
                )),
                "--" => Ok(Value::new(
                    if let Some(BasicValueEnum::PointerValue(pv)) = val.comp_val {
                        if self
                            .base()
                            .ptr_type(ctx)
                            .map_or(false, BasicTypeEnum::is_pointer_type)
                        {
                            let pt = ctx.null_type.ptr_type(Default::default());
                            let v1 = ctx.builder.build_load(pt, pv, "");
                            let v2 = unsafe {
                                ctx.builder.build_gep(
                                    pt,
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
                    self.add_ref(false),
                )),
                _ => Err(invalid_preop(&val, op, oloc)),
            }
        } else {
            Err(invalid_preop(&val, op, oloc))
        }
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
    pub const KIND: NonZeroU64 = make_id(b"mut");
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
                ctx.builder.build_store(pv, val);
            }
            lhs.data_type = types::Reference::new(lhs.data_type);
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
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_iconv_from(other, ctx)
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_econv_to(other, ctx)
    }
    fn _can_econv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.base()._can_econv_from(other, ctx)
    }
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.base()._iconv_to(val, target, ctx)
    }
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.base()._iconv_from(val, target, ctx)
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.base()._econv_to(val, target, ctx)
    }
    fn _econv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.base()._econv_from(val, target, ctx)
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        load_type(buf).map(|t| Self::new(t) as _)
    }
}
submit_types!(Reference, Pointer, Mut);
