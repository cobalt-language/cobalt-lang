use super::*;
static INTRINSIC_INTERN: Interner<Box<str>> = Interner::new();
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
#[display(fmt = "@{_0}")]
pub struct Intrinsic(Box<str>);
impl Intrinsic {
    pub const KIND: NonZeroU64 = make_id(b"intrin");
    #[ref_cast_custom]
    #[inline(always)]
    #[allow(clippy::borrowed_box)]
    fn from_ref(name: &Box<str>) -> &Self;
    pub fn new(name: impl Into<Box<str>>) -> &'static Self {
        Self::from_ref(INTRINSIC_INTERN.intern(name.into()))
    }
    pub fn new_ref(name: &str) -> &'static Self {
        Self::from_ref(INTRINSIC_INTERN.intern_ref(name))
    }
    pub fn name(&self) -> &str {
        &self.0
    }
}
impl ConcreteType for Intrinsic {
    const KIND: NonZeroU64 = make_id(b"intrin");
}
impl Type for Intrinsic {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn decay(&'static self) -> TypeRef {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            (i.ret)().decay()
        } else {
            self
        }
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            (i.ret)().impl_convertible(other, ctx)
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
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            v.impl_convert(target, ctx)
        } else {
            Err(cant_iconv(&val, target.0, target.1))
        }
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            (i.ret)().expl_convertible(other, ctx)
        } else {
            false
        }
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            v.expl_convert(target, ctx)
        } else {
            Err(cant_econv(&val, target.0, target.1))
        }
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::FUNCTION_INTRINSICS.pin().get(self.name()) {
            (i.wraps)(val.loc, cparen.unwrap_or(val.loc), args, ctx)
        } else if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            (i.wraps)(ctx).call(cparen, args, ctx)
        } else {
            Err(CobaltError::UnknownIntrinsic {
                name: self.name().into(),
                loc: val.loc,
            })
        }
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            return v.data_type.attr(v, attr, ctx);
        }
        Err(CobaltError::AttrNotDefined {
            val: self.to_string(),
            attr: attr.0,
            vloc: val.loc,
            aloc: attr.1,
        })
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            return v.data_type.pre_op(v, op, oloc, ctx, can_move);
        }
        Err(invalid_preop(&val, op, oloc))
    }
    fn post_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            return v.data_type.post_op(v, op, oloc, ctx, can_move);
        }
        Err(invalid_postop(&val, op, oloc))
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let v = (i.wraps)(ctx);
            return v.data_type.subscript(v, idx, ctx);
        }
        idx.data_type
            .is::<types::Error>()
            .then(Value::error)
            .ok_or_else(|| invalid_sub(&val, &idx))
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
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let lhs = (i.wraps)(ctx);
            return lhs
                .data_type
                ._bin_lhs(lhs, rhs, op, ctx, move_left, move_right);
        }
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
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
        if let Some(i) = intrinsics::VALUE_INTRINSICS.pin().get(self.name()) {
            let rhs = (i.wraps)(ctx);
            return rhs
                .data_type
                ._bin_rhs(lhs, rhs, op, ctx, move_left, move_right);
        }
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    fn _has_bin_lhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        intrinsics::VALUE_INTRINSICS
            .pin()
            .get(self.name())
            .map_or(false, |i| {
                (i.ret)()._has_bin_lhs(other, op, ctx, move_left, move_right)
            })
    }
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        intrinsics::VALUE_INTRINSICS
            .pin()
            .get(self.name())
            .map_or(false, |i| {
                (i.ret)()._has_bin_rhs(other, op, ctx, move_left, move_right)
            })
    }
    fn compiled<'src, 'ctx>(
        &'static self,
        inter_val: &InterData<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        intrinsics::VALUE_INTRINSICS
            .pin()
            .get(self.name())
            .and_then(|i| {
                let v = (i.wraps)(ctx);
                v.data_type.compiled(&v.inter_val?, ctx)
            })
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        serial_utils::save_str(out, self.name())
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        serial_utils::load_str(buf).map(|s| Self::new(s) as _)
    }
}
static ASM_INTERN: Interner<TypeRef> = Interner::new();
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
#[display(fmt = "@asm({_0})")]
pub struct InlineAsm(TypeRef);
impl InlineAsm {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(ret: &TypeRef) -> &Self;
    pub fn new(ret: TypeRef) -> &'static Self {
        Self::from_ref(ASM_INTERN.intern(ret))
    }
}
impl ConcreteType for InlineAsm {
    const KIND: NonZeroU64 = make_id(b"asm");
}
impl Type for InlineAsm {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if ctx.is_const.get() {
            return Ok(Value::error());
        }
        let Some(InterData::InlineAsm(c, b)) = val.inter_val else {
            return Ok(Value::error());
        };
        let mut err = CobaltError::InvalidInlineAsmCall {
            loc: val.loc,
            args: vec![],
        };
        let mut push_arg = |arg: InvalidAsmArg<'src>| {
            if let CobaltError::InvalidInlineAsmCall { args, .. } = &mut err {
                args.push(arg)
            }
        };
        let mut good = true;
        let mut params = Vec::with_capacity(args.len());
        let mut comp_args = Vec::with_capacity(args.len());
        for arg in args {
            let n = arg.data_type.to_string();
            let l = arg.loc;
            let d = arg.decay(ctx);
            match d.data_type.kind() {
                types::Int::KIND | types::Float::KIND | types::Pointer::KIND => {
                    params.push(d.data_type.llvm_type(ctx).unwrap().into());
                    comp_args.push(d.value(ctx).unwrap().into());
                }
                types::Error::KIND => {}
                _ => {
                    push_arg(InvalidAsmArg(d.data_type.to_string().into(), l));
                    good = false;
                }
            }
        }
        if !good {
            return Err(err);
        }
        if let Some(llt) = self.0.llvm_type(ctx) {
            let fty = llt.fn_type(&params, false);
            let asm = ctx
                .context
                .create_inline_asm(fty, b, c, true, true, None, false);
            Ok(Value::new(
                ctx.builder
                    .build_indirect_call(fty, asm, &comp_args, "")
                    .try_as_basic_value()
                    .left(),
                None,
                self.0,
            ))
        } else if self.0.size() == SizeType::Static(0) {
            let fty = ctx.context.void_type().fn_type(&params, false);
            let asm = ctx
                .context
                .create_inline_asm(fty, b, c, true, true, None, false);
            Ok(Value::new(
                ctx.builder
                    .build_indirect_call(fty, asm, &comp_args, "")
                    .try_as_basic_value()
                    .left(),
                None,
                self.0,
            ))
        } else {
            push_arg(InvalidAsmArg(self.0.to_string().into(), val.loc));
            Err(err)
        }
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        types::save_type(out, self.0)
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        types::load_type(buf).map(|s| Self::new(s) as _)
    }
}
submit_types!(Intrinsic);
