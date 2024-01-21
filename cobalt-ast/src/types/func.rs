use super::*;
#[derive(Debug, ConstIdentify, RefCastCustom)]
#[repr(transparent)]
pub struct Function((TypeRef, Box<[(TypeRef, bool)]>));
impl Function {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(base: &(TypeRef, Box<[(TypeRef, bool)]>)) -> &Self;
    pub fn new(ret: TypeRef, params: impl Into<Box<[(TypeRef, bool)]>>) -> &'static Self {
        static INTERN: Interner<(TypeRef, Box<[(TypeRef, bool)]>)> = Interner::new();
        Self::from_ref(INTERN.intern((ret, params.into())))
    }

    pub fn ret(&self) -> TypeRef {
        self.0 .0
    }
    pub fn params(&self) -> &[(TypeRef, bool)] {
        &self.0 .1
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fn (")?;
        let mut rem = self.params().len();
        for &(ty, c) in self.params() {
            if c {
                f.write_str("const ")?;
            }
            Display::fmt(ty, f)?;
            rem -= 1;
            if rem > 0 {
                f.write_str(", ")?;
            }
        }
        write!(f, "): {}", self.ret())
    }
}
impl Type for Function {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        let params = || {
            self.params()
                .iter()
                .filter_map(|&(t, c)| c.then(|| t.llvm_type(ctx).map(From::from)).flatten())
                .collect::<Vec<_>>()
        };
        if self.ret().size() == SizeType::Static(0) {
            Some(
                ctx.context
                    .void_type()
                    .fn_type(&params(), false)
                    .ptr_type(Default::default())
                    .into(),
            )
        } else {
            Some(
                self.ret()
                    .llvm_type(ctx)?
                    .fn_type(&params(), false)
                    .ptr_type(Default::default())
                    .into(),
            )
        }
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        mut args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let mut aloc = None;
        let mut err = CobaltError::CannotCallWithArgs {
            val: format!(
                "fn ({}): {}",
                self.params()
                    .iter()
                    .map(|(t, _)| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                self.ret()
            ),
            loc: cparen.map_or(val.loc, |cp| merge_spans(val.loc, cp)),
            args: args
                .iter()
                .map(|v| {
                    aloc = if let Some(l) = aloc.and_then(remove_unreachable) {
                        Some(if v.loc == unreachable_span() {
                            l
                        } else {
                            merge_spans(l, v.loc)
                        })
                    } else {
                        remove_unreachable(v.loc)
                    };
                    v.data_type.to_string()
                })
                .collect(),
            aloc,
            nargs: vec![],
        };
        let mut push_arg = |arg: ArgError<'src>| {
            if let CobaltError::CannotCallWithArgs { nargs, .. } = &mut err {
                nargs.push(arg)
            }
        };
        let mut good = true;
        let p = self.params().len();
        let mut a = args.len();
        let defaults = if let Some(InterData::Function(FnData { defaults, .. })) = val.inter_val {
            defaults
        } else {
            vec![]
        };
        if a > p {
            push_arg(ArgError::WrongNumArgs {
                found: a,
                expected: p,
                loc: args[p].loc,
            });
            a = p;
        }
        if a < p {
            let d = defaults.len();
            args.extend(
                &mut defaults.into_iter().enumerate().skip(p - a).map(|(n, v)| {
                    let (pty, pc) = self.params()[p - d + n];
                    Value {
                        loc: cparen.unwrap_or(val.loc),
                        ..Value::new(pc.then(|| pty.compiled(&v, ctx)).flatten(), Some(v), pty)
                    }
                }),
            );
            a = args.len();
        }
        if a < p {
            push_arg(ArgError::WrongNumArgs {
                found: a,
                expected: p,
                loc: cparen.unwrap_or(val.loc),
            });
            args.resize_with(p, || Value {
                loc: cparen.unwrap_or(val.loc),
                ..Value::error()
            });
        }
        let (c, r) = args
            .iter()
            .zip(self.params().iter())
            .enumerate()
            .map(|(n, (v, &(t, c)))| {
                (
                    {
                        let loc = v.loc;
                        if let Ok(val) = v.clone().impl_convert((t, None), ctx) {
                            if c && val.inter_val.is_none() {
                                good = false;
                                push_arg(ArgError::ArgMustBeConst { n, loc })
                            }
                            val
                        } else {
                            good = false;
                            push_arg(ArgError::InvalidArg {
                                n,
                                val: v.data_type.to_string().into(),
                                ty: t.to_string().into(),
                                loc,
                            });
                            Value::error().with_loc(v.loc)
                        }
                    },
                    c,
                )
            })
            .partition::<Vec<_>, _>(|(_, c)| *c);
        if !good {
            return Err(err);
        }
        if !c.is_empty() {
            return Err(CobaltError::ConstFnsArentSupported { loc: val.loc });
        }
        good = true;
        let val: Option<inkwell::values::PointerValue> =
            val.comp_val.and_then(|v| v.try_into().ok());
        let args_v: Vec<inkwell::values::BasicMetadataValueEnum> = r
            .iter()
            .filter_map(|(Value { comp_val, .. }, _)| {
                comp_val.map(|v| v.into()).or_else(|| {
                    good = false;
                    None
                })
            })
            .collect();
        let aty: Vec<inkwell::types::BasicMetadataTypeEnum> = args_v
            .iter()
            .map(|v| BasicValueEnum::try_from(*v).unwrap().get_type().into())
            .collect();
        let fty = if self.ret().size() == SizeType::Static(0) {
            Some(ctx.context.void_type().fn_type(&aty, false))
        } else {
            self.ret().llvm_type(ctx).map(|t| t.fn_type(&aty, false))
        };
        use inkwell::values::BasicValue;
        Ok(Value::new(
            val.and_then(|v| {
                let call = ctx
                    .builder
                    .build_indirect_call(fty?, v, &args_v, "")
                    .unwrap();
                let inst = call
                    .try_as_basic_value()
                    .right_or_else(|v| v.as_instruction_value().unwrap());
                for (n, val) in args.iter().enumerate() {
                    cfg::mark_move(val, cfg::Location::Inst(inst, n), ctx, val.loc)
                }
                call.try_as_basic_value().left()
            }),
            None,
            self.ret(),
        ))
    }
}
#[derive(Debug, ConstIdentify, RefCastCustom)]
#[repr(transparent)]
pub struct BoundMethod((TypeRef, Box<[(TypeRef, bool)]>));
impl BoundMethod {
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(base: &(TypeRef, Box<[(TypeRef, bool)]>)) -> &Self;
    pub fn new(ret: TypeRef, params: impl Into<Box<[(TypeRef, bool)]>>) -> &'static Self {
        let params = params.into();
        assert!(!params.is_empty());
        static INTERN: Interner<(TypeRef, Box<[(TypeRef, bool)]>)> = Interner::new();
        Self::from_ref(INTERN.intern((ret, params)))
    }

    pub fn ret(&self) -> TypeRef {
        self.0 .0
    }
    pub fn full_params(&self) -> &[(TypeRef, bool)] {
        &self.0 .1
    }
    pub fn self_ty(&self) -> TypeRef {
        self.0 .1[0].0
    }
    pub fn is_const(&self) -> bool {
        self.0 .1[0].1
    }
    pub fn params(&self) -> &[(TypeRef, bool)] {
        &self.full_params()[1..]
    }
    pub fn as_function(&self) -> &'static Function {
        Function::new(self.ret(), self.full_params())
    }
}
impl Display for BoundMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {}{}.(",
            if self.is_const() { "const " } else { "" },
            self.self_ty()
        )?;
        let mut rem = self.params().len();
        for &(ty, c) in self.params() {
            if c {
                f.write_str("const ")?;
            }
            Display::fmt(ty, f)?;
            rem -= 1;
            if rem > 0 {
                f.write_str(", ")?;
            }
        }
        write!(f, "): {}", self.ret())
    }
}
impl Type for BoundMethod {
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        let params = || {
            self.full_params()
                .iter()
                .filter_map(|&(t, c)| c.then(|| t.llvm_type(ctx).map(From::from)).flatten())
                .collect::<Vec<_>>()
        };
        let fty = if self.ret().size() == SizeType::Static(0) {
            ctx.context
                .void_type()
                .fn_type(&params(), false)
                .ptr_type(Default::default())
                .into()
        } else {
            self.ret()
                .llvm_type(ctx)?
                .fn_type(&params(), false)
                .ptr_type(Default::default())
                .into()
        };
        if let Some(llt) = self.self_ty().llvm_type(ctx) {
            Some(ctx.context.struct_type(&[llt, fty], false).into())
        } else {
            Some(fty)
        }
    }
    fn has_dtor(&'static self, ctx: &CompCtx) -> bool {
        self.self_ty().has_dtor(ctx)
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        if let Some(BasicValueEnum::StructValue(sv)) = val.comp_val {
            Value::new(
                ctx.builder.build_extract_value(sv, 0, "").ok(),
                None,
                self.self_ty(),
            )
            .ins_dtor(ctx);
        }
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        self.self_ty().is_linear(ctx)
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        mut args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let (comp_val, fv) = match val.comp_val {
            Some(BasicValueEnum::StructValue(sv)) => (
                ctx.builder.build_extract_value(sv, 0, "").ok(),
                ctx.builder.build_extract_value(sv, 1, "").ok(),
            ),
            Some(BasicValueEnum::PointerValue(pv)) => (None, Some(pv.into())),
            _ => (None, None),
        };
        let (iv, fd) = if let Some(InterData::Array(mut v)) = val.inter_val {
            if v.len() == 2 {
                let fd = v.pop().unwrap();
                let iv = v.pop().unwrap();
                matches!(fd, InterData::Function(_)).then_some((Some(iv), Some(fd)))
            } else {
                None
            }
        } else {
            None
        }
        .unwrap_or_default();
        let mut a = Vec::with_capacity(args.len() + 1);
        a.push(Value::new(comp_val, iv, self.self_ty()));
        a.append(&mut args);
        Value::new(fv, fd, self.as_function())
            .with_loc(val.loc)
            .call(cparen, a, ctx)
    }
}

#[doc(hidden)]
#[derive(Clone, Serialize, Deserialize)]
pub struct ParamProxy(
    #[serde(rename = "type")] TypeRef,
    #[serde(rename = "const")] bool,
);
#[doc(hidden)]
#[derive(Serialize, Deserialize)]
pub struct FnProxy {
    ret: TypeRef,
    params: Cow<'static, [ParamProxy]>,
}
impl TypeSerde for Function {
    no_type_header!();
    impl_type_proxy!(
        FnProxy,
        this => FnProxy {
            ret: this.ret(),
            params: Cow::Borrowed(unsafe {std::mem::transmute(this.params())})
        },
        FnProxy { ret, params } => Self::new(ret, unsafe {std::mem::transmute::<_, &[(TypeRef, bool)]>(&*params)})
    );
}
impl TypeSerde for BoundMethod {
    no_type_header!();
    impl_type_proxy!(
        FnProxy,
        this => FnProxy {
            ret: this.ret(),
            params: Cow::Borrowed(unsafe {std::mem::transmute(this.full_params())})
        },
        FnProxy { ret, params } => Self::new(ret, unsafe {std::mem::transmute::<_, &[(TypeRef, bool)]>(&*params)})
    );
}
submit_types!(Function, BoundMethod);
