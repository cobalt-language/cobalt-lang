use super::*;
#[derive(Debug, RefCastCustom)]
#[repr(transparent)]
pub struct Function((TypeRef, Box<[(TypeRef, bool)]>));
impl Function {
    pub const KIND: NonZeroU64 = make_id(b"function");
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
impl ConcreteType for Function {
    const KIND: NonZeroU64 = make_id(b"function");
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
                    aloc = Some(if let Some(l) = aloc {
                        merge_spans(l, v.loc)
                    } else {
                        v.loc
                    });
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
                            Value::error()
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
                let call = ctx.builder.build_indirect_call(fty?, v, &args_v, "");
                let inst = call
                    .try_as_basic_value()
                    .right_or_else(|v| v.as_instruction_value().unwrap());
                for (n, val) in args.iter().enumerate() {
                    ops::mark_move(val, cfg::Location::Inst(inst, n), ctx, val.loc)
                }
                call.try_as_basic_value().left()
            }),
            None,
            self.ret(),
        ))
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.ret())?;
        self.params().iter().try_for_each(|&(ty, c)| {
            save_type(out, ty)?;
            out.write_all(&[u8::from(c)])
        })
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let ret = load_type(buf)?;
        let params = std::iter::from_fn(|| match load_type_opt(buf) {
            Ok(None) => None,
            Ok(Some(t)) => {
                let mut c = 0u8;
                if let Err(err) = buf.read_exact(std::slice::from_mut(&mut c)) {
                    return Some(Err(err));
                }
                Some(Ok((t, c != 0)))
            }
            Err(err) => Some(Err(err)),
        })
        .collect::<Result<Vec<_>, _>>()?;
        Ok(Self::new(ret, params))
    }
}
submit_types!(Function);
