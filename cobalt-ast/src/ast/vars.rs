use crate::*;
use ast::funcs::hoist_allocas;
use glob::Pattern;
use inkwell::module::Linkage::*;
use inkwell::values::{AsValueRef, BasicValueEnum::*, GlobalValue};
use std::collections::{hash_map::Entry, HashSet};
#[derive(Debug, Clone)]
pub struct VarDefAST<'src> {
    loc: SourceSpan,
    pub name: DottedName<'src>,
    pub val: BoxedAST<'src>,
    pub type_: Option<BoxedAST<'src>>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    pub global: bool,
    pub is_mut: bool,
}
impl<'src> VarDefAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: DottedName<'src>,
        val: BoxedAST<'src>,
        type_: Option<BoxedAST<'src>>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
        global: bool,
        is_mut: bool,
    ) -> Self {
        VarDefAST {
            loc,
            name,
            val,
            type_,
            annotations,
            global,
            is_mut,
        }
    }
}
impl<'src> AST<'src> for VarDefAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.loc, self.val.loc())
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + self.type_.as_ref().map_or(0, |x| x.nodes()) + 1
    }
    fn fwddef_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut errs = vec![];
        let mut link_type = None;
        let mut linkas = None;
        let mut vis_spec = None;
        let mut is_extern = false;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "link" => {
                    link_type = match arg.as_deref() {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak")
                        | Some("extern_weak")
                        | Some("external-weak")
                        | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => {
                            Some(LinkOnceAny)
                        }
                        Some("linkonce-odr")
                        | Some("linkonce_odr")
                        | Some("link-once-odr")
                        | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => None,
                    }
                    .map(|x| (x, loc))
                }
                "linkas" => {
                    if let Some(arg) = arg {
                        linkas = Some(arg.clone())
                    }
                }
                "extern" => is_extern = true,
                "c" | "C" => {
                    is_extern |= arg.as_deref() == Some("extern");
                    linkas = Some(
                        self.name
                            .ids
                            .last()
                            .expect("function name shouldn't be empty!")
                            .0
                            .clone(),
                    )
                }
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            target_match = u8::from(
                                negate
                                    ^ pat.matches(
                                        &ctx.module.get_triple().as_str().to_string_lossy(),
                                    ),
                            )
                        }
                    }
                }
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            _ => {}
                        }
                    }
                }
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        let vs = vis_spec.unwrap_or(ctx.export.get());
        if target_match == 0 {
            return;
        }
        let t2 = self.val.const_codegen(ctx).0.data_type;
        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
            let oic = ctx.is_const.replace(true);
            let t = ops::impl_convert(
                t.loc(),
                (t.codegen_errs(ctx, &mut errs), None),
                (types::TypeData::new(), None),
                ctx,
            )
            .ok()
            .and_then(Value::as_type)
            .unwrap_or(types::Error::new());
            ctx.is_const.set(oic);
            t
        }) {
            t
        } else {
            t2.decay()
        };
        let _ = ctx.with_vars(|v| {
            v.insert(
                &self.name,
                Symbol(
                    Value::new(
                        dt.llvm_type(ctx).map(|t| {
                            let gv = ctx.module.add_global(
                                t,
                                None,
                                &linkas.unwrap_or_else(|| ctx.mangle(&self.name).into()),
                            );
                            match link_type {
                                None => {
                                    if ctx.flags.private_syms && !(vs || is_extern) {
                                        gv.set_linkage(Private)
                                    }
                                }
                                Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                                Some((x, _)) => gv.set_linkage(x),
                            }
                            gv.set_constant(!self.is_mut);
                            PointerValue(gv.as_pointer_value())
                        }),
                        None,
                        Type::Reference(ops::maybe_mut(Box::new(dt), self.is_mut)),
                    ),
                    VariableData {
                        fwd: true,
                        ..VariableData::with_vis(self.loc, vs)
                    },
                ),
            )
        });
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "static" => {
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "static",
                            expected: None,
                            found: arg.clone(),
                            loc,
                        });
                    }
                    is_static = true;
                }
                "link" => {
                    if let Some((_, prev)) = link_type {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "link",
                            loc,
                            prev,
                        });
                    }
                    link_type = match arg.as_deref() {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak")
                        | Some("extern_weak")
                        | Some("external-weak")
                        | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => {
                            Some(LinkOnceAny)
                        }
                        Some("linkonce-odr")
                        | Some("linkonce_odr")
                        | Some("link-once-odr")
                        | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "link",
                                found: arg.clone(),
                                expected: Some("link type"),
                                loc,
                            });
                            None
                        }
                    }
                    .map(|x| (x, loc))
                }
                "linkas" => {
                    if let Some((_, prev)) = linkas {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "linkas",
                            loc,
                            prev,
                        });
                    }
                    if let Some(arg) = arg {
                        linkas = Some((arg.clone(), loc))
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "linkas",
                            found: arg.clone(),
                            expected: Some("linkage name"),
                            loc,
                        });
                    }
                }
                "extern" => {
                    is_extern = Some(loc);
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "extern",
                            found: arg.clone(),
                            expected: None,
                            loc,
                        });
                    }
                }
                "c" | "C" => {
                    match arg.as_deref() {
                        Some("") | None => {}
                        Some("extern") => is_extern = Some(loc),
                        Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                            name: "C",
                            found: arg.clone(),
                            expected: Some(r#"no argument or "extern""#),
                            loc,
                        }),
                    }
                    linkas = Some((
                        self.name
                            .ids
                            .last()
                            .expect("variable name shouldn't be empty!")
                            .0
                            .clone(),
                        loc,
                    ))
                }
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        match Pattern::new(arg) {
                            Ok(pat) => {
                                if target_match != 1 {
                                    target_match = u8::from(
                                        negate
                                            ^ pat.matches(
                                                &ctx.module.get_triple().as_str().to_string_lossy(),
                                            ),
                                    )
                                }
                            }
                            Err(err) => errs.push(CobaltError::GlobPatternError {
                                pos: err.pos,
                                msg: err.msg,
                                loc,
                            }),
                        }
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc,
                        });
                    }
                }
                "export" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "export",
                            loc,
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((true, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "private" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "private",
                            loc,
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((false, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "variable",
                }),
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {
            return (Value::null(), errs);
        }
        if self.global || is_static {
            if is_extern.is_some() {
                let t2 = self.val.const_codegen_errs(ctx, &mut errs).data_type;
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = ops::impl_convert(
                        t.loc(),
                        (t.codegen_errs(ctx, &mut errs), None),
                        (types::TypeData::new(), None),
                        ctx,
                    )
                    .map_or_else(
                        |e| {
                            errs.push(e);
                            types::Error::new()
                        },
                        |v| {
                            if let Some(InterData::Type(t)) = v.inter_val {
                                *t
                            } else {
                                types::Error::new()
                            }
                        },
                    );
                    ctx.is_const.set(oic);
                    t
                }) {
                    t
                } else {
                    t2.decay()
                };
                match ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            Value::new(
                                dt.llvm_type(ctx)
                                    .map(|t| {
                                        let mangled = linkas.map_or_else(
                                            || ctx.mangle(&self.name).into(),
                                            |(name, _)| name,
                                        );
                                        let gv = ctx
                                            .lookup_full(&self.name)
                                            .and_then(|x| -> Option<GlobalValue> {
                                                Some(unsafe {
                                                    std::mem::transmute(x.comp_val?.as_value_ref())
                                                })
                                            })
                                            .unwrap_or_else(|| {
                                                ctx.module.add_global(t, None, &mangled)
                                            });
                                        match link_type {
                                            None => {}
                                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                                            Some((x, _)) => gv.set_linkage(x),
                                        }
                                        gv.set_constant(!self.is_mut);
                                        PointerValue(gv.as_pointer_value())
                                    })
                                    .or_else(|| {
                                        if dt != types::Error::new() {
                                            errs.push(CobaltError::TypeIsConstOnly {
                                                ty: dt.to_string(),
                                                loc: self.type_.as_ref().unwrap_or(&self.val).loc(),
                                            })
                                        };
                                        None
                                    }),
                                None,
                                Type::Reference(ops::maybe_mut(Box::new(dt), self.is_mut)),
                            ),
                            VariableData::with_vis(self.loc, vs),
                        ),
                    )
                }) {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                        });
                        (Value::error(), errs)
                    }
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d,
                        });
                        (Value::error(), errs)
                    }
                }
            } else if self.val.is_const() && self.type_.is_none() {
                let mut val = self.val.codegen_errs(ctx, &mut errs);
                let t2 = val.data_type.clone();
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = ops::impl_convert(
                        t.loc(),
                        (t.codegen_errs(ctx, &mut errs), None),
                        (types::TypeData::new(), None),
                        ctx,
                    )
                    .map_or_else(
                        |e| {
                            errs.push(e);
                            types::Error::new()
                        },
                        |v| {
                            if let Some(InterData::Type(t)) = v.inter_val {
                                *t
                            } else {
                                types::Error::new()
                            }
                        },
                    );
                    ctx.is_const.set(oic);
                    t
                }) {
                    t
                } else {
                    t2.decay()
                };
                match if let Some(v) = val.value(ctx) {
                    val.inter_val = None;
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| {
                            v.insert(
                                &self.name,
                                Symbol(val, VariableData::with_vis(self.loc, vs)),
                            )
                        })
                    } else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let mangled =
                            linkas.map_or_else(|| ctx.mangle(&self.name).into(), |(name, _)| name);
                        let gv = ctx
                            .lookup_full(&self.name)
                            .and_then(|x| -> Option<GlobalValue> {
                                Some(unsafe { std::mem::transmute(x.comp_val?.as_value_ref()) })
                            })
                            .unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_initializer(&v);
                        gv.set_constant(!self.is_mut);
                        if let Some((link, _)) = link_type {
                            gv.set_linkage(link)
                        } else if ctx.flags.private_syms && !(vs || is_extern.is_some()) {
                            gv.set_linkage(Private)
                        }
                        ctx.with_vars(|v| {
                            v.insert(
                                &self.name,
                                Symbol(
                                    Value::new(
                                        Some(PointerValue(gv.as_pointer_value())),
                                        None,
                                        Type::Reference(ops::maybe_mut(Box::new(dt), self.is_mut)),
                                    ),
                                    VariableData::with_vis(self.loc, vs),
                                ),
                            )
                        })
                    }
                } else {
                    val.inter_val = None;
                    if dt != types::Error::new() {
                        errs.push(CobaltError::TypeIsConstOnly {
                            ty: dt.to_string(),
                            loc: self.type_.as_ref().unwrap_or(&self.val).loc(),
                        })
                    }
                    ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(val, VariableData::with_vis(self.loc, false)),
                        )
                    })
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                        });
                        (Value::error(), errs)
                    }
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d,
                        });
                        (Value::error(), errs)
                    }
                }
            } else {
                let res = if ctx.is_const.get() {
                    let val = self.val.codegen_errs(ctx, &mut errs);
                    let t2 = val.data_type.clone();
                    let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                        let oic = ctx.is_const.replace(true);
                        let t = ops::impl_convert(
                            t.loc(),
                            (t.codegen_errs(ctx, &mut errs), None),
                            (types::TypeData::new(), None),
                            ctx,
                        )
                        .map_or(types::Error::new(), |v| {
                            if let Some(InterData::Type(t)) = v.inter_val {
                                *t
                            } else {
                                types::Error::new()
                            }
                        });
                        ctx.is_const.set(oic);
                        t
                    }) {
                        t
                    } else {
                        t2.decay()
                    };
                    let mut val = ops::impl_convert(self.val.loc(), (val, None), (dt, None), ctx)
                        .unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                    val.inter_val = None;
                    ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(val.freeze(self.loc), VariableData::with_vis(self.loc, vs)),
                        )
                    })
                } else {
                    let f = ctx.module.add_function(
                        format!("cobalt.init{}", ctx.mangle(&self.name)).as_str(),
                        ctx.context.void_type().fn_type(&[], false),
                        Some(inkwell::module::Linkage::Private),
                    );
                    let entry = ctx.context.append_basic_block(f, "entry");
                    let old_ip = ctx.builder.get_insert_block();
                    ctx.builder.position_at_end(entry);
                    let old_scope = ctx.push_scope(&self.name);
                    let val = self.val.codegen_errs(ctx, &mut errs);
                    let t2 = val.data_type.clone();
                    let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                        let oic = ctx.is_const.replace(true);
                        let t = ops::impl_convert(
                            t.loc(),
                            (t.codegen_errs(ctx, &mut errs), None),
                            (types::TypeData::new(), None),
                            ctx,
                        )
                        .map_or_else(
                            |e| {
                                errs.push(e);
                                types::Error::new()
                            },
                            |v| {
                                if let Some(InterData::Type(t)) = v.inter_val {
                                    *t
                                } else {
                                    types::Error::new()
                                }
                            },
                        );
                        ctx.is_const.set(oic);
                        t
                    }) {
                        t
                    } else {
                        t2.decay()
                    };
                    let mut val =
                        ops::impl_convert(self.val.loc(), (val, None), (dt.clone(), None), ctx)
                            .unwrap_or_else(|e| {
                                errs.push(e);
                                Value::error()
                            });
                    if let Some(t) = dt.llvm_type(ctx) {
                        let mangled =
                            linkas.map_or_else(|| ctx.mangle(&self.name).into(), |(name, _)| name);
                        let gv = ctx
                            .lookup_full(&self.name)
                            .and_then(|x| -> Option<GlobalValue> {
                                Some(unsafe { std::mem::transmute(x.comp_val?.as_value_ref()) })
                            })
                            .unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_constant(!self.is_mut);
                        gv.set_initializer(&t.const_zero());
                        if let Some((link, _)) = link_type {
                            gv.set_linkage(link)
                        }
                        ctx.restore_scope(old_scope);
                        if let Some(v) = val.value(ctx) {
                            val.inter_val = None;
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            hoist_allocas(&ctx.builder);
                            if let Some(bb) = old_ip {
                                ctx.builder.position_at_end(bb);
                            } else {
                                ctx.builder.clear_insertion_position();
                            }
                            {
                                let as0 = inkwell::AddressSpace::from(0u16);
                                let i32t = ctx.context.i32_type();
                                let i8tp = ctx.context.i8_type().ptr_type(as0);
                                let st = ctx.context.struct_type(
                                    &[
                                        i32t.into(),
                                        ctx.context
                                            .void_type()
                                            .fn_type(&[], false)
                                            .ptr_type(as0)
                                            .into(),
                                        i8tp.into(),
                                    ],
                                    false,
                                );
                                let g = ctx.module.add_global(
                                    st.array_type(1),
                                    None,
                                    "llvm.global_ctors",
                                );
                                g.set_linkage(inkwell::module::Linkage::Appending);
                                g.set_initializer(
                                    &st.const_array(&[st.const_named_struct(&[
                                        i32t.const_int(ctx.priority.decr().get() as u64, false)
                                            .into(),
                                        f.as_global_value().as_pointer_value().into(),
                                        i8tp.const_zero().into(),
                                    ])]),
                                );
                            }
                            ctx.with_vars(|v| {
                                v.insert(
                                    &self.name,
                                    Symbol(
                                        Value::new(
                                            Some(PointerValue(gv.as_pointer_value())),
                                            None,
                                            Type::Reference(ops::maybe_mut(
                                                Box::new(dt),
                                                self.is_mut,
                                            )),
                                        )
                                        .freeze(self.loc),
                                        VariableData::with_vis(self.loc, vs),
                                    ),
                                )
                            })
                        } else {
                            val.inter_val = None;
                            unsafe {
                                gv.delete();
                                f.delete();
                            }
                            if dt != types::Error::new() {
                                errs.push(CobaltError::TypeIsConstOnly {
                                    ty: dt.to_string(),
                                    loc: self.type_.as_ref().unwrap_or(&self.val).loc(),
                                })
                            }
                            ctx.with_vars(|v| {
                                v.insert(
                                    &self.name,
                                    Symbol(
                                        val.freeze(self.loc),
                                        VariableData::with_vis(self.loc, false),
                                    ),
                                )
                            })
                        }
                    } else {
                        let old_scope = ctx.push_scope(&self.name);
                        if dt != types::Error::new() {
                            errs.push(CobaltError::TypeIsConstOnly {
                                ty: dt.to_string(),
                                loc: self.type_.as_ref().unwrap_or(&self.val).loc(),
                            })
                        }
                        let val = self.val.codegen_errs(ctx, &mut errs);
                        let t2 = val.data_type.clone();
                        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                            let oic = ctx.is_const.replace(true);
                            let t = ops::impl_convert(
                                t.loc(),
                                (t.codegen_errs(ctx, &mut errs), None),
                                (types::TypeData::new(), None),
                                ctx,
                            )
                            .map_or(types::Error::new(), |v| {
                                if let Some(InterData::Type(t)) = v.inter_val {
                                    *t
                                } else {
                                    types::Error::new()
                                }
                            });
                            ctx.is_const.set(oic);
                            t
                        }) {
                            t
                        } else {
                            t2.decay()
                        };
                        let val = ops::impl_convert(self.val.loc(), (val, None), (dt, None), ctx)
                            .unwrap_or_else(|e| {
                                errs.push(e);
                                Value::error()
                            });
                        ctx.restore_scope(old_scope);
                        ctx.with_vars(|v| {
                            v.insert(
                                &self.name,
                                Symbol(val.freeze(self.loc), VariableData::with_vis(self.loc, vs)),
                            )
                        })
                    }
                };
                match res {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                        });
                        (Value::error(), errs)
                    }
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d,
                        });
                        (Value::error(), errs)
                    }
                }
            }
        } else {
            if let Some(loc) = is_extern {
                errs.push(CobaltError::MustBeGlobal {
                    name: "extern",
                    loc,
                });
            }
            if let Some((_, loc)) = link_type {
                errs.push(CobaltError::MustBeGlobal { name: "link", loc });
            }
            if let Some((_, loc)) = linkas {
                errs.push(CobaltError::MustBeGlobal {
                    name: "linkas",
                    loc,
                });
            }
            let old_scope = ctx.push_scope(&self.name);
            let val = self.val.codegen_errs(ctx, &mut errs);
            let t2 = val.data_type.clone();
            let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                let oic = ctx.is_const.replace(true);
                let t = ops::impl_convert(
                    t.loc(),
                    (t.codegen_errs(ctx, &mut errs), None),
                    (types::TypeData::new(), None),
                    ctx,
                )
                .map_or_else(
                    |e| {
                        errs.push(e);
                        types::Error::new()
                    },
                    |v| {
                        if let Some(InterData::Type(t)) = v.inter_val {
                            *t
                        } else {
                            types::Error::new()
                        }
                    },
                );
                ctx.is_const.set(oic);
                t
            }) {
                t
            } else {
                t2.decay()
            };
            let mut val = ops::impl_convert(self.val.loc(), (val, None), (dt.clone(), None), ctx)
                .unwrap_or_else(|e| {
                    errs.push(e);
                    Value::error()
                });
            ops::mark_move(
                &val,
                cfg::Location::current(ctx).unwrap(),
                ctx,
                self.val.loc(),
            );
            ctx.restore_scope(old_scope);
            val.comp_val = val.value(ctx);
            val.name = self
                .name
                .ids
                .get(0)
                .map(|x| (x.0.clone(), ctx.lex_scope.get()));
            val.frozen = (!self.is_mut).then_some(self.loc);
            match if ctx.is_const.get() || !self.is_mut {
                ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            val,
                            VariableData {
                                scope: ctx.var_scope.get().try_into().ok(),
                                ..VariableData::with_vis(self.loc, false)
                            },
                        ),
                    )
                })
            } else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = val.addr(ctx).unwrap_or_else(|| {
                    let a = ctx
                        .builder
                        .build_alloca(t, self.name.ids.last().map_or("", |(x, _)| &**x));
                    ctx.builder.build_store(a, v);
                    a
                });
                let mut val = Value::new(
                    Some(PointerValue(a)),
                    val.inter_val,
                    *ops::maybe_mut(Box::new(val.data_type), self.is_mut),
                );
                val.name = self
                    .name
                    .ids
                    .get(0)
                    .map(|x| (x.0.clone(), ctx.lex_scope.get()));
                ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(val, VariableData::with_vis(self.loc, false)),
                    )
                })
            } else {
                if dt != types::Error::new() {
                    errs.push(CobaltError::TypeIsConstOnly {
                        ty: dt.to_string(),
                        loc: self.type_.as_ref().unwrap_or(&self.val).loc(),
                    })
                }
                ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            val,
                            VariableData {
                                scope: ctx.var_scope.get().try_into().ok(),
                                ..VariableData::with_vis(self.loc, false)
                            },
                        ),
                    )
                })
            } {
                Ok(x) => (x.0.clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(CobaltError::NotAModule {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string(),
                    });
                    (Value::error(), errs)
                }
                Err(RedefVariable::AlreadyExists(x, d, _)) => {
                    errs.push(CobaltError::RedefVariable {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string(),
                        prev: d,
                    });
                    (Value::error(), errs)
                }
            }
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(
            f,
            "let{}: {}",
            if self.is_mut { " (mut)" } else { "" },
            self.name
        )?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(
                f,
                "{pre}{}@{name}{}",
                if n + 1 < self.annotations.len() {
                    "├── "
                } else {
                    "└── "
                },
                arg.as_ref().map(|x| format!("({x})")).unwrap_or_default()
            )?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {
            print_ast_child(f, pre, &**ast, false, file)?
        }
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct ConstDefAST<'src> {
    loc: SourceSpan,
    pub name: DottedName<'src>,
    pub val: BoxedAST<'src>,
    pub type_: Option<BoxedAST<'src>>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    lastmissing: CellExt<HashSet<Cow<'src, str>>>,
}
impl<'src> ConstDefAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: DottedName<'src>,
        val: BoxedAST<'src>,
        type_: Option<BoxedAST<'src>>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    ) -> Self {
        ConstDefAST {
            loc,
            name,
            val,
            type_,
            annotations,
            lastmissing: CellExt::default(),
        }
    }
}
impl<'src> AST<'src> for ConstDefAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.loc, self.val.loc())
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + self.type_.as_ref().map_or(0, |x| x.nodes()) + 1
    }
    fn varfwd_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut target_match = 2u8;
        for (ann, arg, _) in self.annotations.iter() {
            if ann == "target" {
                if let Some(arg) = arg {
                    let mut arg = &**arg;
                    let negate = if arg.as_bytes().first() == Some(&0x21) {
                        arg = &arg[1..];
                        true
                    } else {
                        false
                    };
                    if let Ok(pat) = Pattern::new(arg) {
                        if target_match != 1 {
                            target_match = u8::from(
                                negate
                                    ^ pat.matches(
                                        &ctx.module.get_triple().as_str().to_string_lossy(),
                                    ),
                            )
                        }
                    }
                }
            }
        }
        if target_match == 0 {
            return;
        }
        let _ = ctx.with_vars(|v| {
            v.insert(
                &self.name,
                Symbol(Value::error(), VariableData::uninit(self.loc)),
            )
        });
    }
    fn constinit_prepass(&self, ctx: &CompCtx<'src, '_>, needs_another: &mut bool) {
        let mut target_match = 2u8;
        for (ann, arg, _) in self.annotations.iter() {
            if ann == "target" {
                if let Some(arg) = arg {
                    let mut arg = &**arg;
                    let negate = if arg.as_bytes().first() == Some(&0x21) {
                        arg = &arg[1..];
                        true
                    } else {
                        false
                    };
                    if let Ok(pat) = Pattern::new(arg) {
                        if target_match != 1 {
                            target_match = u8::from(
                                negate
                                    ^ pat.matches(
                                        &ctx.module.get_triple().as_str().to_string_lossy(),
                                    ),
                            )
                        }
                    }
                }
            }
        }
        if target_match == 0 {
            return;
        }
        let mut missing = HashSet::new();
        let pp = ctx.prepass.replace(true);
        for err in self.codegen(ctx).1 {
            if let CobaltError::UninitializedGlobal { name, .. } = err {
                missing.insert(name);
            }
        }
        self.lastmissing.map(|v| {
            *needs_another |= !missing.is_empty() && (v.is_empty() || v.len() > missing.len());
            missing
        });
        ctx.prepass.set(pp);
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        match Pattern::new(arg) {
                            Ok(pat) => {
                                if target_match != 1 {
                                    target_match = u8::from(
                                        negate
                                            ^ pat.matches(
                                                &ctx.module.get_triple().as_str().to_string_lossy(),
                                            ),
                                    )
                                }
                            }
                            Err(err) => errs.push(CobaltError::GlobPatternError {
                                pos: err.pos,
                                msg: err.msg,
                                loc,
                            }),
                        }
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc,
                        });
                    }
                }
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((true, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((false, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "constant",
                }),
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {
            return (Value::null(), errs);
        }
        let old_is_const = ctx.is_const.replace(true);
        let old_scope = ctx.push_scope(&self.name);
        let val = self.val.codegen_errs(ctx, &mut errs);
        let t2 = val.data_type.clone();
        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
            let t = ops::impl_convert(
                t.loc(),
                (t.codegen_errs(ctx, &mut errs), None),
                (types::TypeData::new(), None),
                ctx,
            )
            .map_or_else(
                |e| {
                    errs.push(e);
                    types::Error::new()
                },
                |v| {
                    if let Some(InterData::Type(t)) = v.inter_val {
                        *t
                    } else {
                        types::Error::new()
                    }
                },
            );
            t
        }) {
            t
        } else {
            t2.decay()
        };
        let val =
            ops::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            });
        ctx.restore_scope(old_scope);
        ctx.is_const.set(old_is_const);
        match ctx.with_vars(|v| {
            v.insert(
                &self.name,
                Symbol(
                    val.freeze(self.loc),
                    VariableData {
                        fwd: ctx.prepass.get(),
                        init: !errs
                            .iter()
                            .any(|e| matches!(e, CobaltError::UninitializedGlobal { .. })),
                        ..VariableData::with_vis(self.loc, vs)
                    },
                ),
            )
        }) {
            Ok(x) => (x.0.clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(CobaltError::NotAModule {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                });
                (Value::error(), errs)
            }
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                errs.push(CobaltError::RedefVariable {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                    prev: d,
                });
                (Value::error(), errs)
            }
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "const: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(
                f,
                "{pre}{}@{name}{}",
                if n + 1 < self.annotations.len() {
                    "├── "
                } else {
                    "└── "
                },
                arg.as_ref().map(|x| format!("({x})")).unwrap_or_default()
            )?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {
            print_ast_child(f, pre, &**ast, false, file)?
        }
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct TypeDefAST<'src> {
    loc: SourceSpan,
    pub name: DottedName<'src>,
    pub val: BoxedAST<'src>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    pub methods: Vec<BoxedAST<'src>>,
    lastmissing: CellExt<HashSet<Cow<'src, str>>>,
}
impl<'src> TypeDefAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: DottedName<'src>,
        val: BoxedAST<'src>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
        methods: Vec<BoxedAST<'src>>,
    ) -> Self {
        TypeDefAST {
            loc,
            name,
            val,
            annotations,
            methods,
            lastmissing: CellExt::default(),
        }
    }
}
impl<'src> AST<'src> for TypeDefAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.val.nodes() + self.methods.iter().map(|x| x.nodes()).sum::<usize>() + 1
    }
    fn varfwd_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut target_match = 2u8;
        let mut no_auto_drop = true;
        let mut linear = false;
        let mut transparent = false;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "no_auto_drop" => no_auto_drop = true,
                "must_use" => linear = true,
                "transparent" => transparent = true,
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        let _ = ctx.with_vars(|v| {
            v.insert(
                &self.name,
                Symbol(Value::error(), VariableData::uninit(self.loc)),
            )
        });
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {
                vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| {
                    (
                        k,
                        Symbol(
                            v,
                            VariableData {
                                fwd: true,
                                ..Default::default()
                            },
                        ),
                    )
                }))
            }
            Box::new(vm)
        });
        let pp = ctx.prepass.replace(true);
        let old_scope = ctx.push_scope(&self.name);
        let mut mb = ctx.nom_info.borrow_mut();
        mb.push(Default::default());
        let info = mb.last_mut().unwrap();
        info.no_auto_drop = no_auto_drop;
        info.transparent = transparent;
        info.is_linear_type = linear;
        std::mem::drop(mb);
        ctx.with_vars(|v| {
            v.symbols.insert(
                "base_t".into(),
                Value::make_type(types::Null::new()).freeze(self.loc).into(),
            );
            v.symbols.insert(
                "self_t".into(),
                Value::make_type(Type::Nominal(mangled.clone()))
                    .freeze(self.loc)
                    .into(),
            );
        });
        {
            let mut noms = ctx.nominals.borrow_mut();
            if !noms.contains_key(&mangled) {
                noms.insert(
                    mangled.clone(),
                    (
                        types::Null::new(),
                        true,
                        Default::default(),
                        Default::default(),
                    ),
                );
            }
        }
        self.methods.iter().for_each(|a| a.varfwd_prepass(ctx));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| {
            (
                v.parent.unwrap(),
                v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect(),
            )
        });
        noms.get_mut(&mangled).unwrap().3 = ctx.nom_info.borrow_mut().pop().unwrap();
        ctx.prepass.set(pp);
    }
    fn constinit_prepass(&self, ctx: &CompCtx<'src, '_>, needs_another: &mut bool) {
        let mut target_match = 2u8;
        let mut no_auto_drop = true;
        let mut linear = false;
        let mut transparent = false;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "no_auto_drop" => no_auto_drop = true,
                "must_use" => linear = true,
                "transparent" => transparent = true,
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        let mut missing = HashSet::new();
        let pp = ctx.prepass.replace(true);
        for err in self.codegen(ctx).1 {
            if let CobaltError::UninitializedGlobal { name, .. } = err {
                missing.insert(name);
            }
        }
        self.lastmissing.map(|v| {
            *needs_another |= !missing.is_empty() && (v.is_empty() || v.len() > missing.len());
            missing
        });
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {
                vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| {
                    (
                        k,
                        Symbol(
                            v,
                            VariableData {
                                fwd: true,
                                ..Default::default()
                            },
                        ),
                    )
                }))
            }
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        let mut mb = ctx.nom_info.borrow_mut();
        mb.push(Default::default());
        let info = mb.last_mut().unwrap();
        info.no_auto_drop = no_auto_drop;
        info.transparent = transparent;
        info.is_linear_type = linear;
        std::mem::drop(mb);
        ctx.with_vars(|v| {
            v.symbols.insert(
                "base_t".into(),
                Value::make_type(types::Null::new()).freeze(self.loc).into(),
            );
            v.symbols.insert(
                "self_t".into(),
                Value::make_type(Type::Nominal(mangled.clone()))
                    .freeze(self.loc)
                    .into(),
            );
        });
        {
            let mut noms = ctx.nominals.borrow_mut();
            if !noms.contains_key(&mangled) {
                noms.insert(
                    mangled.clone(),
                    (
                        types::Null::new(),
                        true,
                        Default::default(),
                        Default::default(),
                    ),
                );
            }
        }
        self.methods
            .iter()
            .for_each(|a| a.constinit_prepass(ctx, needs_another));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| {
            (
                v.parent.unwrap(),
                v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect(),
            )
        });
        noms.get_mut(&mangled).unwrap().3 = ctx.nom_info.borrow_mut().pop().unwrap();
        ctx.prepass.set(pp);
    }
    fn fwddef_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut vis_spec = None;
        let mut target_match = 2u8;
        let mut no_auto_drop = true;
        let mut linear = false;
        let mut transparent = false;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            Some(_) => {}
                        }
                    }
                }
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            Some(_) => {}
                        }
                    }
                }
                "no_auto_drop" => no_auto_drop = true,
                "must_use" => linear = true,
                "transparent" => transparent = true,
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {
                vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| {
                    (
                        k,
                        Symbol(
                            v,
                            VariableData {
                                fwd: true,
                                ..Default::default()
                            },
                        ),
                    )
                }))
            }
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        let mut mb = ctx.nom_info.borrow_mut();
        mb.push(Default::default());
        let info = mb.last_mut().unwrap();
        info.no_auto_drop = no_auto_drop;
        info.transparent = transparent;
        info.is_linear_type = linear;
        std::mem::drop(mb);
        let ty = ops::impl_convert(
            unreachable_span(),
            (self.val.const_codegen(ctx).0, None),
            (types::TypeData::new(), None),
            ctx,
        )
        .ok()
        .and_then(Value::as_type)
        .unwrap_or(types::Error::new());
        ctx.with_vars(|v| {
            v.symbols.insert(
                "base_t".into(),
                Value::make_type(ty.clone()).freeze(self.loc).into(),
            );
            v.symbols.insert(
                "self_t".into(),
                Value::make_type(Type::Nominal(mangled.clone()))
                    .freeze(self.loc)
                    .into(),
            );
        });
        match ctx.nominals.borrow_mut().entry(mangled.clone()) {
            Entry::Occupied(mut x) => x.get_mut().0 = ty,
            Entry::Vacant(x) => {
                x.insert((ty, true, Default::default(), Default::default()));
            }
        }
        self.methods.iter().for_each(|a| a.fwddef_prepass(ctx));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| {
            (
                v.parent.unwrap(),
                v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect(),
            )
        });
        noms.get_mut(&mangled).unwrap().3 = ctx.nom_info.borrow_mut().pop().unwrap();
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        let mut no_auto_drop = None;
        let mut linear = None;
        let mut transparent = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        match Pattern::new(arg) {
                            Ok(pat) => {
                                if target_match != 1 {
                                    target_match = u8::from(
                                        negate
                                            ^ pat.matches(
                                                &ctx.module.get_triple().as_str().to_string_lossy(),
                                            ),
                                    )
                                }
                            }
                            Err(err) => errs.push(CobaltError::GlobPatternError {
                                pos: err.pos,
                                msg: err.msg,
                                loc,
                            }),
                        }
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc,
                        });
                    }
                }
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((true, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((false, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "no_auto_drop" => {
                    if let Some(prev) = no_auto_drop {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "no_auto_drop",
                            loc,
                            prev,
                        });
                    } else {
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "no_auto_drop",
                                found: arg.clone(),
                                expected: None,
                                loc,
                            })
                        }
                        no_auto_drop = Some(loc);
                    }
                }
                "must_use" => {
                    if let Some(prev) = no_auto_drop {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "must_use",
                            loc,
                            prev,
                        });
                    } else {
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "must_use",
                                found: arg.clone(),
                                expected: None,
                                loc,
                            })
                        }
                        linear = Some(loc);
                    }
                }
                "transparent" => {
                    if let Some(prev) = transparent {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "transparent",
                            loc,
                            prev,
                        });
                    } else {
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "transparent",
                                found: arg.clone(),
                                expected: None,
                                loc,
                            })
                        }
                        transparent = Some(loc);
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "type",
                }),
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {
            return (Value::null(), errs);
        }
        let ty = ops::impl_convert(
            self.val.loc(),
            (self.val.codegen_errs(ctx, &mut errs), None),
            (types::TypeData::new(), None),
            ctx,
        )
        .map_or_else(
            |e| {
                errs.push(e);
                types::Error::new()
            },
            |v| {
                if let Some(InterData::Type(t)) = v.inter_val {
                    *t
                } else {
                    types::Error::new()
                }
            },
        );
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {
                vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| {
                    (
                        k,
                        Symbol(
                            v,
                            VariableData {
                                fwd: true,
                                ..Default::default()
                            },
                        ),
                    )
                }))
            }
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        let mut mb = ctx.nom_info.borrow_mut();
        mb.push(Default::default());
        let info = mb.last_mut().unwrap();
        info.no_auto_drop = no_auto_drop.is_some();
        info.transparent = transparent.is_some();
        info.is_linear_type = linear.is_some();
        std::mem::drop(mb);
        ctx.with_vars(|v| {
            v.symbols.insert(
                "base_t".into(),
                Value::make_type(ty.clone()).freeze(self.loc).into(),
            );
            v.symbols.insert(
                "self_t".into(),
                Value::make_type(Type::Nominal(mangled.clone()))
                    .freeze(self.loc)
                    .into(),
            );
        });
        match ctx.nominals.borrow_mut().entry(mangled.clone()) {
            Entry::Occupied(mut x) => x.get_mut().0 = ty,
            Entry::Vacant(x) => {
                x.insert((ty, true, Default::default(), Default::default()));
            }
        }
        if !ctx.prepass.get() {
            self.methods.iter().for_each(|a| {
                a.codegen_errs(ctx, &mut errs);
            });
        }
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| {
            (
                v.parent.unwrap(),
                v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect(),
            )
        });
        noms.get_mut(&mangled).unwrap().3 = ctx.nom_info.borrow_mut().pop().unwrap();
        match ctx.with_vars(|v| {
            v.insert(
                &self.name,
                Symbol(
                    Value::make_type(Type::Nominal(mangled.clone())).freeze(self.loc),
                    VariableData {
                        fwd: ctx.prepass.get(),
                        init: !errs
                            .iter()
                            .any(|e| matches!(e, CobaltError::UninitializedGlobal { .. })),
                        ..VariableData::with_vis(self.loc, vs)
                    },
                ),
            )
        }) {
            Ok(x) => (x.0.clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                noms.remove(&mangled);
                errs.push(CobaltError::NotAModule {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                });
                (Value::error(), errs)
            }
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                noms.remove(&mangled);
                errs.push(CobaltError::RedefVariable {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                    prev: d,
                });
                (Value::error(), errs)
            }
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "type: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(
                f,
                "{pre}{}@{name}{}",
                if n + 1 < self.annotations.len() {
                    "├── "
                } else {
                    "└── "
                },
                arg.as_ref().map(|x| format!("({x})")).unwrap_or_default()
            )?;
        }
        pre.pop();
        print_ast_child(f, pre, &*self.val, self.methods.is_empty(), file)?;
        if !self.methods.is_empty() {
            writeln!(f, "{pre}└── statics:")?;
            pre.push(true);
            let mut count = self.methods.len();
            for m in self.methods.iter() {
                print_ast_child(f, pre, &**m, count == 1, file)?;
                count -= 1;
            }
            pre.pop();
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct VarGetAST<'src> {
    loc: SourceSpan,
    pub name: Cow<'src, str>,
    pub global: bool,
}
impl<'src> VarGetAST<'src> {
    pub fn new(loc: SourceSpan, name: Cow<'src, str>, global: bool) -> Self {
        VarGetAST { loc, name, global }
    }
}
impl<'src> AST<'src> for VarGetAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match ctx.lookup(&self.name, self.global) {
            Some(Symbol(x, d)) if d.scope.map_or(true, |x| x.get() == ctx.var_scope.get()) => (
                x.clone(),
                if d.init {
                    vec![]
                } else {
                    vec![CobaltError::UninitializedGlobal {
                        name: self.name.clone(),
                        loc: self.loc,
                    }]
                },
            ),
            _ => (
                Value::error(),
                vec![CobaltError::VariableDoesNotExist {
                    name: self.name.clone(),
                    module: Default::default(),
                    container: "",
                    loc: self.loc,
                }],
            ),
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(
            f,
            "var: {}{}",
            if self.global { "." } else { "" },
            self.name
        )
    }
}
