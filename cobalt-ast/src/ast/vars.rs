use crate::*;
use inkwell::values::{AsValueRef, GlobalValue, BasicValueEnum::*};
use inkwell::module::Linkage::*;
use glob::Pattern;
use std::collections::{HashSet, hash_map::Entry};
#[derive(Debug, Clone)]
pub struct VarDefAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>,
    pub global: bool
}
impl VarDefAST {
    pub fn new(loc: SourceSpan, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, SourceSpan)>, global: bool) -> Self {VarDefAST {loc, name, val, type_, annotations, global}}
}
impl AST for VarDefAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.loc, self.val.loc())}
    fn nodes(&self) -> usize {self.val.nodes() + self.type_.as_ref().map_or(0, |x| x.nodes()) + 1}
    fn fwddef_prepass(&self, ctx: &CompCtx) {
        let mut errs = vec![];
        let mut link_type = None;
        let mut linkas = None;
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "link" => {
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => None
                    }.map(|x| (x, loc))
                },
                "linkas" => {
                    if let Some(arg) = arg {
                        linkas = Some(arg.clone())
                    }
                },
                "c" | "C" => linkas = Some(self.name.ids.last().expect("function name shouldn't be empty!").0.clone()),
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        if let Ok(pat) = Pattern::new(arg) {
                            target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))
                        }
                    }
                },
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            _ => {},
                        }
                    }
                },
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            _ => {}
                        }
                    }
                },
                _ => {}
            }
        }
        let vs = vis_spec.unwrap_or(ctx.export.get());
        if target_match == 0 {return}
        let t2 = self.val.const_codegen(ctx).0.data_type;
        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
            let oic = ctx.is_const.replace(true);
            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error);
            ctx.is_const.set(oic);
            t
        }) {t} else {
            match t2 {
                Type::IntLiteral => Type::Int(64, false),
                Type::Reference(b, m) => match *b {
                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                    x => x
                },
                x => x
            }
        };
        let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
            dt.llvm_type(ctx).map(|t| {
                let gv = ctx.module.add_global(t, None, &linkas.unwrap_or_else(|| ctx.mangle(&self.name)));
                match link_type {
                    None => {},
                    Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                    Some((x, _)) => gv.set_linkage(x)
                }
                PointerValue(gv.as_pointer_value())
            }),
            None,
            Type::Reference(Box::new(dt), false)
        ), VariableData {fwd: true, ..VariableData::with_vis(self.loc, vs)})));
    }
    fn res_type(&self, ctx: &CompCtx) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut vis_spec = None;
        let mut stack = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "static" => {
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "static",
                            expected: None,
                            found: arg.clone(),
                            loc
                        });
                    }
                    is_static = true;
                },
                "link" => {
                    if let Some((_, prev)) = link_type {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "link",
                            loc, prev
                        });
                    }
                    link_type = match arg.as_deref() {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "link",
                                found: arg.clone(),
                                expected: Some("link type"),
                                loc
                            });
                            None
                        }
                    }.map(|x| (x, loc))
                },
                "linkas" => {
                    if let Some((_, prev)) = linkas {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "linkas",
                            loc, prev
                        });
                    }
                    if let Some(arg) = arg {linkas = Some((arg.clone(), loc))}
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "linkas",
                            found: arg.clone(),
                            expected: Some("linkage name"),
                            loc
                        });
                    }
                },
                "extern" => {
                    is_extern = Some(loc);
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "extern",
                            found: arg.clone(),
                            expected: None,
                            loc
                        });
                    }
                },
                "c" | "C" => {
                    match arg.as_ref().map(|x| x.as_str()) {
                        Some("") | None => {},
                        Some("extern") => is_extern = Some(loc),
                        Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                            name: "C",
                            found: arg.clone(),
                            expected: Some(r#"no argument or "extern""#),
                            loc
                        })
                    }
                    linkas = Some((self.name.ids.last().expect("variable name shouldn't be empty!").0.clone(), loc))
                },
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(CobaltError::GlobPatternError {pos: err.pos, msg: err.msg.to_string(), loc})
                        }
                    }
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc
                        });
                    }
                },
                "export" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "export",
                            loc
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                "private" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "private",
                            loc
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                "stack" => {
                    if let Some(prev) = stack {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "stack",
                            loc, prev
                        });
                    }
                    else {
                        stack = Some(loc);
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "stack",
                                found: arg.clone(),
                                expected: None,
                                loc
                            })
                        }
                    }
                },
                _ => errs.push(CobaltError::UnknownAnnotation {loc, name: ann.clone(), def: "variable"})
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        if self.global || is_static {
            if let Some(loc) = stack {
                errs.push(CobaltError::MustBeLocal {name: "stack", loc});
            }
            if is_extern.is_some() {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t2 {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    dt.llvm_type(ctx).map(|t| {
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}; None}),
                    None,
                    Type::Reference(Box::new(dt), false)
                ), VariableData::with_vis(self.loc, vs)))) {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let mut val = self.val.codegen_errs(ctx, &mut errs);
                let t2 = val.data_type.clone();
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t2 {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match if let Some(v) = val.value(ctx) {
                    val.inter_val = None;
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                            Some(PointerValue(gv.as_pointer_value())),
                            None,
                            Type::Reference(Box::new(dt), false)
                        ), VariableData::with_vis(self.loc, vs))))
                    }
                }
                else {
                    val.inter_val = None;
                    if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, false))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match if let Some(t) = dt.llvm_type(ctx) {
                    let old_global = ctx.global.replace(true);
                    let val = if ctx.is_const.get() {
                        let val = self.val.codegen_errs(ctx, &mut errs);
                        let t2 = val.data_type.clone();
                        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                            let oic = ctx.is_const.replace(true);
                            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or(Type::Error, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                            ctx.is_const.set(oic);
                            t
                        }) {t} else {
                            match t2 {
                                Type::IntLiteral => Type::Int(64, false),
                                Type::Reference(b, m) => match *b {
                                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                    x => x
                                },
                                x => x
                            }
                        };
                        let mut val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        val.inter_val = None;
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                    }
                    else {
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_constant(false);
                        gv.set_initializer(&t.const_zero());
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("cobalt.init{}", ctx.mangle(&self.name)).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        {
                            let as0 = inkwell::AddressSpace::from(0u16);
                            let i32t = ctx.context.i32_type();
                            let i8tp = ctx.context.i8_type().ptr_type(as0);
                            let st = ctx.context.struct_type(&[i32t.into(), ctx.context.void_type().fn_type(&[], false).ptr_type(as0).into(), i8tp.into()], false);
                            let g = ctx.module.add_global(st.array_type(1), None, "llvm.global_ctors");
                            g.set_linkage(inkwell::module::Linkage::Appending);
                            g.set_initializer(&st.const_array(&[st.const_named_struct(&[i32t.const_int(ctx.priority.decr().get() as u64, false).into(), f.as_global_value().as_pointer_value().into(), i8tp.const_zero().into()])]));
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        let val = self.val.codegen_errs(ctx, &mut errs);
                        let t2 = val.data_type.clone();
                        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                            let oic = ctx.is_const.replace(true);
                            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                            ctx.is_const.set(oic);
                            t
                        }) {t} else {
                            match t2 {
                                Type::IntLiteral => Type::Int(64, false),
                                Type::Reference(b, m) => match *b {
                                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                    x => x
                                },
                                x => x
                            }
                        };
                        let mut val = types::utils::impl_convert(self.val.loc(), (val, None), (dt.clone(), None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        ctx.restore_scope(old_scope);
                        if let Some(v) = val.value(ctx) {
                            val.inter_val = None;
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                None,
                                Type::Reference(Box::new(dt), false)
                            ), VariableData::with_vis(self.loc, vs))))
                        }
                        else {
                            val.inter_val = None;
                            unsafe {
                                gv.delete();
                                f.delete();
                            }
                            if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, false))))
                        }
                    };
                    ctx.global.set(old_global);
                    val
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                    let val = self.val.codegen_errs(ctx, &mut errs);
                    let t2 = val.data_type.clone();
                    let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                        let oic = ctx.is_const.replace(true);
                        let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or(Type::Error, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                        ctx.is_const.set(oic);
                        t
                    }) {t} else {
                        match t2 {
                            Type::IntLiteral => Type::Int(64, false),
                            Type::Reference(b, m) => match *b {
                                x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                x => x
                            },
                            x => x
                        }
                    };
                    let val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error()
                    });
                    ctx.restore_scope(old_scope);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                } 
            }
        }
        else {
            if let Some(loc) = is_extern {
                errs.push(CobaltError::MustBeLocal {name: "extern", loc});
            }
            if let Some((_, loc)) = link_type {
                errs.push(CobaltError::MustBeLocal {name: "link", loc});
            }
            if let Some((_, loc)) = linkas {
                errs.push(CobaltError::MustBeLocal {name: "linkas", loc});
            }
            let old_scope = ctx.push_scope(&self.name);
            let val = self.val.codegen_errs(ctx, &mut errs);
            let t2 = val.data_type.clone();
            let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                let oic = ctx.is_const.replace(true);
                let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                ctx.is_const.set(oic);
                t
            }) {t} else {
                match t2 {
                    Type::IntLiteral => Type::Int(64, false),
                    Type::Reference(b, m) => match *b {
                        x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                        x => x
                    },
                    x => x
                }
            };
            let val = types::utils::impl_convert(self.val.loc(), (val, None), (dt.clone(), None), ctx).unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            });
            ctx.restore_scope(old_scope);
            match if ctx.is_const.get() || stack.is_none() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData {scope: ctx.var_scope.get().try_into().ok(), ..VariableData::with_vis(self.loc, false)})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.value(ctx)) {
                let a = val.addr(ctx).unwrap_or_else(|| {
                    let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                    ctx.builder.build_store(a, v);
                    a
                });
                ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    Some(PointerValue(a)),
                    val.inter_val,
                    Type::Reference(Box::new(val.data_type), false)
                ), VariableData::with_vis(self.loc, false))))
            }
            else {
                if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData {scope: ctx.var_scope.get().try_into().ok(), ..VariableData::with_vis(self.loc, false)})))
            } {
                Ok(x) => (x.0.clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(CobaltError::NotAModule {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string()
                    });
                    (Value::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, d, _)) => {
                    errs.push(CobaltError::RedefVariable {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string(),
                        prev: d
                    });
                    (Value::error(), errs)
                }
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or_default().as_str() + " ")) {out += s.as_str();}
        out + format!("let {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {}", t.to_code())), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "let: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false, file)?}
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct MutDefAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>,
    pub global: bool
}
impl MutDefAST {
    pub fn new(loc: SourceSpan, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, SourceSpan)>, global: bool) -> Self {MutDefAST {loc, name, val, type_, annotations, global}}
}
impl AST for MutDefAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.loc, self.val.loc())}
    fn nodes(&self) -> usize {self.val.nodes() + self.type_.as_ref().map_or(0, |x| x.nodes()) + 1}
    fn fwddef_prepass(&self, ctx: &CompCtx) {
        let mut errs = vec![];
        let mut link_type = None;
        let mut linkas = None;
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "link" => {
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => None
                    }.map(|x| (x, loc))
                },
                "linkas" => {
                    if let Some(arg) = arg {
                        linkas = Some(arg.clone())
                    }
                },
                "c" | "C" => linkas = Some(self.name.ids.last().expect("function name shouldn't be empty!").0.clone()),
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        if let Ok(pat) = Pattern::new(arg) {
                            target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))
                        }
                    }
                },
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            _ => {},
                        }
                    }
                },
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            _ => {}
                        }
                    }
                },
                _ => {}
            }
        }
        let vs = vis_spec.unwrap_or(ctx.export.get());
        if target_match == 0 {return}
        let t2 = self.val.const_codegen(ctx).0.data_type;
        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
            let oic = ctx.is_const.replace(true);
            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error);
            ctx.is_const.set(oic);
            t
        }) {t} else {
            match t2 {
                Type::IntLiteral => Type::Int(64, false),
                Type::Reference(b, m) => match *b {
                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                    x => x
                },
                x => x
            }
        };
        let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
            dt.llvm_type(ctx).map(|t| {
                let gv = ctx.module.add_global(t, None, &linkas.unwrap_or_else(|| ctx.mangle(&self.name)));
                match link_type {
                    None => {},
                    Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                    Some((x, _)) => gv.set_linkage(x)
                }
                PointerValue(gv.as_pointer_value())
            }),
            None,
            Type::Reference(Box::new(dt), false)
        ), VariableData {fwd: true, ..VariableData::with_vis(self.loc, vs)})));
    }
    fn res_type(&self, ctx: &CompCtx) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "static" => {
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "static",
                            expected: None,
                            found: arg.clone(),
                            loc
                        });
                    }
                    is_static = true;
                },
                "link" => {
                    if let Some((_, prev)) = link_type {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "link",
                            loc, prev
                        });
                    }
                    link_type = match arg.as_deref() {
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        _ => {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "link",
                                found: arg.clone(),
                                expected: Some("link type"),
                                loc
                            });
                            None
                        }
                    }.map(|x| (x, loc))
                },
                "linkas" => {
                    if let Some((_, prev)) = linkas {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "linkas",
                            loc, prev
                        });
                    }
                    if let Some(arg) = arg {linkas = Some((arg.clone(), loc))}
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "linkas",
                            found: arg.clone(),
                            expected: Some("linkage name"),
                            loc
                        });
                    }
                },
                "extern" => {
                    is_extern = Some(loc);
                    if arg.is_some() {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "extern",
                            found: arg.clone(),
                            expected: None,
                            loc
                        });
                    }
                },
                "c" | "C" => {
                    match arg.as_ref().map(|x| x.as_str()) {
                        Some("") | None => {},
                        Some("extern") => is_extern = Some(loc),
                        Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                            name: "C",
                            found: arg.clone(),
                            expected: Some(r#"no argument or "extern""#),
                            loc
                        })
                    }
                    linkas = Some((self.name.ids.last().expect("variable name shouldn't be empty!").0.clone(), loc))
                },
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(CobaltError::GlobPatternError {pos: err.pos, msg: err.msg.to_string(), loc})
                        }
                    }
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc
                        });
                    }
                },
                "export" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "export",
                            loc
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                "private" => {
                    if !self.global {
                        errs.push(CobaltError::MustBeGlobal {
                            name: "private",
                            loc
                        });
                    }
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                _ => errs.push(CobaltError::UnknownAnnotation {loc, name: ann.clone(), def: "variable"})
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        if self.global || is_static {
            if is_extern.is_some() {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t2 {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    dt.llvm_type(ctx).map(|t| {
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}; None}),
                    None,
                    Type::Reference(Box::new(dt), false)
                ), VariableData::with_vis(self.loc, vs)))) {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let mut val = self.val.codegen_errs(ctx, &mut errs);
                let t2 = val.data_type.clone();
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t2 {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match if let Some(v) = val.value(ctx) {
                    val.inter_val = None;
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_constant(false);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                            Some(PointerValue(gv.as_pointer_value())),
                            None,
                            Type::Reference(Box::new(dt), false)
                        ), VariableData::with_vis(self.loc, vs))))
                    }
                }
                else {
                    val.inter_val = None;
                    if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, false))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                    let oic = ctx.is_const.replace(true);
                    let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                    ctx.is_const.set(oic);
                    t
                }) {t} else {
                    match t {
                        Type::IntLiteral => Type::Int(64, false),
                        Type::Reference(b, m) => match *b {
                            x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                            x => x
                        },
                        x => x
                    }
                };
                match if let Some(t) = dt.llvm_type(ctx) {
                    let old_global = ctx.global.replace(true);
                    let val = if ctx.is_const.get() {
                        let val = self.val.codegen_errs(ctx, &mut errs);
                        let t2 = val.data_type.clone();
                        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                            let oic = ctx.is_const.replace(true);
                            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or(Type::Error, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                            ctx.is_const.set(oic);
                            t
                        }) {t} else {
                            match t2 {
                                Type::IntLiteral => Type::Int(64, false),
                                Type::Reference(b, m) => match *b {
                                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                    x => x
                                },
                                x => x
                            }
                        };
                        let mut val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        val.inter_val = None;
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                    }
                    else {
                        let mangled = linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name);
                        let gv = ctx.lookup_full(&self.name).and_then(|x| -> Option<GlobalValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_global(t, None, &mangled));
                        gv.set_constant(false);
                        gv.set_initializer(&t.const_zero());
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("cobalt.init{}", ctx.mangle(&self.name)).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        {
                            let as0 = inkwell::AddressSpace::from(0u16);
                            let i32t = ctx.context.i32_type();
                            let i8tp = ctx.context.i8_type().ptr_type(as0);
                            let st = ctx.context.struct_type(&[i32t.into(), ctx.context.void_type().fn_type(&[], false).ptr_type(as0).into(), i8tp.into()], false);
                            let g = ctx.module.add_global(st.array_type(1), None, "llvm.global_ctors");
                            g.set_linkage(inkwell::module::Linkage::Appending);
                            g.set_initializer(&st.const_array(&[st.const_named_struct(&[i32t.const_int(ctx.priority.decr().get() as u64, false).into(), f.as_global_value().as_pointer_value().into(), i8tp.const_zero().into()])]));
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        let val = self.val.codegen_errs(ctx, &mut errs);
                        let t2 = val.data_type.clone();
                        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                            let oic = ctx.is_const.replace(true);
                            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                            ctx.is_const.set(oic);
                            t
                        }) {t} else {
                            match t2 {
                                Type::IntLiteral => Type::Int(64, false),
                                Type::Reference(b, m) => match *b {
                                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                    x => x
                                },
                                x => x
                            }
                        };
                        let mut val = types::utils::impl_convert(self.val.loc(), (val, None), (dt.clone(), None), ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            Value::error()
                        });
                        ctx.restore_scope(old_scope);
                        if let Some(v) = val.value(ctx) {
                            val.inter_val = None;
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                None,
                                Type::Reference(Box::new(dt), false)
                            ), VariableData::with_vis(self.loc, vs))))
                        }
                        else {
                            val.inter_val = None;
                            unsafe {
                                gv.delete();
                                f.delete();
                            }
                            if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, false))))
                        }
                    };
                    ctx.global.set(old_global);
                    val
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    if dt != Type::Error {errs.push(CobaltError::TypeIsConstOnly {ty: dt.to_string(), loc: self.type_.as_ref().unwrap_or(&self.val).loc()})}
                    let val = self.val.codegen_errs(ctx, &mut errs);
                    let t2 = val.data_type.clone();
                    let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                        let oic = ctx.is_const.replace(true);
                        let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or(Type::Error, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                        ctx.is_const.set(oic);
                        t
                    }) {t} else {
                        match t2 {
                            Type::IntLiteral => Type::Int(64, false),
                            Type::Reference(b, m) => match *b {
                                x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                                x => x
                            },
                            x => x
                        }
                    };
                    let val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                        errs.push(e);
                        Value::error()
                    });
                    ctx.restore_scope(old_scope);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc, vs))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(CobaltError::NotAModule {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string()
                        });
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        errs.push(CobaltError::RedefVariable {
                            loc: self.name.ids[x].1,
                            name: self.name.start(x).to_string(),
                            prev: d
                        });
                        (Value::error(), errs)
                    }
                }
            }
        }
        else {
            if let Some(loc) = is_extern {
                errs.push(CobaltError::MustBeLocal {name: "extern", loc});
            }
            if let Some((_, loc)) = link_type {
                errs.push(CobaltError::MustBeLocal {name: "link", loc});
            }
            if let Some((_, loc)) = linkas {
                errs.push(CobaltError::MustBeLocal {name: "linkas", loc});
            }
            let old_scope = ctx.push_scope(&self.name);
            let val = self.val.codegen_errs(ctx, &mut errs);
            let t2 = val.data_type.clone();
            let dt = if let Some(t) = self.type_.as_ref().map(|t| {
                let oic = ctx.is_const.replace(true);
                let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
                ctx.is_const.set(oic);
                t
            }) {t} else {
                match t2 {
                    Type::IntLiteral => Type::Int(64, false),
                    Type::Reference(b, m) => match *b {
                        x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                        x => x
                    },
                    x => x
                }
            };
            let val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
                errs.push(e);
                Value::error()
            });
            ctx.restore_scope(old_scope);
            match if ctx.is_const.get() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData {scope: ctx.var_scope.get().try_into().ok(), ..VariableData::with_vis(self.loc, false)})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.value(ctx)) {
                let a = val.addr(ctx).unwrap_or_else(|| {
                    let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                    ctx.builder.build_store(a, v);
                    a
                });
                ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    Some(PointerValue(a)),
                    val.inter_val,
                    Type::Reference(Box::new(val.data_type), true)
                ), VariableData {scope: ctx.var_scope.get().try_into().ok(), ..VariableData::with_vis(self.loc, false)})))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData {scope: ctx.var_scope.get().try_into().ok(), ..VariableData::with_vis(self.loc, false)})))
            } {
                Ok(x) => (x.0.clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(CobaltError::NotAModule {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string()
                    });
                    (Value::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, d, _)) => {
                    errs.push(CobaltError::RedefVariable {
                        loc: self.name.ids[x].1,
                        name: self.name.start(x).to_string(),
                        prev: d
                    });
                    (Value::error(), errs)
                }
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or_default().as_str() + " ")) {out += s.as_str();}
        out + format!("mut {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "mut: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false, file)?}
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct ConstDefAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>,
    lastmissing: CellExt<HashSet<String>>
}
impl ConstDefAST {
    pub fn new(loc: SourceSpan, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, SourceSpan)>) -> Self {ConstDefAST {loc, name, val, type_, annotations, lastmissing: CellExt::default()}}
}
impl AST for ConstDefAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.loc, self.val.loc())}
    fn nodes(&self) -> usize {self.val.nodes() + self.type_.as_ref().map_or(0, |x| x.nodes()) + 1}
    fn res_type(&self, ctx: &CompCtx) -> Type {self.val.res_type(ctx)}
    fn varfwd_prepass(&self, ctx: &CompCtx) {let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::error(), VariableData::uninit(self.loc))));}
    fn constinit_prepass(&self, ctx: &CompCtx, needs_another: &mut bool) {
        let mut missing = HashSet::new();
        let pp = ctx.prepass.replace(true);
        for err in self.codegen(ctx).1 {
            if let CobaltError::UninitializedGlobal {name, ..} = err {
                missing.insert(name);
            }
        }
        self.lastmissing.map(|v| {
            *needs_another |= !missing.is_empty() && (v.is_empty() || v.len() > missing.len());
            missing
        });
        ctx.prepass.set(pp);
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(CobaltError::GlobPatternError {pos: err.pos, msg: err.msg.to_string(), loc})
                        }
                    }
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc
                        });
                    }
                },
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                _ => errs.push(CobaltError::UnknownAnnotation {loc, name: ann.clone(), def: "constant"})
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        let old_is_const = ctx.is_const.replace(true);
        let old_scope = ctx.push_scope(&self.name);
        let val = self.val.codegen_errs(ctx, &mut errs);
        let t2 = val.data_type.clone();
        let dt = if let Some(t) = self.type_.as_ref().map(|t| {
            let t = types::utils::impl_convert(t.loc(), (t.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
            t
        }) {t} else {
            match t2 {
                Type::IntLiteral => Type::Int(64, false),
                Type::Reference(b, m) => match *b {
                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                    x => x
                },
                x => x
            }
        };
        let val = types::utils::impl_convert(self.val.loc(), (val, None), (dt, None), ctx).unwrap_or_else(|e| {
            errs.push(e);
            Value::error()
        });
        ctx.restore_scope(old_scope);
        ctx.is_const.set(old_is_const);
        match ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData {fwd: ctx.prepass.get(), init: errs.iter().any(|e| matches!(e, CobaltError::UninitializedGlobal {..})), ..VariableData::with_vis(self.loc, vs)}))) {
            Ok(x) => (x.0.clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(CobaltError::NotAModule {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string()
                });
                (Value::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                errs.push(CobaltError::RedefVariable {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                    prev: d
                });
                (Value::error(), errs)
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or_default().as_str() + " ")) {out += s.as_str();}
        out + format!("const {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "const: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false, file)?}
        print_ast_child(f, pre, &*self.val, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct TypeDefAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>,
    pub methods: Vec<Box<dyn AST>>,
    lastmissing: CellExt<HashSet<String>>
}
impl TypeDefAST {
    pub fn new(loc: SourceSpan, name: DottedName, val: Box<dyn AST>, annotations: Vec<(String, Option<String>, SourceSpan)>, methods: Vec<Box<dyn AST>>) -> Self {TypeDefAST {loc, name, val, annotations, methods, lastmissing: CellExt::default()}}
}
impl AST for TypeDefAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn nodes(&self) -> usize {self.val.nodes() + self.methods.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::TypeData}
    fn varfwd_prepass(&self, ctx: &CompCtx) {
        let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::error(), VariableData::uninit(self.loc))));
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| (k, Symbol(v, VariableData {fwd: true, ..Default::default()}))))}
            Box::new(vm)
        });
        let pp = ctx.prepass.replace(true);
        let old_scope = ctx.push_scope(&self.name);
        ctx.with_vars(|v| {
            v.symbols.insert("base_t".to_string(), Value::make_type(Type::Null.clone()).into());
            v.symbols.insert("self_t".to_string(), Value::make_type(Type::Nominal(mangled.clone())).into());
        });
        {
            let mut noms = ctx.nominals.borrow_mut();
            if !noms.contains_key(&mangled) {noms.insert(mangled.clone(), (Type::Null, true, Default::default()));}
        }
        self.methods.iter().for_each(|a| a.varfwd_prepass(ctx));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| (v.parent.unwrap(), v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect()));
        ctx.prepass.set(pp);
    }
    fn constinit_prepass(&self, ctx: &CompCtx, needs_another: &mut bool) {
        let mut missing = HashSet::new();
        let pp = ctx.prepass.replace(true);
        for err in self.codegen(ctx).1 {
            if let CobaltError::UninitializedGlobal {name, ..} = err {
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
            if let Some(data) = noms.get_mut(&mangled) {vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| (k, Symbol(v, VariableData {fwd: true, ..Default::default()}))))}
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        ctx.with_vars(|v| {
            v.symbols.insert("base_t".to_string(), Value::make_type(Type::Null.clone()).into());
            v.symbols.insert("self_t".to_string(), Value::make_type(Type::Nominal(mangled.clone())).into());
        });
        {
            let mut noms = ctx.nominals.borrow_mut();
            if !noms.contains_key(&mangled) {noms.insert(mangled.clone(), (Type::Null, true, Default::default()));}
        }
        self.methods.iter().for_each(|a| a.constinit_prepass(ctx, needs_another));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| (v.parent.unwrap(), v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect()));
        ctx.prepass.set(pp);
    }
    fn fwddef_prepass(&self, ctx: &CompCtx) {
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| (k, Symbol(v, VariableData {fwd: true, ..Default::default()}))))}
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        let ty = types::utils::impl_convert(unreachable_span(), (self.val.const_codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error);
        ctx.with_vars(|v| {
            v.symbols.insert("base_t".to_string(), Value::make_type(ty.clone()).into());
            v.symbols.insert("self_t".to_string(), Value::make_type(Type::Nominal(mangled.clone())).into());
        });
        match ctx.nominals.borrow_mut().entry(mangled.clone()) {
            Entry::Occupied(mut x) => x.get_mut().0 = ty,
            Entry::Vacant(x) => {x.insert((ty, true, Default::default()));}
        }
        self.methods.iter().for_each(|a| a.fwddef_prepass(ctx));
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| (v.parent.unwrap(), v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect()));
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(CobaltError::GlobPatternError {pos: err.pos, msg: err.msg.to_string(), loc})
                        }
                    }
                    else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc
                        });
                    }
                },
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc)),
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc, prev
                        });
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc)),
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc
                            })
                        }
                    }
                },
                _ => errs.push(CobaltError::UnknownAnnotation {loc, name: ann.clone(), def: "type"})
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        let ty = types::utils::impl_convert(self.val.loc(), (self.val.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        let mangled = ctx.format(&self.name);
        ctx.map_vars(|v| {
            let mut vm = VarMap::new(Some(v));
            let mut noms = ctx.nominals.borrow_mut();
            if let Some(data) = noms.get_mut(&mangled) {vm.symbols.extend(data.2.clone().into_iter().map(|(k, v)| (k, Symbol(v, VariableData {fwd: true, ..Default::default()}))))}
            Box::new(vm)
        });
        let old_scope = ctx.push_scope(&self.name);
        ctx.with_vars(|v| {
            v.symbols.insert("base_t".to_string(), Value::make_type(ty.clone()).into());
            v.symbols.insert("self_t".to_string(), Value::make_type(Type::Nominal(mangled.clone())).into());
        });
        match ctx.nominals.borrow_mut().entry(mangled.clone()) {
            Entry::Occupied(mut x) => x.get_mut().0 = ty,
            Entry::Vacant(x) => {x.insert((ty, true, Default::default()));}
        }
        if !ctx.prepass.get() {self.methods.iter().for_each(|a| {a.codegen_errs(ctx, &mut errs);});}
        let mut noms = ctx.nominals.borrow_mut();
        ctx.restore_scope(old_scope);
        noms.get_mut(&mangled).unwrap().2 = ctx.map_split_vars(|v| (v.parent.unwrap(), v.symbols.into_iter().map(|(k, v)| (k, v.0)).collect()));
        match ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::make_type(Type::Nominal(mangled.clone())), VariableData {fwd: ctx.prepass.get(), init: errs.iter().any(|e| matches!(e, CobaltError::UninitializedGlobal {..})), ..VariableData::with_vis(self.loc, vs)}))) {
            Ok(x) => (x.0.clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                noms.remove(&mangled);
                errs.push(CobaltError::NotAModule {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string()
                });
                (Value::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                noms.remove(&mangled);
                errs.push(CobaltError::RedefVariable {
                    loc: self.name.ids[x].1,
                    name: self.name.start(x).to_string(),
                    prev: d
                });
                (Value::error(), errs)
            }
        }
    }
    fn to_code(&self) -> String {format!("type {} = {}", self.name, self.val.to_code())}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "type: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
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
pub struct VarGetAST {
    loc: SourceSpan,
    pub name: String,
    pub global: bool
}
impl VarGetAST {
    pub fn new(loc: SourceSpan, name: String, global: bool) -> Self {VarGetAST {loc, name, global}}
}
impl AST for VarGetAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type(&self, ctx: &CompCtx) -> Type {
        if let Some(Symbol(x, _)) = ctx.lookup(&self.name, self.global) {x.data_type.clone()}
        else {Type::Error}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        match ctx.lookup(&self.name, self.global) {
            Some(Symbol(x, d)) if d.scope.map_or(true, |x| x.get() == ctx.var_scope.get()) => (x.clone(), if d.init {vec![]} else {vec![CobaltError::UninitializedGlobal {
                name: self.name.clone(),
                loc: self.loc
            }]}),
            _ => (Value::error(), vec![CobaltError::VariableDoesNotExist {
                name: self.name.clone(),
                module: String::new(),
                container: "",
                loc: self.loc
            }])
        }
    }
    fn to_code(&self) -> String {
        format!("{}{}", if self.global {"."} else {""}, self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "var: {}{}", if self.global {"."} else {""}, self.name)
    }
}
