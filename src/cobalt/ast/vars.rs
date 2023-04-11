use crate::*;
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
use glob::Pattern;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, Location)>,
    pub global: bool
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, Location)>, global: bool) -> Self {VarDefAST {loc, name, val, type_, annotations, global}}
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {(self.loc.0, self.loc.1.start..self.val.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut vis_spec = None;
        let mut stack = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
                "static" => {
                    if let Some(arg) = arg {
                        errs.push(Diagnostic::error(loc.clone(), 411, Some(format!("unexpected argument {arg:?} to @static annotation"))))
                    }
                    if self.global {
                        errs.push(Diagnostic::warning(loc.clone(), 30, None))
                    }
                    if is_static {
                        errs.push(Diagnostic::warning(loc.clone(), 31, None))
                    }
                    is_static = true;
                },
                "link" => {
                    if let Some((_, prev)) = link_type.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 414, None).note(prev, "previously defined here".to_string()))
                    }
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Diagnostic::error(loc.clone(), 412, None)); None},
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        Some(x) => {errs.push(Diagnostic::error(loc.clone(), 413, Some(format!("unknown link type {x:?}")))); None},
                    }.map(|x| (x, loc.clone()))
                },
                "linkas" => {
                    if let Some((_, prev)) = linkas.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 416, None).note(prev, "previously defined here".to_string()))
                    }
                    if let Some(arg) = arg {
                        linkas = Some((arg.clone(), loc.clone()))
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 415, None))
                    }
                },
                "extern" => {
                    if let Some(prev) = is_extern.clone() {
                        errs.push(Diagnostic::warning(loc.clone(), 22, None).note(prev, "previously defined here".to_string()))
                    }
                    is_extern = Some(loc.clone());
                },
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                "export" => {
                    if !self.global {
                        errs.push(Diagnostic::error(loc.clone(), 429, None));
                    }
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((false, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                "private" => {
                    if !self.global {
                        errs.push(Diagnostic::error(loc.clone(), 429, None));
                    }
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((true, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                "stack" => {
                    if let Some(l) = stack.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 33, None).note(l, "previously defined here".to_string()));
                    }
                    else {
                        stack = Some(loc.clone());
                        if let Some(arg) = arg.as_deref() {
                            errs.push(Diagnostic::error(loc.clone(), 437, Some(format!("unexpected argument {arg:?}"))));
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        if self.global || is_static {
            if let Some(loc) = stack {
                errs.push(Diagnostic::error(loc, 436, None));
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
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }; None}),
                    None,
                    Type::Reference(Box::new(dt), false)
                ), VariableData::with_vis(self.loc.clone(), vs)))) {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
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
                val.inter_val = None;
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                            Some(PointerValue(gv.as_pointer_value())),
                            None,
                            Type::Reference(Box::new(dt), false)
                        ), VariableData::with_vis(self.loc.clone(), vs))))
                    }
                }
                else {
                    if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
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
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
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
                        val.inter_val = None;
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                None,
                                Type::Reference(Box::new(dt), false)
                            ), VariableData::with_vis(self.loc.clone(), vs))))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            if dt != Type::Error {
                                errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
                        }
                    };
                    ctx.global.set(old_global);
                    val
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }
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
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
                        (Value::error(), errs)
                    }
                } 
            }
        }
        else {
            if let Some(loc) = is_extern {
                errs.push(Diagnostic::error(loc, 417, None))
            }
            if let Some((_, loc)) = link_type {
                errs.push(Diagnostic::error(loc, 418, None))
            }
            if let Some((_, loc)) = linkas {
                errs.push(Diagnostic::error(loc, 419, None))
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
            match if ctx.is_const.get() || (val.data_type.register() && stack.is_none()) {
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = val.addr(ctx).unwrap_or_else(|| {
                    let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                    ctx.builder.build_store(a, v);
                    a
                });
                ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    Some(PointerValue(a)),
                    val.inter_val,
                    Type::Reference(Box::new(val.data_type), false)
                ), VariableData::with_vis(self.loc.clone(), false))))
            }
            else {
                if dt != Type::Error {
                    errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                }
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
            } {
                Ok(x) => (x.0.clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                    (Value::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, d, _)) => {
                    let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                    if let Some(loc) = d {
                        err.add_note(loc, "previously defined here".to_string());
                    }
                    errs.push(err);
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "let: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false)?}
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, Location)>,
    pub global: bool
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, Location)>, global: bool) -> Self {MutDefAST {loc, name, val, type_, annotations, global}}
}
impl AST for MutDefAST {
    fn loc(&self) -> Location {(self.loc.0, self.loc.1.start..self.val.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
                "static" => {
                    if let Some(arg) = arg {
                        errs.push(Diagnostic::error(loc.clone(), 411, Some(format!("unexpected argument {arg:?} to @static annotation"))))
                    }
                    if self.global {
                        errs.push(Diagnostic::warning(loc.clone(), 30, None))
                    }
                    if is_static {
                        errs.push(Diagnostic::warning(loc.clone(), 31, None))
                    }
                    is_static = true;
                },
                "link" => {
                    if let Some((_, prev)) = link_type.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 414, None).note(prev, "previously defined here".to_string()))
                    }
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Diagnostic::error(loc.clone(), 412, None)); None},
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        Some(x) => {errs.push(Diagnostic::error(loc.clone(), 413, Some(format!("unknown link type {x:?}")))); None},
                    }.map(|x| (x, loc.clone()))
                },
                "linkas" => {
                    if let Some((_, prev)) = linkas.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 416, None).note(prev, "previously defined here".to_string()))
                    }
                    if let Some(arg) = arg {
                        linkas = Some((arg.clone(), loc.clone()))
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 415, None))
                    }
                },
                "extern" => {
                    if let Some(prev) = is_extern.clone() {
                        errs.push(Diagnostic::warning(loc.clone(), 22, None).note(prev, "previously defined here".to_string()))
                    }
                    is_extern = Some(loc.clone());
                },
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                "export" => {
                    if self.global {
                        errs.push(Diagnostic::error(loc.clone(), 429, None));
                    }
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((false, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                "private" => {
                    if self.global {
                        errs.push(Diagnostic::error(loc.clone(), 429, None));
                    }
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((true, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
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
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }; None}),
                    None,
                    Type::Reference(Box::new(dt), false)
                ), VariableData::with_vis(self.loc.clone(), vs)))) {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
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
                val.inter_val = None;
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(false);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                            Some(PointerValue(gv.as_pointer_value())),
                            None,
                            Type::Reference(Box::new(dt), false)
                        ), VariableData::with_vis(self.loc.clone(), vs))))
                    }
                }
                else {
                    if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
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
                        ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
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
                        val.inter_val = None;
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                None,
                                Type::Reference(Box::new(dt), false)
                            ), VariableData::with_vis(self.loc.clone(), vs))))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            if dt != Type::Error {
                                errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
                        }
                    };
                    ctx.global.set(old_global);
                    val
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    if dt != Type::Error {
                        errs.push(Diagnostic::error(self.loc.clone(), 327, None).note(self.type_.as_ref().unwrap_or(&self.val).loc(), format!("variable type is {dt}")).info("consider using const for const-only values".to_string()));
                    }
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
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs))))
                } {
                    Ok(x) => (x.0.clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Value::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, d, _)) => {
                        let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                        if let Some(loc) = d {
                            err.add_note(loc, "previously defined here".to_string());
                        }
                        errs.push(err);
                        (Value::error(), errs)
                    }
                } 
            }
        }
        else {
            if let Some(loc) = is_extern {
                errs.push(Diagnostic::error(loc, 417, None))
            }
            if let Some((_, loc)) = link_type {
                errs.push(Diagnostic::error(loc, 418, None))
            }
            if let Some((_, loc)) = linkas {
                errs.push(Diagnostic::error(loc, 419, None))
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
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = val.addr(ctx).unwrap_or_else(|| {
                    let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                    ctx.builder.build_store(a, v);
                    a
                });
                ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    Some(PointerValue(a)),
                    val.inter_val,
                    Type::Reference(Box::new(val.data_type), true)
                ), VariableData::with_vis(self.loc.clone(), false))))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), false))))
            } {
                Ok(x) => (x.0.clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                    (Value::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, d, _)) => {
                    let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                    if let Some(loc) = d {
                        err.add_note(loc, "previously defined here".to_string());
                    }
                    errs.push(err);
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mut: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false)?}
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct ConstDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<Box<dyn AST>>,
    pub annotations: Vec<(String, Option<String>, Location)>
}
impl ConstDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<Box<dyn AST>>, annotations: Vec<(String, Option<String>, Location)>) -> Self {ConstDefAST {loc, name, val, type_, annotations}}
}
impl AST for ConstDefAST {
    fn loc(&self) -> Location {(self.loc.0, self.loc.1.start..self.val.loc().1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                "export" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((false, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                "private" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((true, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
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
        match ctx.with_vars(|v| v.insert(&self.name, Symbol(val, VariableData::with_vis(self.loc.clone(), vs)))) {
            Ok(x) => (x.0.clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                (Value::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                if let Some(loc) = d {
                    err.add_note(loc, "previously defined here".to_string());
                }
                errs.push(err);
                (Value::error(), errs)
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or_default().as_str() + " ")) {out += s.as_str();}
        out + format!("const {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "const: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        if let Some(ref ast) = self.type_ {print_ast_child(f, pre, &**ast, false)?}
        print_ast_child(f, pre, &*self.val, true)
    }
}
pub struct TypeDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub annotations: Vec<(String, Option<String>, Location)>,
    pub methods: Vec<Box<dyn AST>>
}
impl TypeDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, annotations: Vec<(String, Option<String>, Location)>, methods: Vec<Box<dyn AST>>) -> Self {TypeDefAST {loc, name, val, annotations, methods}}
}
impl AST for TypeDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::TypeData}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut vis_spec = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                "export" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((true, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((false, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                "private" => {
                    if let Some((_, vs)) = vis_spec.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 428, None).note(vs, "previously defined here".to_string()));
                    }
                    else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some((false, loc.clone())),
                            Some("false") | Some("0") => vis_spec = Some((true, loc.clone())),
                            Some(x) => errs.push(Diagnostic::error(loc.clone(), 428, Some(format!("expected an argument like 'true' or 'false', got '{x}'"))))
                        }
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        if target_match == 0 {return (Value::null(), errs)}
        let ty = types::utils::impl_convert(self.val.loc(), (self.val.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {errs.push(e); Type::Error}, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        match ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::make_type(Type::Nominal(ctx.mangle(&self.name))), VariableData::with_vis(self.loc.clone(), vs)))) {
            Ok(x) => {
                types::NOMINAL_TYPES.write().expect("Value should not be poisoned!").insert(ctx.mangle(&self.name), (ty, true));
                (x.0.clone(), errs)
            },
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(Diagnostic::error(self.name.ids[x].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                (Value::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, d, _)) => {
                let mut err = Diagnostic::error(self.name.ids[x].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x))));
                if let Some(loc) = d {
                    err.add_note(loc, "previously defined here".to_string());
                }
                errs.push(err);
                (Value::error(), errs)
            }
        }
    }
    fn to_code(&self) -> String {format!("type {} = {}", self.name, self.val.to_code())}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "type: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        print_ast_child(f, pre, &*self.val, self.methods.is_empty())?;
        if !self.methods.is_empty() {
            writeln!(f, "{pre}└── statics:")?;
            pre.push(true);
            let mut count = self.methods.len();
            for m in self.methods.iter() {
                print_ast_child(f, pre, &**m, count == 1)?;
                count -= 1;
            }
            pre.pop();
        }
        Ok(())
    }
}
pub struct VarGetAST {
    loc: Location,
    pub name: String,
    pub global: bool
}
impl VarGetAST {
    pub fn new(loc: Location, name: String, global: bool) -> Self {VarGetAST {loc, name, global}}
}
impl AST for VarGetAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Some(Symbol(x, _)) = ctx.lookup(&self.name, self.global) {x.data_type.clone()}
        else {Type::Error}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match ctx.lookup(&self.name, self.global) {
            Some(Symbol(x, _)) => (x.clone(), vec![]),
            None => (Value::error(), vec![Diagnostic::error(self.loc.clone(), 320, Some(format!("{} does not exist", self.name)))])
        }
    }
    fn to_code(&self) -> String {
        format!("{}{}", if self.global {"."} else {""}, self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "var: {}{}", if self.global {"."} else {""}, self.name)
    }
}
