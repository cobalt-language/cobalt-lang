use crate::*;
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
use glob::Pattern;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>, Location)>,
    pub global: bool
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
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
                        let negate = if arg.as_bytes().get(0) == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = if negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()) {1} else {0}},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
            }
        }
        if target_match == 0 {return (Variable::error(), errs)}
        if self.global || is_static {
            if is_extern.is_some() {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime(loc)) => {
                            errs.push(Diagnostic::error(loc, 324, None));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: dt.llvm_type(ctx).map(|t| {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {errs.push(Diagnostic::warning(self.loc.clone(), 21, None)); None}),
                    inter_val: None,
                    data_type: dt,
                    export: true
                }))) {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
                    }
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let (val, mut es) = self.val.codegen(ctx);
                errs.append(&mut es);
                let t2 = val.data_type.clone();
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime(loc)) => {
                            errs.push(Diagnostic::error(loc, 324, None));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: Type::Reference(Box::new(dt), false),
                            export: true
                        })))
                    }
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
                    }
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let mut errs;
                match if let Some(t) = t.llvm_type(ctx) {
                    if ctx.is_const.get() {
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime(loc)) => {
                                    errs.push(Diagnostic::error(loc, 324, None));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                            Variable::error()
                        });
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(false);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime(loc)) => {
                                    errs.push(Diagnostic::error(loc, 324, None));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                            Variable::error()
                        });
                        ctx.restore_scope(old_scope);
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                                comp_val: Some(PointerValue(gv.as_pointer_value())),
                                inter_val: val.inter_val,
                                data_type: Type::Reference(Box::new(dt), false),
                                export: true
                            })))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                        }
                    }
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    let t2 = val.data_type.clone();
                    let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                        let (t, mut es) = t.into_type(ctx);
                        errs.append(&mut es);
                        let t = match t {
                            Ok(t) => Some(t),
                            Err(IntoTypeError::NotAnInt(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                None
                            },
                            Err(IntoTypeError::NotCompileTime(loc)) => {
                                errs.push(Diagnostic::error(loc, 324, None));
                                None
                            },
                            Err(IntoTypeError::NotAModule(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                None
                            },
                            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                None
                            }
                        };
                        t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                    }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                    let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                        errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                        Variable::error()
                    });
                    ctx.restore_scope(old_scope);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
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
            let (val, mut es) = self.val.codegen(ctx);
            errs.append(&mut es);
            let t2 = val.data_type.clone();
            let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                let (t, mut es) = t.into_type(ctx);
                errs.append(&mut es);
                let t = match t {
                    Ok(t) => Some(t),
                    Err(IntoTypeError::NotAnInt(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                        None
                    },
                    Err(IntoTypeError::NotCompileTime(loc)) => {
                        errs.push(Diagnostic::error(loc, 324, None));
                        None
                    },
                    Err(IntoTypeError::NotAModule(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                        None
                    },
                    Err(IntoTypeError::DoesNotExist(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                        None
                    }
                };
                t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
            }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
            let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                Variable::error()
            });
            ctx.restore_scope(old_scope);
            match if ctx.is_const.get() || val.data_type.register() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                ctx.builder.build_store(a, v);
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: Some(PointerValue(a)),
                    inter_val: val.inter_val,
                    data_type: Type::Reference(Box::new(val.data_type), false),
                    export: true
                })))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
            } {
                Ok(x) => (x.as_var().unwrap().clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                    (Variable::error(), errs)
                }
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("let {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "vardef: {}", self.name)?;
        for (name, arg, _) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>, Location)>, global: bool) -> Self {VarDefAST {loc, name, val, type_, annotations, global}}
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>, Location)>,
    pub global: bool
}
impl AST for MutDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let mut is_static = false;
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
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
                        let negate = if arg.as_bytes().get(0) == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        match Pattern::new(arg) {
                            Ok(pat) => if target_match != 1 {target_match = if negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()) {1} else {0}},
                            Err(err) => errs.push(Diagnostic::error(loc.clone(), 427, Some(format!("error at byte {}: {}", err.pos, err.msg))))
                        }
                    }
                    else {
                        errs.push(Diagnostic::error(loc.clone(), 426, None));
                    }
                },
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition"))))
            }
        }
        if target_match == 0 {return (Variable::error(), errs)}
        if self.global || is_static {
            if is_extern.is_some() {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime(loc)) => {
                            errs.push(Diagnostic::error(loc, 324, None));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: dt.llvm_type(ctx).map(|t| {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        match link_type {
                            None => {},
                            Some((WeakAny, _)) => gv.set_linkage(ExternalWeak),
                            Some((x, _)) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {errs.push(Diagnostic::warning(self.loc.clone(), 23, None)); None}),
                    inter_val: None,
                    data_type: dt,
                    export: true
                }))) {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
                    }
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let old_scope = ctx.push_scope(&self.name);
                let (val, mut es) = self.val.codegen(ctx);
                errs.append(&mut es);
                let t2 = val.data_type.clone();
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime(loc)) => {
                            errs.push(Diagnostic::error(loc, 324, None));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name, loc)) => {
                            errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                ctx.restore_scope(old_scope);
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: Type::Reference(Box::new(dt), false),
                            export: true
                        })))
                    }
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
                    }
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let mut errs;
                match if let Some(t) = t.llvm_type(ctx) {
                    if ctx.is_const.get() {
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime(loc)) => {
                                    errs.push(Diagnostic::error(loc, 324, None));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                            Variable::error()
                        });
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.map_or_else(|| ctx.mangle(&self.name), |(name, _)| name).as_str());
                        gv.set_constant(false);
                        if let Some((link, _)) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime(loc)) => {
                                    errs.push(Diagnostic::error(loc, 324, None));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                            Variable::error()
                        });
                        ctx.restore_scope(old_scope);
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                                comp_val: Some(PointerValue(gv.as_pointer_value())),
                                inter_val: val.inter_val,
                                data_type: Type::Reference(Box::new(dt), false),
                                export: true
                            })))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                        }
                    }
                }
                else {
                    let old_scope = ctx.push_scope(&self.name);
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    let t2 = val.data_type.clone();
                    let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                        let (t, mut es) = t.into_type(ctx);
                        errs.append(&mut es);
                        let t = match t {
                            Ok(t) => Some(t),
                            Err(IntoTypeError::NotAnInt(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                                None
                            },
                            Err(IntoTypeError::NotCompileTime(loc)) => {
                                errs.push(Diagnostic::error(loc, 324, None));
                                None
                            },
                            Err(IntoTypeError::NotAModule(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                                None
                            },
                            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                                None
                            }
                        };
                        t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                    }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                    let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                        errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                        Variable::error()
                    });
                    ctx.restore_scope(old_scope);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                        (Variable::error(), errs)
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
            let (val, mut errs) = self.val.codegen(ctx);
            let t2 = val.data_type.clone();
            let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                let (t, mut es) = t.into_type(ctx);
                errs.append(&mut es);
                let t = match t {
                    Ok(t) => Some(t),
                    Err(IntoTypeError::NotAnInt(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                        None
                    },
                    Err(IntoTypeError::NotCompileTime(loc)) => {
                        errs.push(Diagnostic::error(loc, 324, None));
                        None
                    },
                    Err(IntoTypeError::NotAModule(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                        None
                    },
                    Err(IntoTypeError::DoesNotExist(name, loc)) => {
                        errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                        None
                    }
                };
                t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
            }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
            let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
                Variable::error()
            });
            ctx.restore_scope(old_scope);
            match if ctx.is_const.get() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |(x, _)| x.as_str()));
                ctx.builder.build_store(a, v);
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: Some(PointerValue(a)),
                    inter_val: val.inter_val,
                    data_type: Type::Reference(Box::new(val.data_type), true),
                    export: true
                })))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val})))
            } {
                Ok(x) => (x.as_var().unwrap().clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, _)) => {
                    errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                    (Variable::error(), errs)
                }
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("mut {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mutdef: {}", self.name)?;
        for (name, arg, _) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>, Location)>, global: bool) -> Self {MutDefAST {loc, name, val, type_, annotations, global}}
}
pub struct ConstDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>, Location)>
}
impl AST for ConstDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let mut errs = self.annotations.iter().map(|(x, _, loc)| Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for variable definition")))).collect::<Vec<_>>();
        let old_is_const = ctx.is_const.replace(true);
        let old_scope = ctx.push_scope(&self.name);
        let (val, mut es) = self.val.codegen(ctx);
        errs.append(&mut es);
        let t2 = val.data_type.clone();
        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
            let (t, mut es) = t.into_type(ctx);
            errs.append(&mut es);
            let t = match t {
                Ok(t) => Some(t),
                Err(IntoTypeError::NotAnInt(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                    None
                },
                Err(IntoTypeError::NotCompileTime(loc)) => {
                    errs.push(Diagnostic::error(loc, 324, None));
                    None
                },
                Err(IntoTypeError::NotAModule(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                    None
                },
                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                    None
                }
            };
            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
            errs.push(Diagnostic::error(self.val.loc(), 311, Some(err)));
            Variable::error()
        });
        ctx.restore_scope(old_scope);
        ctx.is_const.set(old_is_const);
        match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {export: true, ..val}))) {
            Ok(x) => (x.as_var().unwrap().clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 321, Some(format!("{} is not a module", self.name.start(x)))));
                (Variable::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, _)) => {
                errs.push(Diagnostic::error(self.name.ids[x - 1].1.clone(), 323, Some(format!("{} has already been defined", self.name.start(x)))));
                (Variable::error(), errs)
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("const {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "const: {}", self.name)?;
        for (name, arg, _) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl ConstDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>, Location)>) -> Self {ConstDefAST {loc, name, val, type_, annotations}}
}
pub struct VarGetAST {
    loc: Location,
    pub name: DottedName
}
impl VarGetAST {
    pub fn new(loc: Location, name: DottedName) -> Self {VarGetAST {loc, name}}
}
impl AST for VarGetAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Ok(Symbol::Variable(x)) = ctx.with_vars(|v| v.lookup(&self.name)) {x.data_type.clone()}
        else {Type::Null}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        match ctx.with_vars(|v| v.lookup(&self.name)) {
            Ok(Symbol::Variable(x)) => (x.clone(), vec![]),
            Ok(Symbol::Module(_)) => (Variable::error(), vec![Diagnostic::error(self.name.ids.last().unwrap().1.clone(), 322, Some(format!("{} is not a variable", self.name)))]),
            Err(UndefVariable::NotAModule(idx)) => (Variable::error(), vec![Diagnostic::error(self.name.ids[idx].1.clone(), 321, Some(format!("{} is not a module", self.name.start(idx))))]),
            Err(UndefVariable::DoesNotExist(idx)) => (Variable::error(), vec![Diagnostic::error(self.name.ids[idx].1.clone(), 320, Some(format!("{} does not exist", self.name.start(idx))))])
        }
    }
    fn to_code(&self) -> String {
        format!("{}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "varget: {}", self.name)
    }
}