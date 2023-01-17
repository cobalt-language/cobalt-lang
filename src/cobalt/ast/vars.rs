use crate::*;
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
use std::cell::Cell;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>)>,
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
        let mut is_extern = false;
        for (ann, arg) in self.annotations.iter() {
            match ann.as_str() {
                "static" => {
                    if let Some(arg) = arg {
                        errs.push(Error::new(self.loc.clone(), 411, format!("unexpected argument {arg:?} to @static annotation")))
                    }
                    if self.global {
                        errs.push(Error::new(self.loc.clone(), 20, "@static annotation on a global variable is redundant".to_string()))
                    }
                    if is_static {
                        errs.push(Error::new(self.loc.clone(), 21, "specifying the @static annotation multiple times doesn't do anything".to_string()))
                    }
                    is_static = true;
                },
                "link" => {
                    if link_type.is_some() {
                        errs.push(Error::new(self.loc.clone(), 414, "respecification of linkage type".to_string()))
                    }
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Error::new(self.loc.clone(), 412, "@link annotation requires an argument".to_string())); None},
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        Some(x) => {errs.push(Error::new(self.loc.clone(), 413, format!("unknown link type {x:?} for @link annotation"))); None},
                    }
                },
                "linkas" => {
                    if linkas.is_some() {
                        errs.push(Error::new(self.loc.clone(), 416, "respecification of @linkas annotation".to_string()))
                    }
                    if let Some(arg) = arg {
                        linkas = Some(arg.clone())
                    }
                    else {
                        errs.push(Error::new(self.loc.clone(), 415, "@linkas annotation requires an argument".to_string()))
                    }
                },
                "extern" => {
                    if is_extern {
                        errs.push(Error::new(self.loc.clone(), 22, "specifying the @extern annotation multiple times doesn't do anything".to_string()))
                    }
                    is_extern = true;
                },
                x => errs.push(Error::new(self.loc.clone(), 410, format!("unknown annotation {x:?} for variable definition")))
            }
        }
        if self.global || is_static {
            if is_extern {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name)) => {
                            errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime) => {
                            errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name)) => {
                            errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name)) => {
                            errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: dt.llvm_type(ctx).map(|t| {
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        match link_type {
                            None => {},
                            Some(WeakAny) => gv.set_linkage(ExternalWeak),
                            Some(x) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {errs.push(Error::new(self.loc.clone(), 23, "externally linked variable has a non-runtime type".to_string())); None}),
                    inter_val: None,
                    data_type: dt,
                    good: Cell::new(true)
                }))) {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let (val, mut es) = self.val.codegen(ctx);
                errs.append(&mut es);
                let t2 = val.data_type.clone();
                let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    let t = match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name)) => {
                            errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime) => {
                            errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name)) => {
                            errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name)) => {
                            errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                            None
                        }
                    };
                    t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some(link) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: Type::Reference(Box::new(dt), false),
                            good: Cell::new(true)
                        })))
                    }
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
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
                                Err(IntoTypeError::NotAnInt(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime) => {
                                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Error::new(self.loc.clone(), 311, err));
                            Variable::error()
                        });
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        gv.set_constant(false);
                        if let Some(link) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime) => {
                                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Error::new(self.loc.clone(), 311, err));
                            Variable::error()
                        });
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                                comp_val: Some(PointerValue(gv.as_pointer_value())),
                                inter_val: val.inter_val,
                                data_type: Type::Reference(Box::new(dt), false),
                                good: Cell::new(true)
                            })))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                        }
                    }
                }
                else {
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    let t2 = val.data_type.clone();
                    let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                        let (t, mut es) = t.into_type(ctx);
                        errs.append(&mut es);
                        let t = match t {
                            Ok(t) => Some(t),
                            Err(IntoTypeError::NotAnInt(name)) => {
                                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                None
                            },
                            Err(IntoTypeError::NotCompileTime) => {
                                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                None
                            },
                            Err(IntoTypeError::NotAModule(name)) => {
                                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                None
                            },
                            Err(IntoTypeError::DoesNotExist(name)) => {
                                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                None
                            }
                        };
                        t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                    }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                    let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                        errs.push(Error::new(self.loc.clone(), 311, err));
                        Variable::error()
                    });
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
        }
        else {
            if is_extern {
                errs.push(Error::new(self.loc.clone(), 417, "@extern annotation cannot be used on a variable that isn't global or static".to_string()))
            }
            if link_type.is_some() {
                errs.push(Error::new(self.loc.clone(), 418, "@link annotation cannot be used on a variable that isn't global or static".to_string()))
            }
            if linkas.is_some() {
                errs.push(Error::new(self.loc.clone(), 419, "@linkas annotation cannot be used on a variable that isn't global or static".to_string()))
            }
            let (val, mut es) = self.val.codegen(ctx);
            errs.append(&mut es);
            let t2 = val.data_type.clone();
            let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                let (t, mut es) = t.into_type(ctx);
                errs.append(&mut es);
                let t = match t {
                    Ok(t) => Some(t),
                    Err(IntoTypeError::NotAnInt(name)) => {
                        errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                        None
                    },
                    Err(IntoTypeError::NotCompileTime) => {
                        errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                        None
                    },
                    Err(IntoTypeError::NotAModule(name)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                        None
                    },
                    Err(IntoTypeError::DoesNotExist(name)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                        None
                    }
                };
                t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
            }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
            let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                errs.push(Error::new(self.loc.clone(), 311, err));
                Variable::error()
            });
            match if ctx.is_const.get() || val.data_type.register() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |x| x.as_str()));
                ctx.builder.build_store(a, v);
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: Some(PointerValue(a)),
                    inter_val: val.inter_val,
                    data_type: Type::Reference(Box::new(val.data_type), false),
                    good: Cell::new(true)
                })))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
            } {
                Ok(x) => (x.as_var().unwrap().clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, _)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("let {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "vardef: {}", self.name)?;
        for (name, arg) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>)>, global: bool) -> Self {VarDefAST {loc, name, val, type_, annotations, global}}
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>)>,
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
        let mut is_extern = false;
        for (ann, arg) in self.annotations.iter() {
            match ann.as_str() {
                "static" => {
                    if let Some(arg) = arg {
                        errs.push(Error::new(self.loc.clone(), 411, format!("unexpected argument {arg:?} to @static annotation")))
                    }
                    if self.global {
                        errs.push(Error::new(self.loc.clone(), 20, "@static annotation on a global variable is redundant".to_string()))
                    }
                    if is_static {
                        errs.push(Error::new(self.loc.clone(), 21, "specifying the @static annotation multiple times doesn't do anything".to_string()))
                    }
                    is_static = true;
                },
                "link" => {
                    if link_type.is_some() {
                        errs.push(Error::new(self.loc.clone(), 414, "respecification of linkage type".to_string()))
                    }
                    link_type = match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Error::new(self.loc.clone(), 412, "@link annotation requires an argument".to_string())); None},
                        Some("extern") | Some("external") => Some(External),
                        Some("extern-weak") | Some("extern_weak") | Some("external-weak") | Some("external_weak") => Some(ExternalWeak),
                        Some("intern") | Some("internal") => Some(Internal),
                        Some("private") => Some(Private),
                        Some("weak") => Some(WeakAny),
                        Some("weak-odr") | Some("weak_odr") => Some(WeakODR),
                        Some("linkonce") | Some("link-once") | Some("link_once") => Some(LinkOnceAny),
                        Some("linkonce-odr") | Some("linkonce_odr") | Some("link-once-odr") | Some("link_once_odr") => Some(LinkOnceODR),
                        Some("common") => Some(Common),
                        Some(x) => {errs.push(Error::new(self.loc.clone(), 413, format!("unknown link type {x:?} for @link annotation"))); None},
                    }
                },
                "linkas" => {
                    if linkas.is_some() {
                        errs.push(Error::new(self.loc.clone(), 416, "respecification of @linkas annotation".to_string()))
                    }
                    if let Some(arg) = arg {
                        linkas = Some(arg.clone())
                    }
                    else {
                        errs.push(Error::new(self.loc.clone(), 415, "@linkas annotation requires an argument".to_string()))
                    }
                },
                "extern" => {
                    if is_extern {
                        errs.push(Error::new(self.loc.clone(), 22, "specifying the @extern annotation multiple times doesn't do anything".to_string()))
                    }
                    is_extern = true;
                },
                x => errs.push(Error::new(self.loc.clone(), 410, format!("unknown annotation {x:?} for variable definition")))
            }
        }
        if self.global || is_static {
            if is_extern {
                let t2 = self.val.res_type(ctx);
                let dt = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name)) => {
                            errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime) => {
                            errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name)) => {
                            errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name)) => {
                            errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                            None
                        }
                    }
                }) {t} else if t2 == Type::IntLiteral {Type::Int(64, false)} else if let Type::Reference(b, _) = t2 {*b} else {t2};
                match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: dt.llvm_type(ctx).map(|t| {
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        match link_type {
                            None => {},
                            Some(WeakAny) => gv.set_linkage(ExternalWeak),
                            Some(x) => gv.set_linkage(x)
                        }
                        PointerValue(gv.as_pointer_value())
                    }).or_else(|| {errs.push(Error::new(self.loc.clone(), 23, "externally linked variable has a non-runtime type".to_string())); None}),
                    inter_val: None,
                    data_type: dt,
                    good: Cell::new(true)
                }))) {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
            else if self.val.is_const() && self.type_.is_none() {
                let (val, mut es) = self.val.codegen(ctx);
                errs.append(&mut es);
                let t2 = val.data_type.clone();
                let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                    let (t, mut es) = t.into_type(ctx);
                    errs.append(&mut es);
                    let t = match t {
                        Ok(t) => Some(t),
                        Err(IntoTypeError::NotAnInt(name)) => {
                            errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                            None
                        },
                        Err(IntoTypeError::NotCompileTime) => {
                            errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                            None
                        },
                        Err(IntoTypeError::NotAModule(name)) => {
                            errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                            None
                        },
                        Err(IntoTypeError::DoesNotExist(name)) => {
                            errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                            None
                        }
                    };
                    t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
                        if let Some(link) = link_type {gv.set_linkage(link)}
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: Type::Reference(Box::new(dt), false),
                            good: Cell::new(true)
                        })))
                    }
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
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
                                Err(IntoTypeError::NotAnInt(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime) => {
                                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Error::new(self.loc.clone(), 311, err));
                            Variable::error()
                        });
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let gv = ctx.module.add_global(t, None, linkas.unwrap_or_else(|| format!("{}", self.name)).as_str());
                        gv.set_constant(false);
                        if let Some(link) = link_type {gv.set_linkage(link)}
                        let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                        let entry = ctx.context.append_basic_block(f, "entry");
                        let old_ip = ctx.builder.get_insert_block();
                        ctx.builder.position_at_end(entry);
                        let (val, es) = self.val.codegen(ctx);
                        errs = es;
                        let t2 = val.data_type.clone();
                        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                            let (t, mut es) = t.into_type(ctx);
                            errs.append(&mut es);
                            let t = match t {
                                Ok(t) => Some(t),
                                Err(IntoTypeError::NotAnInt(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                    None
                                },
                                Err(IntoTypeError::NotCompileTime) => {
                                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                    None
                                },
                                Err(IntoTypeError::NotAModule(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                    None
                                },
                                Err(IntoTypeError::DoesNotExist(name)) => {
                                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                    None
                                }
                            };
                            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                            errs.push(Error::new(self.loc.clone(), 311, err));
                            Variable::error()
                        });
                        if let Some(v) = val.comp_val {
                            ctx.builder.build_store(gv.as_pointer_value(), v);
                            ctx.builder.build_return(None);
                            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
                            else {ctx.builder.clear_insertion_position();}
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                                comp_val: Some(PointerValue(gv.as_pointer_value())),
                                inter_val: val.inter_val,
                                data_type: Type::Reference(Box::new(dt), false),
                                good: Cell::new(true)
                            })))
                        }
                        else {
                            unsafe {
                                gv.delete();
                                f.as_global_value().delete();
                            }
                            ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                        }
                    }
                }
                else {
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    let t2 = val.data_type.clone();
                    let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                        let (t, mut es) = t.into_type(ctx);
                        errs.append(&mut es);
                        let t = match t {
                            Ok(t) => Some(t),
                            Err(IntoTypeError::NotAnInt(name)) => {
                                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                                None
                            },
                            Err(IntoTypeError::NotCompileTime) => {
                                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                                None
                            },
                            Err(IntoTypeError::NotAModule(name)) => {
                                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                                None
                            },
                            Err(IntoTypeError::DoesNotExist(name)) => {
                                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                                None
                            }
                        };
                        t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
                    }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
                    let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                        errs.push(Error::new(self.loc.clone(), 311, err));
                        Variable::error()
                    });
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
        }
        else {
            if is_extern {
                errs.push(Error::new(self.loc.clone(), 417, "@extern annotation cannot be used on a variable that isn't global or static".to_string()))
            }
            if link_type.is_some() {
                errs.push(Error::new(self.loc.clone(), 418, "@link annotation cannot be used on a variable that isn't global or static".to_string()))
            }
            if linkas.is_some() {
                errs.push(Error::new(self.loc.clone(), 419, "@linkas annotation cannot be used on a variable that isn't global or static".to_string()))
            }

            let (val, mut errs) = self.val.codegen(ctx);
            let t2 = val.data_type.clone();
            let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
                let (t, mut es) = t.into_type(ctx);
                errs.append(&mut es);
                let t = match t {
                    Ok(t) => Some(t),
                    Err(IntoTypeError::NotAnInt(name)) => {
                        errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                        None
                    },
                    Err(IntoTypeError::NotCompileTime) => {
                        errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                        None
                    },
                    Err(IntoTypeError::NotAModule(name)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                        None
                    },
                    Err(IntoTypeError::DoesNotExist(name)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                        None
                    }
                };
                t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
            }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
            let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
                errs.push(Error::new(self.loc.clone(), 311, err));
                Variable::error()
            });
            match if ctx.is_const.get() {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
            } 
            else if let (Some(t), Some(v)) = (val.data_type.llvm_type(ctx), val.comp_val) {
                let a = ctx.builder.build_alloca(t, self.name.ids.last().map_or("", |x| x.as_str()));
                ctx.builder.build_store(a, v);
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: Some(PointerValue(a)),
                    inter_val: val.inter_val,
                    data_type: Type::Reference(Box::new(val.data_type), true),
                    good: Cell::new(true)
                })))
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
            } {
                Ok(x) => (x.as_var().unwrap().clone(), errs),
                Err(RedefVariable::NotAModule(x, _)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::AlreadyExists(x, _)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                    (Variable::error(), errs)
                },
                Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
            }
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("mut {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mutdef: {}", self.name)?;
        for (name, arg) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>)>, global: bool) -> Self {MutDefAST {loc, name, val, type_, annotations, global}}
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
            Ok(Symbol::Variable(x)) =>
                (x.clone(), if x.good.get() {if !x.data_type.copyable() {x.good.set(false);} vec![]}
                else {vec![Error::new(self.loc.clone(), 90, format!("{} has been moved from and is now in an undefined state", self.name))]}),
            Ok(Symbol::Module(_)) => (Variable::error(), vec![Error::new(self.loc.clone(), 322, format!("{} is not a variable", self.name))]),
            Err(UndefVariable::NotAModule(idx)) => (Variable::error(), vec![Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(idx)))]),
            Err(UndefVariable::DoesNotExist(idx)) => (Variable::error(), vec![Error::new(self.loc.clone(), 323, format!("{} does not exist", self.name.start(idx)))])
        }
    }
    fn to_code(&self) -> String {
        format!("{}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "varget: {}", self.name)
    }
}
pub struct ConstDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub annotations: Vec<(String, Option<String>)>
}
impl AST for ConstDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let mut errs = self.annotations.iter().map(|(x, _)| Error::new(self.loc.clone(), 410, format!("unknown annotation {x:?} for variable definition"))).collect::<Vec<_>>();
        let old_is_const = ctx.is_const.replace(true);
        let (val, mut es) = self.val.codegen(ctx);
        errs.append(&mut es);
        let t2 = val.data_type.clone();
        let (dt, err) = if let Some(t) = self.type_.as_ref().and_then(|t| {
            let (t, mut es) = t.into_type(ctx);
            errs.append(&mut es);
            let t = match t {
                Ok(t) => Some(t),
                Err(IntoTypeError::NotAnInt(name)) => {
                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                    None
                },
                Err(IntoTypeError::NotCompileTime) => {
                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                    None
                },
                Err(IntoTypeError::NotAModule(name)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                    None
                },
                Err(IntoTypeError::DoesNotExist(name)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                    None
                }
            };
            t.map(|x| (x.clone(), format!("cannot convert value of type {} to {x}", t2)))
        }) {t} else if t2 == Type::IntLiteral {(Type::Int(64, false), "INFALLIBLE".to_string())} else if let Type::Reference(b, _) = t2 {(*b, "INFALLIBLE".to_string())} else {(t2, "INFALLIBLE".to_string())};
        let val = types::utils::impl_convert(val, dt.clone(), ctx).unwrap_or_else(|| {
            errs.push(Error::new(self.loc.clone(), 311, err));
            Variable::error()
        });
        ctx.is_const.set(old_is_const);
        match ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val}))) {
            Ok(x) => (x.as_var().unwrap().clone(), errs),
            Err(RedefVariable::NotAModule(x, _)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                (Variable::error(), errs)
            },
            Err(RedefVariable::AlreadyExists(x, _)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                (Variable::error(), errs)
            },
            Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
        }
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out + format!("const {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "const: {}", self.name)?;
        for (name, arg) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl ConstDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, annotations: Vec<(String, Option<String>)>) -> Self {ConstDefAST {loc, name, val, type_, annotations}}
}
