use crate::*;
use inkwell::values::BasicValueEnum::*;
use std::cell::Cell;
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub global: bool
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        if self.global {
            if self.val.is_const() && self.type_.is_none() {
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
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                        gv.set_constant(true);
                        gv.set_initializer(&v);
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
                        let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                        gv.set_constant(false);
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
        format!("let {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "vardef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, global: bool) -> Self {VarDefAST {loc, name, val, type_, global}}
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub type_: Option<ParsedType>,
    pub global: bool
}
impl AST for MutDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        if self.global {
            if self.val.is_const() && self.type_.is_none() {
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
                match if let Some(v) = val.comp_val {
                    if ctx.is_const.get() {
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {good: Cell::new(true), ..val})))
                    }
                    else {
                        let t = dt.llvm_type(ctx).unwrap();
                        let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                        gv.set_constant(false);
                        gv.set_initializer(&v);
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
                        let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                        gv.set_constant(false);
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
                        let t2 = val.data_type.clone();
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
        format!("mut {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mutdef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>, global: bool) -> Self {MutDefAST {loc, name, val, type_, global}}
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
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
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
    pub type_: Option<ParsedType>
}
impl AST for ConstDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let old_is_const = ctx.is_const.replace(true);
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
        format!("const {}{} = {}", self.name, self.type_.as_ref().map_or("".to_string(), |t| format!(": {t}")), self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "constdef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl ConstDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, type_: Option<ParsedType>) -> Self {ConstDefAST {loc, name, val, type_}}
}
