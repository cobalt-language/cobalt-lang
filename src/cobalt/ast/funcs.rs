use crate::*;
use std::cell::Cell;
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::*};
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Normal,
    Mutable,
    Constant
}
pub struct FnDefAST {
    loc: Location,
    pub name: DottedName,
    pub ret: ParsedType,
    pub params: Vec<(String, ParamType, ParsedType, Option<Box<dyn AST>>)>, // parameter, mutable, type, default
    pub body: Box<dyn AST>,
    pub annotations: Vec<(String, Option<String>)>
}
impl FnDefAST {
    pub fn new(loc: Location, name: DottedName, ret: ParsedType, params: Vec<(String, ParamType, ParsedType, Option<Box<dyn AST>>)>, body: Box<dyn AST>, annotations: Vec<(String, Option<String>)>) -> Self {FnDefAST {loc, name, ret, params, body, annotations}}
}
impl AST for FnDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name)) => {
                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime) => {
                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                Type::Null
            }
        };
        Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name)) => {
                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime) => {
                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name)) => {
                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime) => {
                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                Type::Null
            }
        };
        let fty = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name)) => {
                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime) => {
                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect());
        let mut errs = vec![];
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = false;
        let mut cconv: Option<u32> = None;
        for (ann, arg) in self.annotations.iter() {
            match ann.as_str() {
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
                        Some(x) => {errs.push(Error::new(self.loc.clone(), 413, format!("unknown link type {x:?} for @link annotation"))); None}
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
                "cconv" => {
                    if cconv.is_some() {
                        errs.push(Error::new(self.loc.clone(), 420, "respecification of calling convention".to_string()))
                    }
                    cconv = cconv.or(match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Error::new(self.loc.clone(), 421, "@cconv annotation requires an argument".to_string())); None},
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => Some(12),
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        Some(x) => {
                            match x.parse::<u32>() {
                                Ok(v) => Some(v),
                                Err(_) => {errs.push(Error::new(self.loc.clone(), 422, format!("unknown calling convention {x:?} for @cconv annotation"))); None}
                            }
                        }
                    });
                },
                "extern" => {
                    if is_extern {
                        errs.push(Error::new(self.loc.clone(), 22, "specifying the @extern annotation multiple times doesn't do anything".to_string()))
                    }
                    is_extern = true;
                    if cconv.is_some() {
                        errs.push(Error::new(self.loc.clone(), 420, "respecification of calling convention".to_string()))
                    }
                    cconv = cconv.or(match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.pop(); None},
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => Some(12),
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        Some(x) => {
                            match x.parse::<u32>() {
                                Ok(v) => Some(v),
                                Err(_) => {errs.push(Error::new(self.loc.clone(), 422, format!("unknown calling convention {x:?} for @cconv annotation"))); None}
                            }
                        }
                    });
                },
                x => errs.push(Error::new(self.loc.clone(), 410, format!("unknown annotation {x:?} for variable definition")))
            }
        }
        let old_ip = ctx.builder.get_insert_block();
        let val = if let Type::Function(ref ret, ref params) = fty {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(format!("{}", self.name).as_str(), ft, None);
                    f.set_call_conventions(cconv.unwrap_or(8));
                    if let Some(link) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let var = ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty.clone(),
                        good: Cell::new(true)
                    }))).clone();
                    if !is_extern {
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        {
                            let mut param_count = 0;
                            for (name, (ty, is_const)) in self.params.iter().map(|x| &x.0).zip(params.iter()) {
                                if name.len() == 0 {
                                    if !is_const {
                                        param_count += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(param_count).unwrap();
                                    param.set_name(name.as_str());
                                    ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                        comp_val: Some(param),
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        good: Cell::new(true)
                                    }))).map_or((), |x| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                        comp_val: None,
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        good: Cell::new(true)
                                    }))).map_or((), |x| ());
                                }
                            }
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let (body, mut es) = self.body.codegen(ctx);
                        errs.append(&mut es);
                        ctx.map_vars(|v| v.parent.unwrap());
                        let err = format!("cannot convert value of type {} to {}", body.data_type, *ret);
                        ctx.builder.build_return(Some(&types::utils::impl_convert(body, (&**ret).clone(), ctx).and_then(|v| v.comp_val).unwrap_or_else(|| {
                            errs.push(Error::new(self.loc.clone(), 311, err));
                            llt.const_zero()
                        })));
                    }
                    var
                }
                else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    }))).clone()
                }
            }
            else if **ret == Type::Null {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(format!("{}", self.name).as_str(), ft, None);
                    f.set_call_conventions(cconv.unwrap_or(8));
                    if let Some(link) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let var = ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty.clone(),
                        good: Cell::new(true)
                    }))).clone();
                    if !is_extern {
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        {
                            let mut param_count = 0;
                            for (name, (ty, is_const)) in self.params.iter().map(|x| &x.0).zip(params.iter()) {
                                if name.len() == 0 {
                                    if !is_const {
                                        param_count += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(param_count).unwrap();
                                    param.set_name(name.as_str());
                                    ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                        comp_val: Some(param),
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        good: Cell::new(true)
                                    }))).map_or((), |x| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                        comp_val: None,
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        good: Cell::new(true)
                                    }))).map_or((), |x| ());
                                }
                            }
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let (body, mut es) = self.body.codegen(ctx);
                        errs.append(&mut es);
                        ctx.builder.build_return(None);
                        ctx.map_vars(|v| v.parent.unwrap());
                    }
                    var
                }
                else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    }))).clone()
                }
            }
            else {
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: None,
                    inter_val: Some(InterData::Function(FnData {
                        defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let (val, mut es) = a.codegen(ctx);
                            let err = format!("cannot convert value of type {} to {t}", val.data_type);
                            let val = types::utils::impl_convert(val, t.clone(), ctx);
                            ctx.is_const.set(old_const);
                            errs.append(&mut es);
                            if let Some(val) = val {
                                if let Some(val) = val.inter_val {val}
                                else {
                                    errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                    InterData::Null
                                }
                            }
                            else {
                                errs.push(Error::new(a.loc(), 311, err));
                                InterData::Null
                            }
                        })).collect()
                    })),
                    data_type: fty,
                    good: Cell::new(true)
                }))).clone()
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
        } else {panic!("In order for this to be reachable, fty would have to somehow be mutated, which is impossible")}.clone();
        if !is_extern {
            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
            else {ctx.builder.clear_insertion_position();}
        }
        val
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
        out += format!("fn {}(", self.name).as_str();
        let mut len = self.params.len();
        for (param, param_ty, ty, default) in self.params.iter() {
            out += match param_ty {
                ParamType::Normal => "",
                ParamType::Mutable => "mut ",
                ParamType::Constant => "const "
            };
            out += format!("{}: {}", param, ty).as_str();
            if let Some(val) = default {
                out += format!(" = {}", val.to_code()).as_str();
            }
            if len > 1 {
                out += ", ";
            }
            len -= 1;
        }
        out + format!("): {} = {}", self.ret, self.body.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "function: {}(", self.name)?;
        let mut len = self.params.len(); 
        for (param, param_ty, ty, default) in self.params.iter() {
            write!(f, "{}", match param_ty {
                ParamType::Normal => "",
                ParamType::Mutable => "mut ",
                ParamType::Constant => "const "
            })?;
            write!(f, "{}: {}", param, ty)?;
            if let Some(val) = default {
                write!(f, " = {}", val.to_code())?;
            }
            if len > 1 {
                write!(f, ", ")?;
            }
            len -= 1;
        }
        writeln!(f, "): {}", self.ret)?;
        for (name, arg) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.body, true)
    }
}
pub struct CallAST {
    loc: Location,
    pub target: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>
}
impl CallAST {
    pub fn new(loc: Location, target: Box<dyn AST>, args: Vec<Box<dyn AST>>) -> Self {CallAST {loc, target, args}}
}
impl AST for CallAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Type::Function(ret, _) = self.target.res_type(ctx) {*ret}
        else {Type::Null}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let (val, mut errs) = self.target.codegen(ctx);
        (types::utils::call(val, self.loc.clone(), self.args.iter().map(|a| {
            let (arg, mut es) = a.codegen(ctx);
            errs.append(&mut es);
            (arg, a.loc())
        }).collect(), ctx).unwrap_or_else(|err| {errs.push(err); Variable::error()}), errs)
    }
    fn to_code(&self) -> String {
        let mut out = format!("{}(", self.target.to_code());
        let mut count = self.args.len();
        for arg in self.args.iter() {
            out += arg.to_code().as_str();
            if count > 1 {
                out += ", ";
            }
            count -= 1;
        }
        out + ")"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "call")?;
        let mut count = self.args.len();
        print_ast_child(f, pre, &*self.target, count == 0)?;
        for arg in self.args.iter() {
            print_ast_child(f, pre, &**arg, count <= 1)?;
            count -= 1;
        }
        Ok(())
    }
}
pub struct IntrinsicAST {
    loc: Location,
    pub name: String,
    pub args: Option<String>
}
impl IntrinsicAST {
    pub fn new(loc: Location, name: String, args: Option<String>) -> Self {IntrinsicAST {loc, name, args}}
}
impl AST for IntrinsicAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {Type::Null}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match self.name.as_str() {
            "asm" => todo!("inline assembly isn't yet implemented"),
            x => (Variable::error(), vec![Error::new(self.loc.clone(), 391, format!("unknown intrinsic {x:?}"))])
        }
    }
    fn to_code(&self) -> String {self.name.clone() + self.args.as_ref().map(|x| x.as_str()).unwrap_or("")}
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "intrinsic: {}", self.name)?;
        let mut is_first = true;
        if let Some(params) = self.args.as_ref() {
            for line in params.split('\n') {
                writeln!(f, "{pre}{}{line}", if is_first {"└── "} else {"    "})?;
                is_first = false;
            }
        }
        Ok(())
    }
}
