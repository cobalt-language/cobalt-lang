use crate::*;
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::*};
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
use inkwell::attributes::{Attribute, AttributeLoc::Function};
use glob::Pattern;
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
    pub annotations: Vec<(String, Option<String>, Location)>
}
impl FnDefAST {
    pub fn new(loc: Location, name: DottedName, ret: ParsedType, params: Vec<(String, ParamType, ParsedType, Option<Box<dyn AST>>)>, body: Box<dyn AST>, annotations: Vec<(String, Option<String>, Location)>) -> Self {FnDefAST {loc, name, ret, params, body, annotations}}
}
impl AST for FnDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name, loc)) => {
                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime(loc)) => {
                errs.push(Diagnostic::error(loc, 324, None));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name, loc)) => {
                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                Type::Null
            }
        };
        Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime(loc)) => {
                    errs.push(Diagnostic::error(loc, 324, None));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name, loc)) => {
                errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime(loc)) => {
                errs.push(Diagnostic::error(loc, 324, None));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name, loc)) => {
                errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name, loc)) => {
                errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                Type::Null
            }
        };
        let fty = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 311, Some(format!("cannot convert value of type {name} to u64"))));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime(loc)) => {
                    errs.push(Diagnostic::error(loc, 324, None));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 321, Some(format!("{name} is not a module"))));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name, loc)) => {
                    errs.push(Diagnostic::error(loc, 320, Some(format!("{name} does not exist"))));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect());
        let mut errs = vec![];
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut cconv = None;
        let mut inline = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            match ann.as_str() {
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
                "cconv" => {
                    if let Some((_, prev)) = cconv.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 420, None).note(prev, "previously defined here".to_string()))
                    }
                    cconv = cconv.or(match arg.as_ref().map(|x| x.as_str()) {
                        None => {errs.push(Diagnostic::error(loc.clone(), 421, None)); None},
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
                                Err(_) => {errs.push(Diagnostic::error(loc.clone(), 422, Some(format!("unknown calling convention {x:?}")))); None}
                            }
                        }
                    }.map(|cc| (cc, loc.clone())));
                },
                "extern" => {
                    if let Some(prev) = is_extern.clone() {
                        errs.push(Diagnostic::warning(loc.clone(), 22, None).note(prev, "previously defined here".to_string()))
                    }
                    is_extern = Some(loc.clone());
                    if let Some((_, prev)) = cconv.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 420, None).note(prev, "previously defined here".to_string()))
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
                                Err(_) => {errs.push(Diagnostic::error(loc.clone(), 422, Some(format!("unknown calling convention {x:?}")))); None}
                            }
                        }
                    }.map(|cc| (cc, loc.clone())));
                },
                "inline" => {
                    if let Some((_, prev)) = inline.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 423, None).note(prev, "previous specification here".to_string()))
                    }
                    if let Some(arg) = arg {
                        match arg.as_str() {
                            "always" | "true" | "1" => inline = Some((true, loc.clone())),
                            "never" | "false" | "0" => inline = Some((false, loc.clone())),
                            x => errs.push(Diagnostic::error(loc.clone(), 424, Some(format!("expected 'always' or 'never', got {x:?}"))))
                        }
                    }
                    else {
                        inline = Some((true, loc.clone()))
                    }
                },
                "c" | "C" => {
                    match arg.as_ref().map(|x| x.as_str()) {
                        Some("") | None => {},
                        Some("extern") => {
                            if let Some(prev) = is_extern.clone() {
                                errs.push(Diagnostic::warning(loc.clone(), 22, None).note(prev, "previously defined here".to_string()))
                            }
                            is_extern = Some(loc.clone());
                        },
                        Some(x) => {
                            errs.push(Diagnostic::error(loc.clone(), 425, Some(format!("expected no argument or 'extern' as argument to @C annotation, got {x:?}"))))
                        }
                    }
                    if let Some((_, prev)) = cconv.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 420, None).note(prev, "previously defined here".to_string()))
                    }
                    cconv = Some((0, loc.clone()));
                    if let Some((_, prev)) = linkas.clone() {
                        errs.push(Diagnostic::error(loc.clone(), 416, None).note(prev, "previously defined here".to_string()))
                    }
                    linkas = Some((self.name.ids.last().expect("function name shouldn't be empty!").0.clone(), loc.clone()))
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
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for function definition"))))
            }
        }
        if target_match == 0 {return (Variable::null(), errs)}
        let old_ip = ctx.builder.get_insert_block();
        let val = if let Type::Function(ref ret, ref params) = fty {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| ctx.mangle(&self.name), |v| v.0.clone()).as_str(), ft, None);
                    f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("nobuiltin"), 0));
                    match inline {
                        Some((true, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some((false, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cconv.map_or(8, |(cc, _)| cc));
                    if let Some((link, _)) = link_type {
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
                                        errs.push(Diagnostic::error(a.loc(), 314, None));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Diagnostic::error(a.loc(), 311, Some(err)));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty.clone(),
                        export: true
                    }))).clone();
                    if is_extern.is_none() {
                        let old_scope = ctx.push_scope(&self.name);
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
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol::Variable(Variable {
                                        comp_val: Some(param),
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        export: true
                                    }))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol::Variable(Variable {
                                        comp_val: None,
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        export: true
                                    }))).map_or((), |_| ());
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
                            errs.push(Diagnostic::error(self.body.loc(), 311, Some(err)));
                            llt.const_zero()
                        })));
                        ctx.restore_scope(old_scope);
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
                                        errs.push(Diagnostic::error(a.loc(), 314, None));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Diagnostic::error(a.loc(), 311, Some(err)));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        export: true
                    }))).clone()
                }
            }
            else if **ret == Type::Null {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| ctx.mangle(&self.name), |v| v.0.clone()).as_str(), ft, None);
                    f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("nobuiltin"), 0));
                    match inline {
                        Some((true, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some((false, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cconv.map_or(8, |(cc, _)| cc));
                    if let Some((link, _)) = link_type {
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
                                        errs.push(Diagnostic::error(a.loc(), 314, None));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Diagnostic::error(a.loc(), 311, Some(err)));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty.clone(),
                        export: true
                    }))).clone();
                    if is_extern.is_none() {
                        let old_scope = ctx.push_scope(&self.name);
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
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol::Variable(Variable {
                                        comp_val: Some(param),
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        export: true
                                    }))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol::Variable(Variable {
                                        comp_val: None,
                                        inter_val: None,
                                        data_type: ty.clone(),
                                        export: true
                                    }))).map_or((), |_| ());
                                }
                            }
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let (_, mut es) = self.body.codegen(ctx);
                        errs.append(&mut es);
                        ctx.builder.build_return(None);
                        ctx.map_vars(|v| v.parent.unwrap());
                        ctx.restore_scope(old_scope);
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
                                        errs.push(Diagnostic::error(a.loc(), 314, None));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Diagnostic::error(a.loc(), 311, Some(err)));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        export: true
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
                                    errs.push(Diagnostic::error(a.loc(), 314, None));
                                    InterData::Null
                                }
                            }
                            else {
                                errs.push(Diagnostic::error(a.loc(), 311, Some(err)));
                                InterData::Null
                            }
                        })).collect()
                    })),
                    data_type: fty,
                    export: true
                }))).clone()
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
        } else {panic!("In order for this to be reachable, fty would have to somehow be mutated, which is impossible")}.clone();
        if is_extern.is_none() {
            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
            else {ctx.builder.clear_insertion_position();}
        }
        val
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()).as_str() + " ").to_string()) {out += s.as_str();}
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
        for (name, arg, _) in self.annotations.iter() {
            writeln!(f, "{pre}├── @{name}{}", arg.as_ref().map(|x| format!("({x})")).unwrap_or("".to_string()))?;
        }
        print_ast_child(f, pre, &*self.body, true)
    }
}
pub struct CallAST {
    loc: Location,
    pub cparen: Location,
    pub target: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>
}
impl CallAST {
    pub fn new(loc: Location, cparen: Location, target: Box<dyn AST>, args: Vec<Box<dyn AST>>) -> Self {CallAST {loc, cparen, target, args}}
}
impl AST for CallAST {
    fn loc(&self) -> Location {(self.loc.0, self.loc.1.start..self.cparen.1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.target.res_type(ctx) {
            Type::Function(ret, _) => *ret,
            Type::InlineAsm => Type::Null,
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        let (val, mut errs) = self.target.codegen(ctx);
        (types::utils::call(val, self.loc.clone(), self.cparen.clone(), self.args.iter().map(|a| {
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
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {if self.name == "asm" {Type::InlineAsm} else {Type::Null}}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Diagnostic>) {
        match self.name.as_str() {
            "asm" => {
                if let Some(ref args) = self.args {
                    if let Some(idx) = args.find(';') {
                        (Variable::metaval(InterData::InlineAsm(args[..idx].to_string(), args[(idx + 1)..].to_string()), Type::InlineAsm), vec![])
                    }
                    else {
                        (Variable::error(), vec![Diagnostic::error(self.loc.clone(), 431, None)])
                    }
                }
                else {
                    (Variable::error(), vec![Diagnostic::error(self.loc.clone(), 430, None)])
                }
            },
            x => (Variable::error(), vec![Diagnostic::error(self.loc.clone(), 391, Some(format!("unknown intrinsic {x:?}")))])
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
