use crate::*;
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::*};
use inkwell::values::BasicValueEnum::*;
use inkwell::module::Linkage::*;
use inkwell::attributes::{Attribute, AttributeLoc::Function};
use std::collections::LinkedList;
use glob::Pattern;
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Normal,
    Mutable,
    Constant
}
type Parameter = (String, ParamType, Box<dyn AST>, Option<Box<dyn AST>>); // parameter, mut/const, type, default
pub struct FnDefAST {
    loc: Location,
    pub name: DottedName,
    pub ret: Box<dyn AST>,
    pub params: Vec<Parameter>,
    pub body: Box<dyn AST>,
    pub annotations: Vec<(String, Option<String>, Location)>
}
impl FnDefAST {
    pub fn new(loc: Location, name: DottedName, ret: Box<dyn AST>, params: Vec<Parameter>, body: Box<dyn AST>, annotations: Vec<(String, Option<String>, Location)>) -> Self {FnDefAST {loc, name, ret, params, body, annotations}}
}
impl AST for FnDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        let oic = ctx.is_const.replace(true);
        let ret = if let Ok(Value {inter_val: Some(InterData::Type(t)), data_type: Type::TypeData, ..}) = types::utils::impl_convert((0, 0..0), (self.ret.codegen(ctx).0, None), (Type::TypeData, None), ctx) {*t} else {Type::Error};
        let out = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| (if let Ok(Value {inter_val: Some(InterData::Type(t)), data_type: Type::TypeData, ..}) = types::utils::impl_convert((0, 0..0), (ty.codegen(ctx).0, None), (Type::TypeData, None), ctx) {*t} else {Type::Error}, pt == &ParamType::Constant)).collect());
        ctx.is_const.set(oic);
        out
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let oic = ctx.is_const.replace(true);
        let ret = types::utils::impl_convert(self.ret.loc(), (self.ret.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {
            errs.push(e);
            Type::Error
        }, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        let fty = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| (types::utils::impl_convert(ty.loc(), (ty.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {
            errs.push(e);
            Type::Error
        }, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error}), pt == &ParamType::Constant)).collect());
        ctx.is_const.set(oic);
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut cconv = None;
        let mut inline = None;
        let mut vis_spec = None;
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
                x => errs.push(Diagnostic::error(loc.clone(), 410, Some(format!("unknown annotation {x:?} for function definition"))))
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        let cf = ctx.is_cfunc(&self.name);
        let cc = cconv.map_or(if cf {0} else {8}, |(cc, _)| cc);
        if target_match == 0 {return (Value::null(), errs)}
        let old_ip = ctx.builder.get_insert_block();
        let val = if let Type::Function(ref ret, ref params) = fty {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None);
                    match inline {
                        Some((true, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some((false, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    if let Some((link, _)) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let var = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        Some(PointerValue(f.as_global_value().as_pointer_value())),
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                match val {
                                    Ok(val) => 
                                        if let Some(val) = val.inter_val {val}
                                        else {
                                            errs.push(Diagnostic::error(a.loc(), 314, None));
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc
                        })),
                        fty.clone(),
                    ), VariableData::with_vis(self.loc.clone(), vs)))).clone();
                    if is_extern.is_none() {
                        let old_scope = ctx.push_scope(&self.name);
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        {
                            let mut param_count = 0;
                            for (name, (ty, is_const)) in self.params.iter().map(|x| &x.0).zip(params.iter()) {
                                if name.is_empty() {
                                    if !is_const {
                                        param_count += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(param_count).unwrap();
                                    param.set_name(name.as_str());
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol(Value::new(
                                        Some(param),
                                        None,
                                        ty.clone(),
                                    ), VariableData::default()))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol(Value::new(
                                        None,
                                        None,
                                        ty.clone(),
                                    ), VariableData::default()))).map_or((), |_| ());
                                }
                            }
                        }
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let (body, mut es) = self.body.codegen(ctx);
                        errs.append(&mut es);
                        ctx.map_vars(|v| v.parent.unwrap());
                        ctx.builder.build_return(Some(&types::utils::impl_convert(self.body.loc(), (body, None), ((**ret).clone(), None), ctx).map_err(|e| errs.push(e)).ok().and_then(|v| v.value(ctx)).unwrap_or(llt.const_zero())));
                        ctx.restore_scope(old_scope);
                    }
                    var
                }
                else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        None,
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                match val {
                                    Ok(val) => 
                                        if let Some(val) = val.inter_val {val}
                                        else {
                                            errs.push(Diagnostic::error(a.loc(), 314, None));
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc
                        })),
                        fty
                    ), VariableData::with_vis(self.loc.clone(), vs)))).clone()
                }
            }
            else if **ret == Type::Null {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None);
                    match inline {
                        Some((true, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some((false, _)) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    if let Some((link, _)) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let var = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        Some(PointerValue(f.as_global_value().as_pointer_value())),
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                match val {
                                    Ok(val) => 
                                        if let Some(val) = val.inter_val {val}
                                        else {
                                            errs.push(Diagnostic::error(a.loc(), 314, None));
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc
                        })),
                        fty.clone()
                    ), VariableData::with_vis(self.loc.clone(), vs)))).clone();
                    if is_extern.is_none() {
                        let old_scope = ctx.push_scope(&self.name);
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        {
                            let mut param_count = 0;
                            for (name, (ty, is_const)) in self.params.iter().map(|x| &x.0).zip(params.iter()) {
                                if name.is_empty() {
                                    if !is_const {
                                        param_count += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(param_count).unwrap();
                                    param.set_name(name.as_str());
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol(Value::new(
                                        Some(param),
                                        None,
                                        ty.clone()
                                    ), VariableData::default()))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), (0, 0..0))), Symbol(Value::new(
                                        None,
                                        None,
                                        ty.clone()
                                    ), VariableData::default()))).map_or((), |_| ());
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
                    ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        None,
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                match val {
                                    Ok(val) => 
                                        if let Some(val) = val.inter_val {val}
                                        else {
                                            errs.push(Diagnostic::error(a.loc(), 314, None));
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc
                        })),
                        fty
                    ), VariableData::with_vis(self.loc.clone(), vs)))).clone()
                }
            }
            else {
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    None,
                    Some(InterData::Function(FnData {
                        defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let (val, mut es) = a.codegen(ctx);
                            let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                            ctx.is_const.set(old_const);
                            errs.append(&mut es);
                            match val {
                                Ok(val) => 
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Diagnostic::error(a.loc(), 314, None));
                                        InterData::Null
                                    }
                                Err(e) => {
                                    errs.push(e);
                                    InterData::Null
                                }
                            }
                        })).collect(),
                        cconv: cc
                    })),
                    fty
                ), VariableData::with_vis(self.loc.clone(), vs)))).clone()
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
        } else {panic!("In order for this to be reachable, fty would have to somehow be mutated, which is impossible")}.clone();
        if is_extern.is_none() {
            if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
            else {ctx.builder.clear_insertion_position();}
        }
        val
    }
    fn to_code(&self) -> String {
        let mut out = "".to_string();
        for s in self.annotations.iter().map(|(name, arg, _)| ("@".to_string() + name.as_str() + arg.as_ref().map(|x| format!("({x})")).unwrap_or_default().as_str() + " ")) {out += s.as_str();}
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
        writeln!(f, "function: {}", self.name)?;
        writeln!(f, "{pre}├── annotations:")?;
        pre.push(false);
        for (n, (name, arg, _)) in self.annotations.iter().enumerate() {
            writeln!(f, "{pre}{}@{name}{}", if n + 1 < self.annotations.len() {"├── "} else {"└── "}, arg.as_ref().map(|x| format!("({x})")).unwrap_or_default())?;
        }
        pre.pop();
        writeln!(f, "{pre}├── parameters:")?;
        pre.push(false);
        for (n, (param, param_ty, ty, default)) in self.params.iter().enumerate() {
            writeln!(f, "{pre}{}{}{}", if n + 1 < self.params.len() {"├── "} else {"└── "}, match param_ty {
                ParamType::Normal => "",
                ParamType::Mutable => "mut ",
                ParamType::Constant => "const "
            }, param)?;
            pre.push(n + 1 == self.params.len());
            if let Some(val) = default {
                write!(f, "{pre}├── type: ")?;
                pre.push(false);
                ty.print_impl(f, pre)?;
                pre.pop();
                write!(f, "{pre}└── default: ")?;
                pre.push(true);
                val.print_impl(f, pre)?;
                pre.pop();
            }
            else {
                write!(f, "{pre}└── type: ")?;
                pre.push(true);
                ty.print_impl(f, pre)?;
                pre.pop();
            }
            pre.pop();
        }
        pre.pop();
        write!(f, "{pre}├── return: ")?;
        pre.push(false);
        self.ret.print_impl(f, pre)?;
        pre.pop();
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
    fn expl_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> bool {matches!(self.target.res_type(ctx), Type::InlineAsm(..))}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        types::utils::call_type(self.target.res_type(ctx), self.args.iter().map(|a| a.const_codegen(ctx).0).collect::<Vec<_>>())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let (val, mut errs) = self.target.codegen(ctx);
        (types::utils::call(val, self.loc.clone(), self.cparen.clone(), self.args.iter().map(|a| {
            let (arg, mut es) = a.codegen(ctx);
            errs.append(&mut es);
            (arg, a.loc())
        }).collect(), ctx).unwrap_or_else(|err| {errs.push(err); Value::error()}), errs)
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
    pub args: Vec<Box<dyn AST>>
}
impl IntrinsicAST {
    pub fn new(loc: Location, name: String, args: Vec<Box<dyn AST>>) -> Self {IntrinsicAST {loc, name, args}}
}
impl AST for IntrinsicAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        match self.name.as_str() {
            "asm" => Type::InlineAsm(self.args.first().map_or(Box::new(Type::Null), |v| {
                if let Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(t)), ..} = v.const_codegen(ctx).0 {t}
                else {Box::new(Type::Null)}
            })),
            _ => Type::Error
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match self.name.as_str() {
            "asm" => {
                let oic = ctx.is_const.replace(true);
                let mut errs = vec![];
                let mut args = self.args.iter().map(|a| a.codegen_errs(ctx, &mut errs)).collect::<LinkedList<_>>();
                ctx.is_const.set(oic);
                match args.len() {
                    2 => {
                        let a0 = args.pop_front().unwrap();
                        let a1 = args.pop_front().unwrap();
                        if is_str(&a0.data_type) && is_str(&a1.data_type) {
                            match (a0, a1) {
                                (
                                    Value {inter_val: Some(InterData::Str(c)), ..},
                                    Value {inter_val: Some(InterData::Str(b)), ..}
                                ) => (Value::metaval(InterData::InlineAsm(c, b), Type::InlineAsm(Box::new(Type::Null))), errs),
                                (a0, a1) => {
                                    errs.push(Diagnostic::error(self.loc.clone(), 430, None)
                                        .note(self.args[0].loc(), format!("first argument type is {} ({})", a0.data_type, if a0.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                        .note(self.args[1].loc(), format!("second argument type is {} ({})", a1.data_type, if a1.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                        .info("both arguments should be constant strings (i8 const*)".to_string()));
                                    (Value::metaval(InterData::InlineAsm(String::new(), String::new()), Type::InlineAsm(Box::new(Type::Null))), errs)
                                }
                            }
                        }
                        else {
                            errs.push(Diagnostic::error(self.loc.clone(), 430, None)
                                .note(self.args[0].loc(), format!("first argument type is {} ({})", a0.data_type, if a0.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                .note(self.args[1].loc(), format!("second argument type is {} ({})", a1.data_type, if a1.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                .info("both arguments should be constant strings (i8 const*)".to_string()));
                            (Value::metaval(InterData::InlineAsm(String::new(), String::new()), Type::InlineAsm(Box::new(Type::Null))), errs)
                        }
                    },
                    3 => {
                        let a0 = args.pop_front().unwrap();
                        let a1 = args.pop_front().unwrap();
                        let a2 = args.pop_front().unwrap();
                        if let Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(r)), ..} = a0 {
                            if is_str(&a1.data_type) && is_str(&a2.data_type) {
                                match (a1, a2) {
                                    (
                                        Value {inter_val: Some(InterData::Str(c)), ..},
                                        Value {inter_val: Some(InterData::Str(b)), ..}
                                    ) => (Value::metaval(InterData::InlineAsm(c, b), Type::InlineAsm(r)), errs),
                                    (a1, a2) => {
                                        errs.push(Diagnostic::error(self.loc.clone(), 430, None)
                                            .note(self.args[1].loc(), format!("second argument type is {} ({})", a1.data_type, if a1.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                            .note(self.args[2].loc(), format!("third argument type is {} ({})", a2.data_type, if a2.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                            .info("arguments should be a type, then two constant strings (i8 const*)".to_string()));
                                        (Value::metaval(InterData::InlineAsm(String::new(), String::new()), Type::InlineAsm(r)), errs)
                                    }
                                }
                            }
                            else {
                                errs.push(Diagnostic::error(self.loc.clone(), 430, None)
                                    .note(self.args[1].loc(), format!("second argument type is {} ({})", a1.data_type, if a1.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                    .note(self.args[2].loc(), format!("third argument type is {} ({})", a2.data_type, if a2.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                    .info("arguments should be a type, then two constant strings (i8 const*)".to_string()));
                                (Value::metaval(InterData::InlineAsm(String::new(), String::new()), Type::InlineAsm(r)), errs)
                            }
                        }
                        else {
                            errs.push(Diagnostic::error(self.loc.clone(), 430, None)
                                .note(self.args[0].loc(), format!("first argument type is {} ({})", a0.data_type, if a0.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                .note(self.args[1].loc(), format!("second argument type is {} ({})", a1.data_type, if a1.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                .note(self.args[2].loc(), format!("third argument type is {} ({})", a2.data_type, if a2.inter_val.is_some() {"constant"} else {"runtime-only"}))
                                .info("arguments should be a type, then two constant strings (i8 const*)".to_string()));
                            (Value::metaval(InterData::InlineAsm(String::new(), String::new()), Type::InlineAsm(Box::new(Type::Null))), errs)
                        }
                    },
                    x => {
                        errs.push(Diagnostic::error(self.loc.clone(), 430, Some(format!("expected 2 or 3 arguments, got {x}")))
                            .info("acceptable forms are:".to_string())
                            .info("constraint, body".to_string())
                            .info("return, constraint, body".to_string()));
                        (Value::error(), errs)
                    }
                }
            },
            "alloca" => {
                let mut errs = vec![];
                let mut args = self.args.iter().map(|a| a.codegen_errs(ctx, &mut errs)).collect::<LinkedList<_>>();
                if args.is_empty() {return (Value::error(), vec![Diagnostic::error(self.loc.clone(), 435, None)]);}
                let ty = if args.front().unwrap().data_type == Type::TypeData {if let Some(InterData::Type(t)) = args.pop_front().unwrap().inter_val {Some(t)} else {None}} else {None};
                if args.is_empty() {
                    if let Some(ty) = ty {
                        if let Some(llt) = ty.llvm_type(ctx) {
                            (Value::compiled(ctx.builder.build_alloca(llt, "").into(), Type::Pointer(ty, true)), vec![])
                        }
                        else {
                            (Value::new(None, None, Type::Pointer(Box::new(Type::Null), true)), vec![Diagnostic::error(self.loc.clone(), 431, Some(format!("type is {}", *ty)))])
                        }
                    }
                    else {
                        unreachable!()
                    }
                }
                else {
                    let mut val = None;
                    for (n, mut arg) in args.into_iter().enumerate() {
                        loop {
                            match arg.data_type {
                                Type::Borrow(b) => arg.data_type = *b,
                                Type::Reference(b, _) => {
                                    if b.register() && !ctx.is_const.get() {
                                        if let Some(PointerValue(v)) = arg.comp_val {
                                            arg.comp_val = Some(ctx.builder.build_load(v, ""));
                                        }
                                    }
                                    arg.data_type = *b;
                                },
                                x @ (Type::Int(..) | Type::IntLiteral) => {
                                    arg.data_type = x;
                                    if !ctx.is_const.get() {
                                        if let Some(IntValue(v)) = arg.value(ctx) {
                                            if let Some(v2) = val {
                                                val = Some(ctx.builder.build_int_mul(v, v2, ""));
                                            }
                                            else {
                                                val = Some(v);
                                            }
                                        }
                                    }
                                    break;
                                },
                                x => {
                                    errs.push(Diagnostic::error(self.args[n + usize::from(ty.is_some())].loc(), 434, Some(format!("argument type is {x}"))));
                                    break;
                                }
                            }
                        }
                    }
                    if let Some(ty) = ty {
                        if let Some(llt) = ty.llvm_type(ctx) {
                            (Value::compiled(ctx.builder.build_array_alloca(llt, val.unwrap(), "").into(), Type::Pointer(ty, true)), errs)
                        }
                        else {
                            errs.push(Diagnostic::error(self.loc.clone(), 431, Some(format!("type is {}", *ty))));
                            (Value::new(None, None, Type::Pointer(ty, true)), errs)
                        }
                    }
                    else {
                        (Value::compiled(ctx.builder.build_array_alloca(ctx.context.i8_type(), val.unwrap(), "").into(), Type::Pointer(Box::new(Type::Null), true)), errs)
                    }
                }
            },
            x => (Value::error(), vec![Diagnostic::error(self.loc.clone(), 391, Some(format!("unknown intrinsic {x:?}")))])
        }
    }
    fn to_code(&self) -> String {
        let mut out = format!("{}(", self.name);
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
        let mut count = self.args.len();
        writeln!(f, "intrinsic: {}", self.name)?;
        for arg in self.args.iter() {
            print_ast_child(f, pre, &**arg, count <= 1)?;
            count -= 1;
        }
        Ok(())
    }
}
fn is_str(ty: &Type) -> bool {
    match ty {
        Type::Pointer(b, _) => **b == Type::Int(8, false),
        Type::Reference(b, _) => if let Type::Array(ref b, _) = **b {**b == Type::Int(8, false)} else {false}
        _ => false
    }
}
