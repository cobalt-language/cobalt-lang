use crate::*;
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::*};
use inkwell::values::{AsValueRef, FunctionValue, BasicValueEnum::*};
use inkwell::module::Linkage::*;
use inkwell::attributes::{Attribute, AttributeLoc::Function};
use llvm_sys::core::{LLVMGetInsertBlock, LLVMIsABasicBlock};
use llvm_sys::prelude::LLVMValueRef;
use glob::Pattern;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Normal,
    Mutable,
    Constant
}
pub type Parameter = (String, ParamType, Box<dyn AST>, Option<Box<dyn AST>>); // parameter, mut/const, type, default
#[derive(Debug, Clone)]
pub struct FnDefAST {
    loc: SourceSpan,
    pub name: DottedName,
    pub ret: Box<dyn AST>,
    pub params: Vec<Parameter>,
    pub body: Box<dyn AST>,
    pub annotations: Vec<(String, Option<String>, SourceSpan)>,
    pub in_struct: bool
}
impl FnDefAST {
    pub fn new(loc: SourceSpan, name: DottedName, ret: Box<dyn AST>, params: Vec<Parameter>, body: Box<dyn AST>, annotations: Vec<(String, Option<String>, SourceSpan)>, in_struct: bool) -> Self {FnDefAST {loc, name, ret, params, body, annotations, in_struct}}
}
impl AST for FnDefAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn nodes(&self) -> usize {self.ret.nodes() + self.body.nodes() + self.params.iter().map(|(_, _, ty, def)| ty.nodes() + def.as_ref().map_or(0, |x| x.nodes())).sum::<usize>() + 1}
    fn fwddef_prepass(&self, ctx: &CompCtx) {
        let oic = ctx.is_const.replace(true);
        let ret = types::utils::impl_convert(unreachable_span(), (self.ret.codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error);
        let params = self.params.iter().map(|(_, pt, ty, _)| (types::utils::impl_convert(unreachable_span(), (ty.codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error), pt == &ParamType::Constant)).collect::<Vec<_>>();
        ctx.is_const.set(oic);
        let mut link_type = None;
        let mut linkas = None;
        let mut cconv = None;
        let mut inline = None;
        let mut vis_spec = None;
        let mut fn_type = None;
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
                    }
                },
                "linkas" => {
                    if let Some(arg) = arg {
                        linkas = Some((arg.clone(), loc))
                    }
                },
                "cconv" => {
                    cconv = cconv.or(match arg.as_ref().map(|x| x.as_str()) {
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
                        _ => None
                    });
                },
                "extern" => {
                    cconv = cconv.or(match arg.as_ref().map(|x| x.as_str()) {
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
                        _ => None
                    });
                },
                "inline" => {
                    if let Some(arg) = arg {
                        match arg.as_str() {
                            "always" | "true" | "1" => inline = Some(true),
                            "never" | "false" | "0" => inline = Some(false),
                            _ => {}
                        }
                    }
                    else {
                        inline = Some(true)
                    }
                },
                "c" | "C" => {
                    cconv = Some(0);
                    linkas = Some((self.name.ids.last().expect("function name shouldn't be empty!").0.clone(), loc))
                },
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = arg.as_str();
                        let negate = if arg.as_bytes().first() == Some(&0x21) {arg = &arg[1..]; true} else {false};
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {target_match = u8::from(negate ^ pat.matches(&ctx.module.get_triple().as_str().to_string_lossy()))}
                        }
                    }
                },
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            _ => {}
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
                "method" if self.in_struct => {
                    if fn_type.is_none() && !params.is_empty() {
                        let self_t = Type::Reference(Box::new(ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap()).clone()), true);
                        if types::utils::impl_convertible(self_t, params[0].0.clone()) {fn_type = Some(MethodType::Normal)};
                    }
                },
                "getter" if self.in_struct => {
                    if fn_type.is_none() && !params.is_empty() {
                        let self_t = Type::Reference(Box::new(ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap()).clone()), true);
                        if types::utils::impl_convertible(self_t, params[0].0.clone()) {fn_type = Some(MethodType::Getter)};
                    }
                },
                _ => {}
            }
        }
        let fty = Type::Function(Box::new(ret), params);
        let vs = vis_spec.unwrap_or(ctx.export.get());
        let cf = ctx.is_cfunc(&self.name);
        let cc = cconv.unwrap_or(if cf {0} else {8});
        let mt = fn_type.unwrap_or(MethodType::Static);
        if target_match == 0 {return}
        if let Type::Function(ref ret, ref params) = fty {
            if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None);
                    match inline {
                        Some(true) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some(false) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    if let Some(link) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        Some(PointerValue(f.as_global_value().as_pointer_value())),
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx).0;
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                val.ok().and_then(|v| v.inter_val).unwrap_or(InterData::Null)
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty.clone(),
                    ), VariableData {fwd: true, ..VariableData::with_vis(self.loc, vs)})));
                }
            }
            else if ret.size(ctx) == SizeType::Static(0) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None);
                    match inline {
                        Some(true) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0)),
                        Some(false) => f.add_attribute(Function, ctx.context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0)),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    if let Some(link) = link_type {
                        f.as_global_value().set_linkage(link)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                        Some(PointerValue(f.as_global_value().as_pointer_value())),
                        Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx).0;
                                let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                                ctx.is_const.set(old_const);
                                val.ok().and_then(|v| v.inter_val).unwrap_or(InterData::Null)
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty.clone(),
                    ), VariableData {fwd: true, ..VariableData::with_vis(self.loc, vs)})));
                }
            }
            else {
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                let _ = ctx.with_vars(|v| v.insert(&self.name, Symbol(Value::new(
                    None,
                    Some(InterData::Function(FnData {
                        defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let val = a.codegen(ctx).0;
                            let val = types::utils::impl_convert(a.loc(), (val, None), (t.clone(), None), ctx);
                            ctx.is_const.set(old_const);
                            val.ok().and_then(|v| v.inter_val).unwrap_or(InterData::Null)
                        })).collect(),
                        cconv: cc,
                        mt
                    })),
                    fty
                ), VariableData {fwd: true, ..VariableData::with_vis(self.loc, vs)})));
            }
        }
        else {unreachable!()};
    }
    fn res_type(&self, ctx: &CompCtx) -> Type {
        let oic = ctx.is_const.replace(true);
        let ret = types::utils::impl_convert(unreachable_span(), (self.ret.codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error);
        let out = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| (types::utils::impl_convert(unreachable_span(), (ty.codegen(ctx).0, None), (Type::TypeData, None), ctx).ok().and_then(Value::into_type).unwrap_or(Type::Error), pt == &ParamType::Constant)).collect());
        ctx.is_const.set(oic);
        out
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let oic = ctx.is_const.replace(true);
        let ret = types::utils::impl_convert(self.ret.loc(), (self.ret.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {
            errs.push(e);
            Type::Error
        }, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error});
        let params = self.params.iter().map(|(_, pt, ty, _)| (types::utils::impl_convert(ty.loc(), (ty.codegen_errs(ctx, &mut errs), None), (Type::TypeData, None), ctx).map_or_else(|e| {
            errs.push(e);
            Type::Error
        }, |v| if let Some(InterData::Type(t)) = v.inter_val {*t} else {Type::Error}), pt == &ParamType::Constant)).collect::<Vec<_>>();
        ctx.is_const.set(oic);
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut cconv = None;
        let mut inline = None;
        let mut vis_spec = None;
        let mut fn_type = None;
        let mut target_match = 2u8;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match ann.as_str() {
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
                "cconv" => {
                    if let Some((_, prev)) = cconv {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "cconv",
                            loc, prev
                        });
                    }
                    cconv = cconv.or(match arg.as_deref() {
                        None => {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "cconv",
                                found: None,
                                expected: Some("calling convention"),
                                loc
                            });
                            None
                        },
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
                            match x.parse() {
                                Ok(v) => Some(v),
                                Err(_) => {
                                    errs.push(CobaltError::InvalidAnnArgument {
                                        name: "cconv",
                                        found: arg.clone(),
                                        expected: Some("calling convention"),
                                        loc
                                    });
                                    None
                                }
                            }
                        }
                    }.map(|cc| (cc, loc)));
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
                    if let Some((_, prev)) = cconv {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "cconv",
                            loc, prev
                        });
                    }
                    cconv = cconv.or(match arg.as_deref() {
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
                            match x.parse() {
                                Ok(v) => Some(v),
                                Err(_) => {
                                    errs.push(CobaltError::InvalidAnnArgument {
                                        name: "cconv",
                                        found: None,
                                        expected: Some("calling convention"),
                                        loc
                                    });
                                    None
                                }
                            }
                        }
                    }.map(|cc| (cc, loc)));
                },
                "inline" => {
                    if let Some((_, prev)) = inline {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "inline",
                            loc, prev
                        });
                    }
                    match arg.as_deref() {
                        Some("always" | "true" | "1") | None => inline = Some((true, loc)),
                        Some("never" | "false" | "0") => inline = Some((false, loc)),
                        _ => errs.push(CobaltError::InvalidAnnArgument {
                            name: "cconv",
                            found: arg.clone(),
                            expected: Some("always or never"),
                            loc
                        })
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
                "method" if self.in_struct => {
                    if let Some((_, prev)) = fn_type {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "method",
                            loc, prev
                        });
                    }
                    else {
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "method",
                                found: arg.clone(),
                                expected: None,
                                loc
                            });
                        }
                        let self_t = Type::Reference(Box::new(ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap()).clone()), true);
                        if params.is_empty() {
                            errs.push(CobaltError::InvalidSelfParam {
                                loc: self.loc,
                                self_t: self_t.to_string(),
                                param: None,
                            });
                        }
                        else {
                            let s = self_t.to_string();
                            if !types::utils::impl_convertible(self_t, params[0].0.clone()) {
                                errs.push(CobaltError::InvalidSelfParam {
                                    loc: self.params[0].2.loc(),
                                    self_t: s,
                                    param: Some(params[0].0.to_string())
                                });
                            }
                            else {
                                fn_type = Some((MethodType::Normal, loc));
                            }
                        }
                    }
                },
                "getter" if self.in_struct => {
                    if let Some((_, prev)) = fn_type {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "getter",
                            loc, prev
                        });
                    }
                    else {
                        if arg.is_some() {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "getter",
                                found: arg.clone(),
                                expected: None,
                                loc
                            });
                        }
                        let self_t = Type::Reference(Box::new(ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap()).clone()), true);
                        if params.is_empty() {
                            errs.push(CobaltError::InvalidSelfParam {
                                loc: self.loc,
                                self_t: self_t.to_string(),
                                param: None,
                            });
                        }
                        else {
                            let s = self_t.to_string();
                            if !types::utils::impl_convertible(self_t, params[0].0.clone()) {
                                errs.push(CobaltError::InvalidSelfParam {
                                    loc: self.params[0].2.loc(),
                                    self_t: s,
                                    param: Some(params[0].0.to_string())
                                });
                            }
                            else {
                                fn_type = Some((MethodType::Getter, loc));
                            }
                        }
                    }
                },
                _ => errs.push(CobaltError::UnknownAnnotation {loc, name: ann.clone(), def: "function"})
            }
        }
        let fty = Type::Function(Box::new(ret), params);
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        let cf = ctx.is_cfunc(&self.name);
        let cc = cconv.map_or(if cf {0} else {8}, |(cc, _)| cc);
        let mt = fn_type.map_or(MethodType::Static, |v| v.0);
        if target_match == 0 {return (Value::null(), errs)}
        let old_ip = unsafe {
            let bb = LLVMGetInsertBlock(ctx.builder.as_mut_ptr());
            if bb.is_null() || !LLVMIsABasicBlock(bb as LLVMValueRef).is_null() {None}
            else {Some(std::mem::transmute::<_, BasicBlock>(bb))} // BasicBlock::new is pub(crate)
        };
        let val = if let Type::Function(ref ret, ref params) = fty {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.lookup_full(&self.name).and_then(|x| -> Option<FunctionValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None));
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
                                            errs.push(CobaltError::NotCompileTime {loc: a.loc()});
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty.clone(),
                    ), VariableData::with_vis(self.loc, vs)))).clone();
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
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), unreachable_span())), Symbol(Value::new(
                                        Some(param),
                                        None,
                                        ty.clone(),
                                    ), VariableData::default()))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), unreachable_span())), Symbol(Value::new(
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
                                            errs.push(CobaltError::NotCompileTime {loc: a.loc()});
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty
                    ), VariableData::with_vis(self.loc, vs)))).clone()
                }
            }
            else if ret.size(ctx) == SizeType::Static(0) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.lookup_full(&self.name).and_then(|x| -> Option<FunctionValue> {Some(unsafe {std::mem::transmute(x.comp_val?.as_value_ref())})}).unwrap_or_else(|| ctx.module.add_function(linkas.map_or_else(|| if cf {self.name.ids.last().unwrap().0.clone()} else {ctx.mangle(&self.name)}, |v| v.0).as_str(), ft, None));
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
                                            errs.push(CobaltError::NotCompileTime {loc: a.loc()});
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty.clone()
                    ), VariableData::with_vis(self.loc, vs)))).clone();
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
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), unreachable_span())), Symbol(Value::new(
                                        Some(param),
                                        None,
                                        ty.clone()
                                    ), VariableData::default()))).map_or((), |_| ());
                                    param_count += 1;
                                }
                                else {
                                    ctx.with_vars(|v| v.insert(&DottedName::local((name.clone(), unreachable_span())), Symbol(Value::new(
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
                                            errs.push(CobaltError::NotCompileTime {loc: a.loc()});
                                            InterData::Null
                                        }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })).collect(),
                            cconv: cc,
                            mt
                        })),
                        fty
                    ), VariableData::with_vis(self.loc, vs)))).clone()
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
                                        errs.push(CobaltError::NotCompileTime {loc: a.loc()});
                                        InterData::Null
                                    }
                                Err(e) => {
                                    errs.push(e);
                                    InterData::Null
                                }
                            }
                        })).collect(),
                        cconv: cc,
                        mt
                    })),
                    fty
                ), VariableData::with_vis(self.loc, vs)))).clone()
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
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
                ty.print_impl(f, pre, file)?;
                pre.pop();
                write!(f, "{pre}└── default: ")?;
                pre.push(true);
                val.print_impl(f, pre, file)?;
                pre.pop();
            }
            else {
                write!(f, "{pre}└── type: ")?;
                pre.push(true);
                ty.print_impl(f, pre, file)?;
                pre.pop();
            }
            pre.pop();
        }
        pre.pop();
        write!(f, "{pre}├── return: ")?;
        pre.push(false);
        self.ret.print_impl(f, pre, file)?;
        pre.pop();
        print_ast_child(f, pre, &*self.body, true, file)
    }
}
#[derive(Debug, Clone)]
pub struct CallAST {
    pub cparen: SourceSpan,
    pub target: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>
}
impl CallAST {
    pub fn new(cparen: SourceSpan, target: Box<dyn AST>, args: Vec<Box<dyn AST>>) -> Self {CallAST {cparen, target, args}}
}
impl AST for CallAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.target.loc(), self.cparen)}
    fn nodes(&self) -> usize {self.target.nodes() + self.args.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn expl_type(&self, ctx: &CompCtx) -> bool {matches!(self.target.res_type(ctx), Type::InlineAsm(..))}
    fn res_type(&self, ctx: &CompCtx) -> Type {
        types::utils::call_type(self.target.res_type(ctx), self.args.iter().map(|a| a.const_codegen(ctx).0).collect::<Vec<_>>())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let (val, mut errs) = self.target.codegen(ctx);
        (types::utils::call(val, self.target.loc(), Some(self.cparen), self.args.iter().map(|a| {
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "call")?;
        let mut count = self.args.len();
        print_ast_child(f, pre, &*self.target, count == 0, file)?;
        for arg in self.args.iter() {
            print_ast_child(f, pre, &**arg, count <= 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct IntrinsicAST {
    loc: SourceSpan,
    pub name: String,
}
impl IntrinsicAST {
    pub fn new(loc: SourceSpan, name: String) -> Self {IntrinsicAST {loc, name}}
}
impl AST for IntrinsicAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn res_type(&self, _ctx: &CompCtx) -> Type {Type::Intrinsic(self.name.clone())}
    fn codegen<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {(Value::new(None, None, Type::Intrinsic(self.name.clone())), vec![])}
    fn to_code(&self) -> String {format!("@{}", self.name)}
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {writeln!(f, "intrinsic: {}", self.name)}
}
