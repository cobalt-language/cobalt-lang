use crate::*;
use glob::Pattern;
use inkwell::attributes::{Attribute, AttributeLoc::Function};
use inkwell::module::Linkage::*;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum::*};
use inkwell::values::{
    AsValueRef, BasicValue, BasicValueEnum::*, FunctionValue, InstructionOpcode,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Normal,
    Mutable,
    Constant,
}
pub type Parameter<'src> = (
    SourceSpan,
    Cow<'src, str>,
    ParamType,
    BoxedAST<'src>,
    Option<BoxedAST<'src>>,
); // parameter, mut/const, type, default
#[derive(Debug, Clone)]
pub struct FnDefAST<'src> {
    loc: SourceSpan,
    pub name: DottedName<'src>,
    pub ret: BoxedAST<'src>,
    pub params: Vec<Parameter<'src>>,
    pub body: BoxedAST<'src>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    pub in_struct: bool,
}
impl<'src> FnDefAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: DottedName<'src>,
        ret: BoxedAST<'src>,
        params: Vec<Parameter<'src>>,
        body: BoxedAST<'src>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
        in_struct: bool,
    ) -> Self {
        FnDefAST {
            loc,
            name,
            ret,
            params,
            body,
            annotations,
            in_struct,
        }
    }
}
impl<'src> AST<'src> for FnDefAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.ret.nodes()
            + self.body.nodes()
            + self
                .params
                .iter()
                .map(|(_, _, _, ty, def)| ty.nodes() + def.as_ref().map_or(0, |x| x.nodes()))
                .sum::<usize>()
            + 1
    }
    fn fwddef_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let oic = ctx.is_const.replace(true);
        let mut ret = self
            .ret
            .codegen(ctx, &mut vec![])
            .into_type(ctx)
            .unwrap_or(types::Error::new());
        while let Some(b) = ret.downcast::<types::Mut>() {
            ret = b.base()
        }
        let params = self
            .params
            .iter()
            .map(|(_, _, pt, ty, _)| {
                (
                    {
                        let mut val = ty
                            .codegen(ctx, &mut vec![])
                            .into_type(ctx)
                            .unwrap_or(types::Error::new());
                        while let Some(b) = val.downcast::<types::Mut>() {
                            val = b.base()
                        }
                        val
                    },
                    pt == &ParamType::Constant,
                )
            })
            .collect::<Vec<_>>();
        ctx.is_const.set(oic);
        let mut link_type = None;
        let mut linkas = None;
        let mut cconv = None;
        let mut inline = None;
        let mut vis_spec = None;
        let mut fn_type = None;
        let mut target_match = 2u8;
        let mut is_extern = false;
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
                }
                "linkas" => {
                    if let Some(arg) = arg {
                        linkas = Some((arg.clone(), loc))
                    }
                }
                "cconv" => {
                    cconv = cconv.or(match arg.as_deref() {
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => {
                            Some(12)
                        }
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        _ => None,
                    });
                }
                "extern" => {
                    is_extern = true;
                    cconv = cconv.or(match arg.as_deref() {
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => {
                            Some(12)
                        }
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        _ => None,
                    });
                }
                "inline" => {
                    if let Some(arg) = arg {
                        match &**arg {
                            "always" | "true" | "1" => inline = Some(true),
                            "never" | "false" | "0" => inline = Some(false),
                            _ => {}
                        }
                    } else {
                        inline = Some(true)
                    }
                }
                "c" | "C" => {
                    cconv = Some(0);
                    is_extern |= arg.as_deref() == Some("extern");
                    linkas = Some((
                        self.name
                            .ids
                            .last()
                            .expect("function name shouldn't be empty!")
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
                "method" if self.in_struct => {
                    if fn_type.is_none() && !params.is_empty() {
                        fn_type = Some(MethodType::Method);
                    }
                }
                "getter" if self.in_struct => {
                    if fn_type.is_none() && !params.is_empty() {
                        fn_type = Some(MethodType::Getter);
                    }
                }
                _ => {}
            }
        }
        let vs = vis_spec.unwrap_or(ctx.export.get());
        let cf = ctx.is_cfunc(&self.name);
        let cc = cconv.unwrap_or(if cf { 0 } else { 8 });
        let mt = fn_type.unwrap_or(MethodType::Static);
        if target_match == 0 {
            return;
        }
        if let Some(llt) = ret.llvm_type(ctx) {
            let mut good = true;
            let ps = params
                .iter()
                .filter_map(|(x, c)| {
                    if *c {
                        None
                    } else {
                        Some(BasicMetadataTypeEnum::from(
                            x.llvm_type(ctx).unwrap_or_else(|| {
                                good = false;
                                IntType(ctx.context.i8_type())
                            }),
                        ))
                    }
                })
                .collect::<Vec<_>>();
            if good && !ctx.is_const.get() {
                let ft = llt.fn_type(ps.as_slice(), false);
                let f = ctx.module.add_function(
                    &linkas.map_or_else(
                        || {
                            if cf {
                                self.name.ids.last().unwrap().0.clone()
                            } else {
                                ctx.mangle(&self.name).into()
                            }
                        },
                        |v| v.0,
                    ),
                    ft,
                    None,
                );
                match inline {
                    Some(true) => f.add_attribute(
                        Function,
                        ctx.context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("alwaysinline"),
                            0,
                        ),
                    ),
                    Some(false) => f.add_attribute(
                        Function,
                        ctx.context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("noinline"),
                            0,
                        ),
                    ),
                    _ => {}
                }
                f.set_call_conventions(cc);
                let gv = f.as_global_value();
                if let Some(link) = link_type {
                    gv.set_linkage(link)
                } else if ctx.flags.private_syms && !(vs || is_extern || cf) {
                    gv.set_linkage(Private)
                }
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                let defaults = self
                    .params
                    .iter()
                    .zip(cloned)
                    .filter_map(|((_, _, _, _, d), (t, _))| {
                        d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let val = a.codegen(ctx, &mut vec![]);
                            let val = val.impl_convert((t, None), ctx);
                            ctx.is_const.set(old_const);
                            val.ok()
                                .and_then(|v| v.inter_val)
                                .unwrap_or(InterData::Null)
                        })
                    })
                    .collect();
                let _ = ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                Some(InterData::Function(FnData {
                                    defaults,
                                    cconv: cc,
                                    mt,
                                })),
                                types::Function::new(ret, params).add_ref(false),
                            ),
                            VariableData {
                                fwd: true,
                                ..VariableData::with_vis(self.loc, vs)
                            },
                        ),
                    )
                });
            }
        } else if ret.size() == SizeType::Static(0) {
            let mut good = true;
            let ps = params
                .iter()
                .filter_map(|(x, c)| {
                    if *c {
                        None
                    } else {
                        Some(BasicMetadataTypeEnum::from(
                            x.llvm_type(ctx).unwrap_or_else(|| {
                                good = false;
                                IntType(ctx.context.i8_type())
                            }),
                        ))
                    }
                })
                .collect::<Vec<_>>();
            if good && !ctx.is_const.get() {
                let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                let f = ctx.module.add_function(
                    &linkas.map_or_else(
                        || {
                            if cf {
                                self.name.ids.last().unwrap().0.clone()
                            } else {
                                ctx.mangle(&self.name).into()
                            }
                        },
                        |v| v.0,
                    ),
                    ft,
                    None,
                );
                match inline {
                    Some(true) => f.add_attribute(
                        Function,
                        ctx.context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("alwaysinline"),
                            0,
                        ),
                    ),
                    Some(false) => f.add_attribute(
                        Function,
                        ctx.context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("noinline"),
                            0,
                        ),
                    ),
                    _ => {}
                }
                f.set_call_conventions(cc);
                let gv = f.as_global_value();
                if let Some(link) = link_type {
                    gv.set_linkage(link)
                } else if ctx.flags.private_syms && !(vs || is_extern || cf) {
                    gv.set_linkage(Private)
                }
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                let defaults = self
                    .params
                    .iter()
                    .zip(cloned)
                    .filter_map(|((_, _, _, _, d), (t, _))| {
                        d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let val = a.codegen(ctx, &mut vec![]);
                            let val = val.impl_convert((t, None), ctx);
                            ctx.is_const.set(old_const);
                            val.ok()
                                .and_then(|v| v.inter_val)
                                .unwrap_or(InterData::Null)
                        })
                    })
                    .collect();
                let _ = ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            Value::new(
                                Some(PointerValue(gv.as_pointer_value())),
                                Some(InterData::Function(FnData {
                                    defaults,
                                    cconv: cc,
                                    mt,
                                })),
                                types::Function::new(ret, params).add_ref(false),
                            ),
                            VariableData {
                                fwd: true,
                                ..VariableData::with_vis(self.loc, vs)
                            },
                        ),
                    )
                });
            }
        } else {
            let cloned = params.clone(); // Rust doesn't like me using params in the following closure
            let defaults = self
                .params
                .iter()
                .zip(cloned)
                .filter_map(|((_, _, _, _, d), (t, _))| {
                    d.as_ref().map(|a| {
                        let old_const = ctx.is_const.replace(true);
                        let val = a.codegen(ctx, &mut vec![]);
                        let val = val.impl_convert((t, None), ctx);
                        ctx.is_const.set(old_const);
                        val.ok()
                            .and_then(|v| v.inter_val)
                            .unwrap_or(InterData::Null)
                    })
                })
                .collect();
            let _ = ctx.with_vars(|v| {
                v.insert(
                    &self.name,
                    Symbol(
                        Value::new(
                            None,
                            Some(InterData::Function(FnData {
                                defaults,
                                cconv: cc,
                                mt,
                            })),
                            types::Function::new(ret, params).add_ref(false),
                        ),
                        VariableData {
                            fwd: true,
                            ..VariableData::with_vis(self.loc, vs)
                        },
                    ),
                )
            });
        }
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let oic = ctx.is_const.replace(true);
        let mut ret = self
            .ret
            .codegen(ctx, errs)
            .into_type(ctx)
            .unwrap_or_else(|e| {
                errs.push(e);
                types::Error::new()
            });
        if let Some(b) = ret.downcast::<types::Mut>() {
            errs.push(CobaltError::ReturnCantBeMut {
                loc: self.ret.loc(),
            });
            ret = b.base();
        }
        while let Some(b) = ret.downcast::<types::Mut>() {
            ret = b.base()
        }
        let mut params = self
            .params
            .iter()
            .map(|(_, _, pt, ty, _)| {
                (
                    {
                        let mut val = ty.codegen(ctx, errs).into_type(ctx).unwrap_or_else(|e| {
                            errs.push(e);
                            types::Error::new()
                        });
                        if let Some(b) = val.downcast::<types::Mut>() {
                            errs.push(CobaltError::ReturnCantBeMut { loc: ty.loc() });
                            val = b.base();
                        }
                        while let Some(b) = val.downcast::<types::Mut>() {
                            val = b.base();
                        }
                        val
                    },
                    pt == &ParamType::Constant,
                )
            })
            .collect::<Vec<_>>();
        ctx.is_const.set(oic);
        let mut link_type = None;
        let mut linkas = None;
        let mut is_extern = None;
        let mut cconv = None;
        let mut inline = None;
        let mut vis_spec = None;
        let mut fn_type = None;
        let mut target_match = 2u8;
        let mut dtor = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
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
                "cconv" => {
                    if let Some((_, prev)) = cconv {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "cconv",
                            loc,
                            prev,
                        });
                    }
                    cconv = cconv.or(match arg.as_deref() {
                        None => {
                            errs.push(CobaltError::InvalidAnnArgument {
                                name: "cconv",
                                found: None,
                                expected: Some("calling convention"),
                                loc,
                            });
                            None
                        }
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => {
                            Some(12)
                        }
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        Some(x) => match x.parse() {
                            Ok(v) => Some(v),
                            Err(_) => {
                                errs.push(CobaltError::InvalidAnnArgument {
                                    name: "cconv",
                                    found: arg.clone(),
                                    expected: Some("calling convention"),
                                    loc,
                                });
                                None
                            }
                        },
                    }
                    .map(|cc| (cc, loc)));
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
                    if let Some((_, prev)) = cconv {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "cconv",
                            loc,
                            prev,
                        });
                    }
                    cconv = cconv.or(match arg.as_deref() {
                        None => {
                            errs.pop();
                            None
                        }
                        Some("c") | Some("C") => Some(0),
                        Some("fast") | Some("Fast") => Some(8),
                        Some("cold") | Some("Cold") => Some(9),
                        Some("ghc") | Some("GHC") => Some(10),
                        Some("hipe") | Some("HiPE") => Some(11),
                        Some("webkit") | Some("webkit_js") | Some("WebKit") | Some("WebKit_JS") => {
                            Some(12)
                        }
                        Some("anyreg") | Some("AnyReg") => Some(13),
                        Some("preservemost") | Some("PreserveMost") => Some(14),
                        Some("preserveall") | Some("PreserveAll") => Some(15),
                        Some("swift") | Some("Swift") => Some(16),
                        Some("tail") | Some("Tail") => Some(18),
                        Some("swifttail") | Some("swift_tail") | Some("SwiftTail") => Some(20),
                        Some(x) => match x.parse() {
                            Ok(v) => Some(v),
                            Err(_) => {
                                errs.push(CobaltError::InvalidAnnArgument {
                                    name: "cconv",
                                    found: None,
                                    expected: Some("calling convention"),
                                    loc,
                                });
                                None
                            }
                        },
                    }
                    .map(|cc| (cc, loc)));
                }
                "inline" => {
                    if let Some((_, prev)) = inline {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "inline",
                            loc,
                            prev,
                        });
                    }
                    match arg.as_deref() {
                        Some("always" | "true" | "1") | None => inline = Some((true, loc)),
                        Some("never" | "false" | "0") => inline = Some((false, loc)),
                        _ => errs.push(CobaltError::InvalidAnnArgument {
                            name: "cconv",
                            found: arg.clone(),
                            expected: Some("always or never"),
                            loc,
                        }),
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
                "method" => {
                    if self.in_struct {
                        if let Some((_, prev)) = fn_type {
                            errs.push(CobaltError::RedefAnnArgument {
                                name: "method",
                                loc,
                                prev,
                            });
                        } else {
                            if arg.is_some() {
                                errs.push(CobaltError::InvalidAnnArgument {
                                    name: "method",
                                    found: arg.clone(),
                                    expected: None,
                                    loc,
                                });
                            }
                            let self_t =
                                ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap());
                            if params.is_empty() {
                                errs.push(CobaltError::InvalidSelfParam {
                                    loc: self.loc,
                                    self_t: self_t.to_string(),
                                    param: None,
                                });
                                params.push((types::Null::new(), false));
                            } else {
                                let p = params[0].0;
                                if !(self_t.impl_convertible(p, ctx)
                                    || self_t.add_ref(false).impl_convertible(p, ctx)
                                    || self_t.add_ref(true).impl_convertible(p, ctx))
                                {
                                    errs.push(CobaltError::InvalidSelfParam {
                                        loc: self.params[0].3.loc(),
                                        self_t: self_t.to_string(),
                                        param: Some(params[0].0.to_string()),
                                    });
                                } else {
                                    fn_type = Some((MethodType::Method, loc));
                                }
                            }
                        }
                    } else {
                        errs.push(CobaltError::UnknownAnnotation {
                            name: "method".into(),
                            def: "non-struct function",
                            loc,
                        })
                    }
                }
                "getter" => {
                    if self.in_struct {
                        if let Some((_, prev)) = fn_type {
                            errs.push(CobaltError::RedefAnnArgument {
                                name: "getter",
                                loc,
                                prev,
                            });
                        } else {
                            if arg.is_some() {
                                errs.push(CobaltError::InvalidAnnArgument {
                                    name: "getter",
                                    found: arg.clone(),
                                    expected: None,
                                    loc,
                                });
                            }
                            let self_t =
                                ctx.with_vars(|v| v.symbols["self_t"].0.as_type().unwrap());
                            if params.is_empty() {
                                errs.push(CobaltError::InvalidSelfParam {
                                    loc: self.loc,
                                    self_t: self_t.to_string(),
                                    param: None,
                                });
                                params.push((types::Null::new(), false));
                            } else {
                                let p = params[0].0;
                                if !(self_t.impl_convertible(p, ctx)
                                    || self_t.add_ref(false).impl_convertible(p, ctx)
                                    || self_t.add_ref(true).impl_convertible(p, ctx))
                                {
                                    errs.push(CobaltError::InvalidSelfParam {
                                        loc: self.params[0].3.loc(),
                                        self_t: self_t.to_string(),
                                        param: Some(params[0].0.to_string()),
                                    });
                                } else {
                                    fn_type = Some((MethodType::Getter, loc));
                                }
                            }
                        }
                    } else {
                        errs.push(CobaltError::UnknownAnnotation {
                            name: "getter".into(),
                            def: "non-struct function",
                            loc,
                        })
                    }
                }
                "op" => {
                    if self.in_struct {
                        match arg.as_deref() {
                            Some("drop") => {
                                if let Some(prev) = dtor {
                                    errs.push(CobaltError::RedefAnnArgument {
                                        name: "op(drop)",
                                        loc,
                                        prev,
                                    });
                                } else {
                                    if params.len() == 1 {
                                        let self_t = ctx.with_vars(|v| {
                                            v.symbols["self_t"].0.as_type().unwrap()
                                        });
                                        let p = params[0].0;
                                        if self_t.add_ref(false) == p || self_t.add_ref(true) == p {
                                            dtor = Some(loc);
                                            continue;
                                        }
                                    }
                                    errs.push(CobaltError::InvalidOpParams {
                                        loc,
                                        op: "drop",
                                        ex: "(&mut self_t)",
                                        found: params
                                            .iter()
                                            .map(|t| t.0.to_string().into())
                                            .collect(),
                                    });
                                }
                            }
                            _ => {
                                // TODO: add another error for invalid operators?
                                errs.push(CobaltError::InvalidAnnArgument {
                                    name: "op",
                                    found: arg.clone(),
                                    expected: Some("operator to overload"),
                                    loc,
                                });
                            }
                        }
                    } else {
                        errs.push(CobaltError::UnknownAnnotation {
                            name: "op".into(),
                            def: "non-struct function",
                            loc,
                        })
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "function",
                }),
            }
        }
        let vs = vis_spec.map_or(ctx.export.get(), |(v, _)| v);
        let cf = ctx.is_cfunc(&self.name);
        let cc = cconv.map_or(0, |(cc, _)| cc);
        let mt = fn_type.map_or(MethodType::Static, |v| v.0);
        if target_match == 0 {
            return Value::null();
        }
        let old_ip = ctx.builder.get_insert_block();
        ctx.var_scope.incr();
        let val = {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params
                    .iter()
                    .filter_map(|(x, c)| {
                        if *c {
                            None
                        } else {
                            Some(BasicMetadataTypeEnum::from(
                                x.llvm_type(ctx).unwrap_or_else(|| {
                                    good = false;
                                    IntType(ctx.context.i8_type())
                                }),
                            ))
                        }
                    })
                    .collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx
                        .lookup_full(&self.name)
                        .and_then(|x| -> Option<FunctionValue> {
                            Some(unsafe { std::mem::transmute(x.comp_val?.as_value_ref()) })
                        })
                        .unwrap_or_else(|| {
                            ctx.module.add_function(
                                &linkas.map_or_else(
                                    || {
                                        if cf {
                                            self.name.ids.last().unwrap().0.clone()
                                        } else {
                                            ctx.mangle(&self.name).into()
                                        }
                                    },
                                    |v| v.0,
                                ),
                                ft,
                                None,
                            )
                        });
                    match inline {
                        Some((true, _)) => f.add_attribute(
                            Function,
                            ctx.context.create_enum_attribute(
                                Attribute::get_named_enum_kind_id("alwaysinline"),
                                0,
                            ),
                        ),
                        Some((false, _)) => f.add_attribute(
                            Function,
                            ctx.context.create_enum_attribute(
                                Attribute::get_named_enum_kind_id("noinline"),
                                0,
                            ),
                        ),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    let gv = f.as_global_value();
                    if let Some((link, _)) = link_type {
                        gv.set_linkage(link)
                    } else if ctx.flags.private_syms && !(vs || is_extern.is_some() || cf) {
                        gv.set_linkage(Private)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let defaults = self
                        .params
                        .iter()
                        .zip(cloned)
                        .filter_map(|((_, _, _, p, d), (t, _))| {
                            d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx, errs);
                                let val = val.impl_convert((t, Some(p.loc())), ctx);
                                ctx.is_const.set(old_const);
                                match val {
                                    Ok(val) => {
                                        if let Some(val) = val.inter_val {
                                            val
                                        } else {
                                            errs.push(CobaltError::NotCompileTime { loc: a.loc() });
                                            InterData::Null
                                        }
                                    }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })
                        })
                        .collect();
                    let var = ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(
                                Value::new(
                                    Some(PointerValue(gv.as_pointer_value())),
                                    Some(InterData::Function(FnData {
                                        defaults,
                                        cconv: cc,
                                        mt,
                                    })),
                                    types::Function::new(ret, params.clone()).add_ref(false),
                                ),
                                VariableData::with_vis(self.loc, vs),
                            ),
                        )
                    });
                    if is_extern.is_none() {
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        ctx.lex_scope.incr();
                        {
                            let mut n = 0;
                            for ((loc, name, pt), &(ty, is_const)) in self
                                .params
                                .iter()
                                .map(|x| (x.0, &x.1, x.2))
                                .zip(params.iter())
                            {
                                if name.is_empty() {
                                    if !is_const {
                                        n += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(n).unwrap();
                                    param.set_name(name);
                                    let mut val = Value::compiled(param, ty);
                                    if pt == ParamType::Mutable {
                                        let a = ctx
                                            .builder
                                            .build_alloca(param.get_type(), name)
                                            .unwrap();
                                        ctx.builder.build_store(a, val.comp_val.unwrap()).unwrap();
                                        val.comp_val = Some(a.into());
                                        val.data_type = types::Mut::new(val.data_type);
                                    }
                                    val.name = Some((name.clone(), ctx.lex_scope.get()));
                                    let _ = ctx.with_vars(|v| {
                                        v.insert(
                                            &DottedName::local((name.clone(), unreachable_span())),
                                            Symbol(
                                                val,
                                                VariableData {
                                                    loc: Some(loc),
                                                    ..VariableData::default()
                                                },
                                            ),
                                        )
                                    });
                                    n += 1;
                                } else {
                                    let _ = ctx.with_vars(|v| {
                                        v.insert(
                                            &DottedName::local((name.clone(), unreachable_span())),
                                            Symbol(
                                                Value::new(None, None, ty),
                                                VariableData::default(),
                                            ),
                                        )
                                    });
                                }
                            }
                        }
                        ctx.to_drop.borrow_mut().push(Vec::new());
                        let body = self.body.codegen(ctx, errs);
                        ctx.to_drop
                            .borrow_mut()
                            .pop()
                            .unwrap()
                            .into_iter()
                            .for_each(|v| v.ins_dtor(ctx));
                        let graph = cfg::Cfg::new(
                            cfg::Location::Block(entry),
                            cfg::Location::current(ctx).unwrap(),
                            ctx,
                        );
                        graph.insert_dtors(ctx, true);
                        unsafe {
                            let seen = errs
                                .iter()
                                .filter_map(|err| {
                                    if let CobaltError::DoubleMove { loc, name, .. } = err {
                                        Some((*loc, &*(&**name as *const str)))
                                    } else {
                                        None
                                    }
                                })
                                .collect::<std::collections::HashSet<_>>();
                            errs.extend(graph.validate(ctx).into_iter().filter(|ce| match ce {
                                CobaltError::DoubleMove { name, loc, .. } => {
                                    !seen.contains(&(*loc, &**name))
                                }

                                CobaltError::LinearTypeNotUsed { .. } => true,

                                _ => false,
                            }));
                        }
                        std::mem::drop(graph);
                        if dtor.is_some() {
                            if let Some(ty) = params
                                .first()
                                .and_then(|ty| ty.0.downcast::<types::Reference>())
                                .and_then(|ty| {
                                    ty.base()
                                        .downcast::<types::Mut>()
                                        .map_or(ty as _, |b| b.base())
                                        .downcast::<types::Custom>()
                                })
                            {
                                if !ty.nom_info(ctx).no_auto_drop {
                                    Value::new(
                                        ty.llvm_type(ctx).and_then(|_| f.get_first_param()),
                                        None,
                                        ty,
                                    )
                                    .ins_dtor(ctx)
                                }
                            }
                        }
                        ctx.builder
                            .build_return(Some(
                                &body
                                    .impl_convert((ret, Some(self.ret.loc())), ctx)
                                    .map_err(|e| errs.push(e))
                                    .ok()
                                    .and_then(|v| v.value(ctx))
                                    .unwrap_or(llt.const_zero()),
                            ))
                            .unwrap();
                        hoist_allocas(&ctx.builder);
                        let mut b = ctx.moves.borrow_mut();
                        b.0.retain(|v| v.name.1 < ctx.lex_scope.get());
                        b.1.retain(|v| v.name.1 < ctx.lex_scope.get());
                        ctx.lex_scope.decr();
                        ctx.map_vars(|v| v.parent.unwrap());
                        ctx.restore_scope(old_scope);
                    }
                    var
                } else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let defaults = self
                        .params
                        .iter()
                        .zip(cloned)
                        .filter_map(|((_, _, _, p, d), (t, _))| {
                            d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx, errs);
                                let val = val.impl_convert((t, Some(p.loc())), ctx);
                                ctx.is_const.set(old_const);
                                match val {
                                    Ok(val) => {
                                        if let Some(val) = val.inter_val {
                                            val
                                        } else {
                                            errs.push(CobaltError::NotCompileTime { loc: a.loc() });
                                            InterData::Null
                                        }
                                    }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })
                        })
                        .collect();
                    ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(
                                Value::new(
                                    None,
                                    Some(InterData::Function(FnData {
                                        defaults,
                                        cconv: cc,
                                        mt,
                                    })),
                                    types::Function::new(ret, params).add_ref(false),
                                ),
                                VariableData::with_vis(self.loc, vs),
                            ),
                        )
                    })
                }
            } else if ret.size() == SizeType::Static(0) {
                let mut good = true;
                let ps = params
                    .iter()
                    .filter_map(|(x, c)| {
                        if *c {
                            None
                        } else {
                            Some(BasicMetadataTypeEnum::from(
                                x.llvm_type(ctx).unwrap_or_else(|| {
                                    good = false;
                                    IntType(ctx.context.i8_type())
                                }),
                            ))
                        }
                    })
                    .collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx
                        .lookup_full(&self.name)
                        .and_then(|x| -> Option<FunctionValue> {
                            Some(unsafe { std::mem::transmute(x.comp_val?.as_value_ref()) })
                        })
                        .unwrap_or_else(|| {
                            ctx.module.add_function(
                                &linkas.map_or_else(
                                    || {
                                        if cf {
                                            self.name.ids.last().unwrap().0.clone()
                                        } else {
                                            ctx.mangle(&self.name).into()
                                        }
                                    },
                                    |v| v.0,
                                ),
                                ft,
                                None,
                            )
                        });
                    match inline {
                        Some((true, _)) => f.add_attribute(
                            Function,
                            ctx.context.create_enum_attribute(
                                Attribute::get_named_enum_kind_id("alwaysinline"),
                                0,
                            ),
                        ),
                        Some((false, _)) => f.add_attribute(
                            Function,
                            ctx.context.create_enum_attribute(
                                Attribute::get_named_enum_kind_id("noinline"),
                                0,
                            ),
                        ),
                        _ => {}
                    }
                    f.set_call_conventions(cc);
                    let gv = f.as_global_value();
                    if let Some((link, _)) = link_type {
                        gv.set_linkage(link)
                    } else if ctx.flags.private_syms && !(vs || is_extern.is_some() || cf) {
                        gv.set_linkage(Private)
                    }
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let defaults = self
                        .params
                        .iter()
                        .zip(cloned)
                        .filter_map(|((_, _, _, p, d), (t, _))| {
                            d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx, errs);
                                let val = val.impl_convert((t, Some(p.loc())), ctx);
                                ctx.is_const.set(old_const);
                                match val {
                                    Ok(val) => {
                                        if let Some(val) = val.inter_val {
                                            val
                                        } else {
                                            errs.push(CobaltError::NotCompileTime { loc: a.loc() });
                                            InterData::Null
                                        }
                                    }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })
                        })
                        .collect();
                    let var = ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(
                                Value::new(
                                    Some(PointerValue(gv.as_pointer_value())),
                                    Some(InterData::Function(FnData {
                                        defaults,
                                        cconv: cc,
                                        mt,
                                    })),
                                    types::Function::new(ret, params.clone()).add_ref(false),
                                ),
                                VariableData::with_vis(self.loc, vs),
                            ),
                        )
                    });
                    if is_extern.is_none() {
                        let entry = ctx.context.append_basic_block(f, "entry");
                        ctx.builder.position_at_end(entry);
                        let old_scope = ctx.push_scope(&self.name);
                        ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                        ctx.lex_scope.incr();
                        {
                            let mut n = 0;
                            for ((loc, name, pt), &(ty, is_const)) in self
                                .params
                                .iter()
                                .map(|x| (x.0, &x.1, x.2))
                                .zip(params.iter())
                            {
                                if name.is_empty() {
                                    if !is_const {
                                        n += 1;
                                    }
                                    continue;
                                }
                                if !is_const {
                                    let param = f.get_nth_param(n).unwrap();
                                    param.set_name(name);
                                    let mut val = Value::compiled(param, ty);
                                    if pt == ParamType::Mutable {
                                        let a = ctx
                                            .builder
                                            .build_alloca(param.get_type(), name)
                                            .unwrap();
                                        ctx.builder.build_store(a, val.comp_val.unwrap()).unwrap();
                                        val.comp_val = Some(a.into());
                                        val.data_type = types::Mut::new(val.data_type);
                                    }
                                    val.name = Some((name.clone(), ctx.lex_scope.get()));
                                    let _ = ctx.with_vars(|v| {
                                        v.insert(
                                            &DottedName::local((name.clone(), unreachable_span())),
                                            Symbol(
                                                val,
                                                VariableData {
                                                    loc: Some(loc),
                                                    ..VariableData::default()
                                                },
                                            ),
                                        )
                                    });
                                    n += 1;
                                } else {
                                    let _ = ctx.with_vars(|v| {
                                        v.insert(
                                            &DottedName::local((name.clone(), unreachable_span())),
                                            Symbol(
                                                Value::new(None, None, ty),
                                                VariableData::default(),
                                            ),
                                        )
                                    });
                                }
                            }
                        }
                        ctx.to_drop.borrow_mut().push(Vec::new());
                        self.body.codegen(ctx, errs);
                        ctx.to_drop
                            .borrow_mut()
                            .pop()
                            .unwrap()
                            .into_iter()
                            .for_each(|v| v.ins_dtor(ctx));
                        let graph = cfg::Cfg::new(
                            cfg::Location::Block(entry),
                            cfg::Location::current(ctx).unwrap(),
                            ctx,
                        );
                        graph.insert_dtors(ctx, true);
                        unsafe {
                            let seen = errs
                                .iter()
                                .filter_map(|err| {
                                    if let CobaltError::DoubleMove { loc, name, .. } = err {
                                        Some((*loc, &*(&**name as *const str)))
                                    } else {
                                        None
                                    }
                                })
                                .collect::<std::collections::HashSet<_>>();
                            errs.extend(graph.validate(ctx).into_iter().filter(|ce| match ce {
                                CobaltError::DoubleMove { name, loc, .. } => {
                                    !seen.contains(&(*loc, &**name))
                                }

                                CobaltError::LinearTypeNotUsed { .. } => true,

                                _ => false,
                            }));
                        }
                        std::mem::drop(graph);
                        if dtor.is_some() {
                            if let Some(ty) = params
                                .first()
                                .and_then(|ty| ty.0.downcast::<types::Reference>())
                                .and_then(|ty| {
                                    ty.base()
                                        .downcast::<types::Mut>()
                                        .map_or(ty as _, |b| b.base())
                                        .downcast::<types::Custom>()
                                })
                            {
                                if !ty.nom_info(ctx).no_auto_drop {
                                    Value::new(
                                        ty.llvm_type(ctx).and_then(|_| f.get_first_param()),
                                        None,
                                        ty,
                                    )
                                    .ins_dtor(ctx)
                                }
                            }
                        }
                        ctx.builder.build_return(None).unwrap();
                        hoist_allocas(&ctx.builder);
                        let mut b = ctx.moves.borrow_mut();
                        b.0.retain(|v| v.name.1 < ctx.lex_scope.get());
                        b.1.retain(|v| v.name.1 < ctx.lex_scope.get());
                        ctx.lex_scope.decr();
                        ctx.map_vars(|v| v.parent.unwrap());
                        ctx.restore_scope(old_scope);
                    }
                    var
                } else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    let defaults = self
                        .params
                        .iter()
                        .zip(cloned)
                        .filter_map(|((_, _, _, p, d), (t, _))| {
                            d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let val = a.codegen(ctx, errs);
                                let val = val.impl_convert((t, Some(p.loc())), ctx);
                                ctx.is_const.set(old_const);
                                match val {
                                    Ok(val) => {
                                        if let Some(val) = val.inter_val {
                                            val
                                        } else {
                                            errs.push(CobaltError::NotCompileTime { loc: a.loc() });
                                            InterData::Null
                                        }
                                    }
                                    Err(e) => {
                                        errs.push(e);
                                        InterData::Null
                                    }
                                }
                            })
                        })
                        .collect();
                    ctx.with_vars(|v| {
                        v.insert(
                            &self.name,
                            Symbol(
                                Value::new(
                                    None,
                                    Some(InterData::Function(FnData {
                                        defaults,
                                        cconv: cc,
                                        mt,
                                    })),
                                    types::Function::new(ret, params).add_ref(false),
                                ),
                                VariableData::with_vis(self.loc, vs),
                            ),
                        )
                    })
                }
            } else {
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                let defaults = self
                    .params
                    .iter()
                    .zip(cloned)
                    .filter_map(|((_, _, _, p, d), (t, _))| {
                        d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let val = a.codegen(ctx, errs);
                            let val = val.impl_convert((t, Some(p.loc())), ctx);
                            ctx.is_const.set(old_const);
                            match val {
                                Ok(val) => {
                                    if let Some(val) = val.inter_val {
                                        val
                                    } else {
                                        errs.push(CobaltError::NotCompileTime { loc: a.loc() });
                                        InterData::Null
                                    }
                                }
                                Err(e) => {
                                    errs.push(e);
                                    InterData::Null
                                }
                            }
                        })
                    })
                    .collect();
                ctx.with_vars(|v| {
                    v.insert(
                        &self.name,
                        Symbol(
                            Value::new(
                                None,
                                Some(InterData::Function(FnData {
                                    defaults,
                                    cconv: cc,
                                    mt,
                                })),
                                types::Function::new(ret, params).add_ref(false),
                            ),
                            VariableData::with_vis(self.loc, vs),
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
        };
        ctx.var_scope.decr();
        if is_extern.is_none() {
            if let Some(bb) = old_ip {
                ctx.builder.position_at_end(bb);
            } else {
                ctx.builder.clear_insertion_position();
            }
        }
        if dtor.is_some() && !ctx.prepass.get() {
            let mut borrow = ctx.nom_stack.borrow_mut();
            let dval = &mut borrow.last_mut().unwrap().dtor;
            *dval = dval.or(val
                .0
                .comp_val
                .map(|v| unsafe { std::mem::transmute(v.as_value_ref()) }));
        }
        Value::null()
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "function: {}", self.name)?;
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
        writeln!(f, "{pre}├── parameters:")?;
        pre.push(false);
        for (n, (_, param, param_ty, ty, default)) in self.params.iter().enumerate() {
            writeln!(
                f,
                "{pre}{}{}{}",
                if n + 1 < self.params.len() {
                    "├── "
                } else {
                    "└── "
                },
                match param_ty {
                    ParamType::Normal => "",
                    ParamType::Mutable => "mut ",
                    ParamType::Constant => "const ",
                },
                param
            )?;
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
            } else {
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
pub struct CallAST<'src> {
    pub cparen: SourceSpan,
    pub target: BoxedAST<'src>,
    pub args: Vec<BoxedAST<'src>>,
}
impl<'src> CallAST<'src> {
    pub fn new(cparen: SourceSpan, target: BoxedAST<'src>, args: Vec<BoxedAST<'src>>) -> Self {
        CallAST {
            cparen,
            target,
            args,
        }
    }
}
impl<'src> AST<'src> for CallAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.target.loc(), self.cparen)
    }
    fn nodes(&self) -> usize {
        self.target.nodes() + self.args.iter().map(|x| x.nodes()).sum::<usize>() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        let val = self.target.codegen(ctx, errs);
        val.call(
            Some(self.cparen),
            self.args.iter().map(|a| a.codegen(ctx, errs)).collect(),
            ctx,
        )
        .unwrap_or_else(|err| {
            errs.push(err);
            Value::error().with_loc(self.target.loc())
        })
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
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
pub struct IntrinsicAST<'src> {
    loc: SourceSpan,
    pub name: Cow<'src, str>,
}
impl<'src> IntrinsicAST<'src> {
    pub fn new(loc: SourceSpan, name: Cow<'src, str>) -> Self {
        IntrinsicAST { loc, name }
    }
}
impl<'src> AST<'src> for IntrinsicAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn codegen_impl<'ctx>(
        &self,
        _ctx: &CompCtx<'src, 'ctx>,
        errs: &mut Vec<CobaltError<'src>>,
    ) -> Value<'src, 'ctx> {
        if intrinsics::FUNCTION_INTRINSICS
            .pin()
            .contains_key(&*self.name)
            || intrinsics::VALUE_INTRINSICS.pin().contains_key(&*self.name)
        {
            Value::new(None, None, types::Intrinsic::new_ref(&self.name))
        } else {
            errs.push(CobaltError::UnknownIntrinsic {
                name: self.name.clone(),
                loc: self.loc,
            });
            Value::error()
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "intrinsic: {}", self.name)
    }
}
/// Move all constant alloca instructions to the entry block of the function
/// The return doesn't matter, but it makes it
pub fn hoist_allocas(b: &inkwell::builder::Builder) -> Option<()> {
    let blocks = b.get_insert_block()?.get_parent()?.get_basic_blocks();
    let (&first, blocks) = blocks.split_first()?;
    b.position_before(&first.get_terminator()?);
    for block in blocks {
        let mut i = block.get_first_instruction();
        while let Some(inst) = i {
            i = inst.get_next_instruction();
            // instruction is an alloca && all operands are constant
            if inst.get_opcode() == InstructionOpcode::Alloca
                && (0..inst.get_num_operands()).all(|n| {
                    inst.get_operand(n).map_or(true, |i| {
                        i.left()
                            .map_or(true, |v| v.as_instruction_value().is_none())
                    })
                })
            {
                let name = inst.get_name();
                inst.remove_from_basic_block();
                b.insert_instruction(&inst, name.and_then(|s| s.to_str().ok()));
            }
        }
    }
    Some(())
}
