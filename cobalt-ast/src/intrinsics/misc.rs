use super::*;
use crate::types;
use std::collections::VecDeque;
fn is_str(ty: TypeRef) -> bool {
    match ty.kind() {
        types::Pointer::KIND => ty
            .downcast::<types::Pointer>()
            .unwrap()
            .base()
            .is_and::<types::Int>(|i| i.bits() == 8),
        types::Reference::KIND => is_str(ty.downcast::<types::Reference>().unwrap()),
        types::SizedArray::KIND => ty
            .downcast::<types::SizedArray>()
            .unwrap()
            .elem()
            .is_and::<types::Int>(|i| i.bits() == 8),
        types::UnsizedArray::KIND => ty
            .downcast::<types::SizedArray>()
            .unwrap()
            .elem()
            .is_and::<types::Int>(|i| i.bits() == 8),
        _ => false,
    }
}
fn asm<'src, 'ctx>(
    loc: SourceSpan,
    _cparen: SourceSpan,
    args: Vec<Value<'src, 'ctx>>,
    ctx: &CompCtx<'src, 'ctx>,
) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
    let mut args = VecDeque::from(args);
    match args.len() {
        2 => {
            let a0 = args.pop_front().unwrap();
            let a1 = args.pop_front().unwrap();
            let loc0 = a0.loc;
            let loc1 = a1.loc;
            if is_str(a0.data_type) && is_str(a1.data_type) {
                match (a0.inter_val, a1.inter_val) {
                    (Some(InterData::Array(c)), Some(InterData::Array(b))) => {
                        let mut errs = vec![];
                        let c = String::from_utf8(
                            c.into_iter()
                                .map(|x| {
                                    if let InterData::Int(v) = x {
                                        v as _
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect(),
                        )
                        .unwrap_or_else(|e| {
                            errs.push(CobaltError::NonUtf8String {
                                pos: e.utf8_error().valid_up_to(),
                                loc: loc0,
                            });
                            String::new()
                        });
                        let b = String::from_utf8(
                            b.into_iter()
                                .map(|x| {
                                    if let InterData::Int(v) = x {
                                        v as _
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect(),
                        )
                        .unwrap_or_else(|e| {
                            errs.push(CobaltError::NonUtf8String {
                                pos: e.utf8_error().valid_up_to(),
                                loc: loc1,
                            });
                            String::new()
                        });
                        if !errs.is_empty() {
                            return Err(CobaltError::InvalidIntrinsicCall {
                                name: "asm",
                                loc,
                                errs,
                            });
                        }
                        Ok(Value::metaval(
                            InterData::InlineAsm(c, b),
                            types::InlineAsm::new(types::Null::new()),
                        ))
                    }
                    (iv0, iv1) => Err(CobaltError::InvalidInlineAsm2 {
                        loc1: loc0,
                        type1: a0.data_type.to_string(),
                        const1: iv0.is_some(),
                        loc2: loc1,
                        type2: a1.data_type.to_string(),
                        const2: iv1.is_some(),
                    }),
                }
            } else {
                Err(CobaltError::InvalidInlineAsm2 {
                    loc1: loc0,
                    type1: a0.data_type.to_string(),
                    const1: a0.inter_val.is_some(),
                    loc2: loc1,
                    type2: a1.data_type.to_string(),
                    const2: a1.inter_val.is_some(),
                })
            }
        }
        3 => {
            let a0 = args.pop_front().unwrap();
            let a1 = args.pop_front().unwrap();
            let a2 = args.pop_front().unwrap();
            let loc0 = a0.loc;
            let loc1 = a1.loc;
            let loc2 = a2.loc;
            let a0s = a0.data_type.to_string();
            let a0c = a0.inter_val.is_some();
            let ty = a0.into_type(ctx)?;
            if is_str(a1.data_type) && is_str(a2.data_type) {
                match (a1.inter_val, a2.inter_val) {
                    (Some(InterData::Array(c)), Some(InterData::Array(b))) => {
                        let mut errs = vec![];
                        let c = String::from_utf8(
                            c.into_iter()
                                .map(|x| {
                                    if let InterData::Int(v) = x {
                                        v as _
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect(),
                        )
                        .unwrap_or_else(|e| {
                            errs.push(CobaltError::NonUtf8String {
                                pos: e.utf8_error().valid_up_to(),
                                loc: loc0,
                            });
                            String::new()
                        });
                        let b = String::from_utf8(
                            b.into_iter()
                                .map(|x| {
                                    if let InterData::Int(v) = x {
                                        v as _
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect(),
                        )
                        .unwrap_or_else(|e| {
                            errs.push(CobaltError::NonUtf8String {
                                pos: e.utf8_error().valid_up_to(),
                                loc: loc1,
                            });
                            String::new()
                        });
                        if !errs.is_empty() {
                            return Err(CobaltError::InvalidIntrinsicCall {
                                name: "asm",
                                loc,
                                errs,
                            });
                        }
                        Ok(Value::metaval(
                            InterData::InlineAsm(c, b),
                            types::InlineAsm::new(ty),
                        ))
                    }
                    (iv0, iv1) => Err(CobaltError::InvalidInlineAsm3 {
                        loc1: loc0,
                        type1: a0s,
                        const1: a0c,
                        loc2: loc1,
                        type2: a1.data_type.to_string(),
                        const2: iv0.is_some(),
                        loc3: loc2,
                        type3: a2.data_type.to_string(),
                        const3: iv1.is_some(),
                    }),
                }
            } else {
                Err(CobaltError::InvalidInlineAsm3 {
                    loc1: loc0,
                    type1: a0s,
                    const1: a0c,
                    loc2: loc1,
                    type2: a1.data_type.to_string(),
                    const2: a1.inter_val.is_some(),
                    loc3: loc2,
                    type3: a2.data_type.to_string(),
                    const3: a2.inter_val.is_some(),
                })
            }
        }
        _ => Err(CobaltError::InvalidInlineAsm {
            nargs: args.len(),
            loc,
        }),
    }
}
fn alloca<'src, 'ctx>(
    loc: SourceSpan,
    _cparen: SourceSpan,
    args: Vec<Value<'src, 'ctx>>,
    ctx: &CompCtx<'src, 'ctx>,
) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
    let mut args = VecDeque::from(args);
    if args.is_empty() {
        return Err(CobaltError::AllocaNeedsArgs { loc });
    }
    let loc0 = args.front().unwrap().loc;
    let ty = args
        .front()
        .unwrap()
        .data_type
        .impl_convertible(types::TypeData::new(), ctx)
        .then(|| args.pop_front().unwrap().into_type(ctx).unwrap());
    if args.is_empty() {
        if let Some(ty) = ty {
            if let Some(llt) = ty.llvm_type(ctx) {
                Ok(Value::compiled(
                    ctx.builder.build_alloca(llt, "").unwrap().into(),
                    types::Pointer::new(types::Mut::new(ty)),
                ))
            } else {
                Err(CobaltError::NonRuntimeAllocaType {
                    ty: ty.to_string(),
                    loc: loc0,
                })
            }
        } else {
            unreachable!()
        }
    } else {
        let mut out = None::<inkwell::values::IntValue>;
        let mut errs = vec![];
        for arg in args {
            let arg = arg.decay(ctx);
            if arg.data_type.is::<types::Int>() {
                if let Some(out) = &mut out {
                    *out = ctx
                        .builder
                        .build_int_mul(*out, arg.value(ctx).unwrap().into_int_value(), "")
                        .unwrap();
                } else {
                    out = Some(arg.value(ctx).unwrap().into_int_value());
                }
            } else {
                errs.push(CobaltError::NonIntegralAllocaArg {
                    ty: arg.data_type.to_string(),
                    loc: arg.loc,
                });
            }
        }
        let ty = ty.unwrap_or_else(|| types::Int::unsigned(8));
        if let Some(llt) = ty.llvm_type(ctx) {
            if !errs.is_empty() {
                return Err(CobaltError::InvalidIntrinsicCall {
                    name: "alloca",
                    loc,
                    errs,
                });
            }
            Ok(Value::compiled(
                if let Some(size) = out {
                    ctx.builder.build_array_alloca(llt, size, "")
                } else {
                    ctx.builder.build_alloca(llt, "")
                }
                .unwrap()
                .into(),
                types::Pointer::new(types::Mut::new(ty)),
            ))
        } else {
            errs.push(CobaltError::NonRuntimeAllocaType {
                ty: ty.to_string(),
                loc: loc0,
            });
            Err(CobaltError::InvalidIntrinsicCall {
                name: "alloca",
                loc,
                errs,
            })
        }
    }
}
inventory::submit! {
    FunctionIntrinsic::new("asm", asm)
}
inventory::submit! {
    FunctionIntrinsic::new("alloca", alloca)
}
