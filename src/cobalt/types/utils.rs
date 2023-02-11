use crate::*;
use std::cmp::{min, max};
use inkwell::values::{BasicValueEnum::*, BasicMetadataValueEnum, CallableValue};
use inkwell::types::{BasicType, BasicMetadataTypeEnum};
use inkwell::{
    IntPredicate::{SLT, ULT, SGT, UGT, SLE, ULE, SGE, UGE, EQ, NE},
    FloatPredicate::{OLT, OGT, OLE, OGE, OEQ, ONE}
};
pub fn bin_type(lhs: Type, rhs: Type, op: &str) -> Type {
    match (lhs, rhs) {
        (l, Type::Reference(x, _) | Type::Borrow(x)) => bin_type(l, *x, op),
        (Type::Int(ls, lu), Type::Int(rs, ru)) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "^^" => Type::Int(max(ls, rs), lu && ru),
            _ => Type::Null
        },
        (x @ Type::Int(..), Type::IntLiteral) | (Type::IntLiteral, x @ Type::Int(..)) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "^^" => x,
            _ => Type::Null
        },
        (Type::IntLiteral, Type::IntLiteral) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "^^" => Type::IntLiteral,
            _ => Type::Null
        },
        (Type::Int(..) | Type::IntLiteral, x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) | (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), Type::Int(..) | Type::IntLiteral) => match op {
            "+" | "-" | "*" | "/" | "%" => x,
            _ => Type::Null
        },
        (Type::Float16, Type::Float16) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float16,
            _ => Type::Null
        },
        (Type::Float32, Type::Float16 | Type::Float32) | (Type::Float16, Type::Float32) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float32,
            _ => Type::Null
        },
        (Type::Float64, Type::Float16 | Type::Float32 | Type::Float64) | (Type::Float16 | Type::Float32, Type::Float64) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float64,
            _ => Type::Null
        },
        (Type::Float128, Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) | (Type::Float16 | Type::Float32 | Type::Float64, Type::Float128) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float128,
            _ => Type::Null
        },
        (x @ Type::Pointer(..), Type::IntLiteral | Type::Int(..)) | (Type::IntLiteral | Type::Int(..), x @ Type::Pointer(..)) => match op {
            "+" | "-" => x,
            _ => Type::Null
        }
        (Type::Reference(x, true), r) => match (*x, r) {
            (Type::IntLiteral, _) => panic!("There shouldn't be a reference to an integer literal"),
            (x @ Type::Int(..), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "^^=" => Type::Reference(Box::new(x), true),
                _ => bin_type(x, r, op)
            },
            (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) => match op {
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^^=" => Type::Reference(Box::new(x), true),
                _ => bin_type(x, r, op)
            },
            (x @ Type::Pointer(..), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "+=" | "-=" => Type::Reference(Box::new(x), true),
                _ => bin_type(x, r, op)
            },
            (x @ Type::Pointer(..), y @ Type::Pointer(..)) if x == y => match op {
                "=" => Type::Reference(Box::new(x), true),
                _ => bin_type(x, y, op)
            },
            (x, r) => bin_type(x, r, op)
        },
        (Type::Reference(x, false) | Type::Borrow(x), r) => bin_type(*x, r, op),
        _ => Type::Null
    }
}
pub fn pre_type(val: Type, op: &str) -> Type {
    match val {
        Type::Reference(x, true) => match *x {
            Type::IntLiteral => panic!("There shouldn't be a reference to an integer literal"),
            x @ (Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 | Type::Pointer(..)) => match op {
                "++" | "--" => Type::Reference(Box::new(x), true),
                _ => Type::Null
            }
            x => pre_type(x, op)
        }
        Type::Reference(x, false) | Type::Borrow(x) => pre_type(*x, op),
        x @ (Type::IntLiteral | Type::Int(..)) => match op {
            "+" | "-" | "~" => x,
            _ => Type::Null
        },
        x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
            "+" | "-" => x,
            _ => Type::Null
        }
        Type::Pointer(b, c) => match op {
            "+" | "-" => Type::Pointer(b, c),
            "*" => Type::Reference(b, c),
            _ => Type::Null
        }
        _ => Type::Null
    }
}
pub fn post_type(val: Type, op: &str) -> Type {
    match val {
        Type::Reference(x, _) | Type::Borrow(x) => post_type(*x, op),
        _ => Type::Null
    }
}
pub fn bin_op<'ctx>(mut lhs: Variable<'ctx>, mut rhs: Variable<'ctx>, op: &str, ctx: &CompCtx<'ctx>) -> Option<Variable<'ctx>> {
    match (lhs.data_type, rhs.data_type) {
        (Type::Borrow(l), r) => {
            lhs.data_type = *l;
            rhs.data_type = r;
            bin_op(lhs, rhs, op, ctx)
        },
        (l, Type::Borrow(r)) => {
            lhs.data_type = l;
            rhs.data_type = *r;
            bin_op(lhs, rhs, op, ctx)
        },
        (Type::Reference(l, false), r) => {
            lhs.data_type = *l;
            rhs.data_type = r;
            if !ctx.is_const.get() && lhs.data_type.register() {
                if let Some(v) = lhs.comp_val {
                    lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            bin_op(lhs, rhs, op, ctx)
        },
        (l, Type::Reference(r, _)) => {
            lhs.data_type = l;
            rhs.data_type = *r;
            if !ctx.is_const.get() && rhs.data_type.register() {
                if let Some(v) = rhs.comp_val {
                    rhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            bin_op(lhs, rhs, op, ctx)
        },
        (Type::Reference(l, true), r) => match (*l, r) {
            (Type::IntLiteral, _) => panic!("There shouldn't be a reference to an integer literal"),
            (l @ Type::Int(..), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {ctx.builder.build_store(l, r);}
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "+=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "-=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "*=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_int_mul(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "/=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = if unsigned {ctx.builder.build_int_unsigned_div(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_div(v1, r.into_int_value(), "")};
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "%=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = if unsigned {ctx.builder.build_int_unsigned_rem(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_rem(v1, r.into_int_value(), "")};
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "&=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_and(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "|=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_or(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "^=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_xor(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "<<=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_left_shift(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                ">>=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            let v1 = ctx.builder.build_load(l, "").into_int_value();
                            let v2 = ctx.builder.build_right_shift(v1, r.into_int_value(), false, "");
                            ctx.builder.build_store(l, v2);
                        },
                        _ => {}
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                "^^=" => {
                    return None;
                    // TODO: implement exponents
                },
                _ => {
                    lhs.data_type = l;
                    rhs.data_type = r;
                    if !ctx.is_const.get() && lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(lhs, rhs, op, ctx)
                }
            },
            (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" => {
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(rv)), false) => {
                            let v1 = match r {
                                Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                                _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                            };
                            ctx.builder.build_store(l, v1);
                        },
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x), true);
                    Some(lhs)
                },
                "+=" => {
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(rv)), false) => {
                            let v1 = match r {
                                Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                                _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                            };
                            let v2 = ctx.builder.build_load(l, "").into_float_value();
                            let v3 = ctx.builder.build_float_add(v1, v2, "");
                            ctx.builder.build_store(l, v3);
                        },
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x), true);
                    Some(lhs)
                },
                "-=" => {
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(rv)), false) => {
                            let v1 = match r {
                                Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                                _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                            };
                            let v2 = ctx.builder.build_load(l, "").into_float_value();
                            let v3 = ctx.builder.build_float_sub(v1, v2, "");
                            ctx.builder.build_store(l, v3);
                        },
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x), true);
                    Some(lhs)
                },
                "*=" => {
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(rv)), false) => {
                            let v1 = match r {
                                Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                                _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                            };
                            let v2 = ctx.builder.build_load(l, "").into_float_value();
                            let v3 = ctx.builder.build_float_mul(v1, v2, "");
                            ctx.builder.build_store(l, v3);
                        },
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x), true);
                    Some(lhs)
                },
                "/=" => {
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(rv)), false) => {
                            let v1 = match r {
                                Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                                _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                            };
                            let v2 = ctx.builder.build_load(l, "").into_float_value();
                            let v3 = ctx.builder.build_float_div(v1, v2, "");
                            ctx.builder.build_store(l, v3);
                        },
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x), true);
                    Some(lhs)
                },
                "%=" => None, // TODO: implement fmod
                "^^=" => None, // TODO: implement powf
                _ => None
            },
            (x @ Type::Pointer(..), y) if x == y => match op {
                "=" => {
                    lhs.data_type = x;
                    rhs.data_type = y;
                    match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(r), false) => {
                            ctx.builder.build_store(l, r);
                        }
                        _ => {},
                    }
                    lhs.inter_val = None;
                    Some(lhs)
                },
                _ => {
                    lhs.data_type = x;
                    rhs.data_type = y;
                    if lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(lhs, rhs, op, ctx)
                }
            }
            (Type::Pointer(b, m), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "+=" => {
                    match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(x), false) => {
                            let pt = ctx.context.i64_type();
                            let v1 = ctx.builder.build_load(l, "").into_pointer_value();
                            let v2 = ctx.builder.build_int_mul(r, pt.const_int(x, false), "");
                            let v3 = ctx.builder.build_ptr_to_int(v1, pt, "");
                            let v4 = ctx.builder.build_int_add(v3, v2, "");
                            let v5 = ctx.builder.build_int_to_ptr(v4, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), "");
                            ctx.builder.build_store(l, v5);
                        }
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Pointer(b, m);
                    Some(lhs)
                },
                "-=" => {
                    match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                        (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(x), false) => {
                            let pt = ctx.context.i64_type();
                            let v1 = ctx.builder.build_load(l, "").into_pointer_value();
                            let v2 = ctx.builder.build_int_mul(r, pt.const_int(x, false), "");
                            let v3 = ctx.builder.build_ptr_to_int(v1, pt, "");
                            let v4 = ctx.builder.build_int_sub(v3, v2, "");
                            let v5 = ctx.builder.build_int_to_ptr(v4, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), "");
                            ctx.builder.build_store(l, v5);
                        }
                        _ => {},
                    }
                    lhs.inter_val = None;
                    lhs.data_type = Type::Pointer(b, m);
                    Some(lhs)
                },
                _ => {
                    lhs.data_type = Type::Pointer(b, m);
                    rhs.data_type = r;
                    if !ctx.is_const.get() && lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(lhs, rhs, op, ctx)
                }
            },
            (l, r) => {
                lhs.data_type = l;
                rhs.data_type = r;
                if !ctx.is_const.get() && lhs.data_type.register() {
                    if let Some(v) = lhs.comp_val {
                        lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                    }
                }
                bin_op(lhs, rhs, op, ctx)
            }
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls > rs => {
            if let (Some(IntValue(val)), false) = (rhs.comp_val, ctx.is_const.get()) {
                rhs.comp_val = Some(IntValue(if ru {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}));
            }
            lhs.data_type = Type::Int(ls, lu);
            rhs.data_type = Type::Int(ls, ru);
            bin_op(lhs, rhs, op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls < rs => {
            if let (Some(IntValue(val)), false) = (lhs.comp_val, ctx.is_const.get()) {
                lhs.comp_val = Some(IntValue(if ru {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}));
            }
            lhs.data_type = Type::Int(rs, lu);
            rhs.data_type = Type::Int(rs, ru);
            bin_op(lhs, rhs, op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls == rs => match op {
            "+" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_add(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l + r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu && ru),
                export: true
            }),
            "-" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_sub(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l - r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu && ru),
                export: true
            }),
            "*" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_mul(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l * r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu && ru),
                export: true
            }),
            "/" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(if ru {ctx.builder.build_int_unsigned_div(l, r, "")} else {ctx.builder.build_int_signed_div(l, r, "")})),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l / r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), ru),
                export: true
            }),
            "%" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(if ru {ctx.builder.build_int_unsigned_rem(l, r, "")} else {ctx.builder.build_int_signed_rem(l, r, "")})),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l % r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), ru),
                export: true
            }),
            "&" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_and(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l & r)),
                    _ => None
                },
                data_type: Type::Int(min(ls, rs), lu || ru),
                export: true
            }),
            "|" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_or(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l | r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu || ru),
                export: true
            }),
            "^" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_xor(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l ^ r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu || ru),
                export: true
            }),
            ">>" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_left_shift(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l << r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu || ru),
                export: true
            }),
            "<<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_right_shift(l, r, false, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l >> r)),
                    _ => None
                },
                data_type: Type::Int(max(ls, rs), lu || ru),
                export: true
            }),
            "<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {ULT} else {SLT}, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l < r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {UGT} else {SGT}, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l > r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "<=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {ULE} else {SLE}, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l <= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {UGE} else {SGE}, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l >= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "==" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(EQ, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l == r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "!=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(NE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l < r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "^^" => None, // TODO: implement exponents
            _ => None
        },
        (x @ Type::Int(..), Type::IntLiteral) => bin_op(Variable {data_type: x.clone(), ..lhs}, impl_convert(Variable {data_type: Type::IntLiteral, ..rhs}, x, ctx)?, op, ctx),
        (Type::IntLiteral, x @ Type::Int(..)) => {
            let t = x.clone();
            lhs.data_type = Type::IntLiteral;
            bin_op(impl_convert(lhs, x, ctx)?, Variable {data_type: t, ..rhs}, op, ctx)
        },
        (Type::IntLiteral, Type::IntLiteral) => match op {
            "+" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_add(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l + r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "-" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_sub(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l - r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "*" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_mul(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l * r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "/" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_signed_div(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l / r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "%" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_signed_rem(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l % r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "&" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_and(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l & r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "|" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_or(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l | r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "^" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_xor(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l ^ r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            ">>" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_left_shift(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l << r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "<<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_right_shift(l, r, false, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l >> r)),
                    _ => None
                },
                data_type: Type::IntLiteral,
                export: true
            }),
            "^^" => None, // TODO: implement exponents
            "<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SLT, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l < r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SGT, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l > r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "<=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SLE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l <= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SGE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l >= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "==" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(EQ, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l == r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "!=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(NE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(if l < r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            _ => None
        },
        (Type::Pointer(b, s), Type::Int(..) | Type::IntLiteral) => match op {
            "+" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(x), false) => Some({
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_int_mul(r, pt.const_int(x, false), "");
                        let v3 = ctx.builder.build_int_add(v1, v2, "");
                        PointerValue(ctx.builder.build_int_to_ptr(v3, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), ""))
                    }),
                    _ => None
                },
                inter_val: None,
                data_type: Type::Pointer(b, s),
                export: true
            }),
            "-" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(x), false) => Some({
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_int_mul(r, pt.const_int(x, false), "");
                        let v3 = ctx.builder.build_int_sub(v1, v2, "");
                        PointerValue(ctx.builder.build_int_to_ptr(v3, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), ""))
                    }),
                    _ => None
                },
                inter_val: None,
                data_type: Type::Pointer(b, s),
                export: true
            }),
            _ => None
        },
        (Type::Int(..) | Type::IntLiteral, Type::Pointer(b, s)) => match op {
            "+" => Some(Variable {
                comp_val: match (rhs.comp_val, lhs.comp_val, b.size(), ctx.is_const.get()) { // I just swapped the sides here
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(x), false) => Some({
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_int_mul(r, pt.const_int(x, false), "");
                        let v3 = ctx.builder.build_int_add(v1, v2, "");
                        PointerValue(ctx.builder.build_int_to_ptr(v3, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), ""))
                    }),
                    _ => None
                },
                inter_val: None,
                data_type: Type::Pointer(b, s),
                export: true
            }),
            _ => None
        },
        (l @ Type::Pointer(..), r @ Type::Pointer(..)) => match op {
            "-" if l == r => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_sub(v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(64, false),
                export: true
            }),
            "<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(ULT, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            ">" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(UGT, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            "<=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(ULE, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            ">=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(UGE, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            "==" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(EQ, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            "!=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.i64_type();
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(NE, v1, v2, "")))
                    },
                    _ => None
                },
                inter_val: None,
                data_type: Type::Int(1, false),
                export: true
            }),
            _ => None
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) if l.size() < r.size() => {
            lhs.comp_val = match (lhs.comp_val, ctx.is_const.get()) {
                (Some(FloatValue(l)), false) => Some(FloatValue(ctx.builder.build_float_cast(l, r.llvm_type(ctx).unwrap().into_float_type(), ""))),
                _ => None
            };
            lhs.data_type = r.clone();
            rhs.data_type = r;
            bin_op(lhs, rhs, op, ctx)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::Float16 | Type::Float32 | Type::Float64)) if l.size() > r.size() => {
            rhs.comp_val = match (rhs.comp_val, ctx.is_const.get()) {
                (Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_cast(r, l.llvm_type(ctx).unwrap().into_float_type(), ""))),
                _ => None
            };
            lhs.data_type = l.clone();
            rhs.data_type = l;
            bin_op(lhs, rhs, op, ctx)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) if l == r => match op {
            "+" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_add(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l + r)),
                    _ => None
                },
                data_type: l,
                export: true
            }),
            "-" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_sub(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l - r)),
                    _ => None
                },
                data_type: l,
                export: true
            }),
            "*" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_mul(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l * r)),
                    _ => None
                },
                data_type: l,
                export: true
            }),
            "/" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_div(l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l / r)),
                    _ => None
                },
                data_type: l,
                export: true
            }),
            "%" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(_l)), Some(FloatValue(_r)), false) => None, // TODO: implement fmod
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l.rem_euclid(r))),
                    _ => None
                },
                data_type: l,
                export: true
            }),
            "^^" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(_l)), Some(FloatValue(_r)), false) => None, // TODO: implement powf
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l.powf(r))),
                    _ => None
                },
                data_type: bin_type(l, r, op),
                export: true
            }),
            "<" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OLT, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l < r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OGT, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l > r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "<=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OLE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l <= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            ">=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OGE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l >= r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "==" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OEQ, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l == r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            "!=" => Some(Variable {
                comp_val: match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(ONE, l, r, ""))),
                    _ => None
                },
                inter_val: match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(if l != r {1} else {0})),
                    _ => None
                },
                data_type: Type::Int(1, false),
                export: true
            }),
            _ => None
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..))) => {
            if let (Some(IntValue(rv)), false) = (rhs.comp_val, ctx.is_const.get()) {
                rhs.comp_val = Some(FloatValue(match r {
                    Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, l.llvm_type(ctx).unwrap().into_float_type(), ""),
                    _ => ctx.builder.build_unsigned_int_to_float(rv, l.llvm_type(ctx).unwrap().into_float_type(), "")
                }));
            }
            lhs.data_type = l.clone();
            rhs.data_type = l;
            bin_op(lhs, rhs, op, ctx)
        },
        (l @ (Type::IntLiteral | Type::Int(..)), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) => {
            if let (Some(IntValue(lv)), false) = (lhs.comp_val, ctx.is_const.get()) {
                lhs.comp_val = Some(FloatValue(match l {
                    Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(lv, r.llvm_type(ctx).unwrap().into_float_type(), ""),
                    _ => ctx.builder.build_unsigned_int_to_float(lv, r.llvm_type(ctx).unwrap().into_float_type(), "")
                }));
            }
            lhs.data_type = r.clone();
            rhs.data_type = r;
            bin_op(lhs, rhs, op, ctx)
        },
        _ => None
    }
}
pub fn pre_op<'ctx>(mut val: Variable<'ctx>, op: &str, ctx: &CompCtx<'ctx>) -> Option<Variable<'ctx>> {
    match val.data_type {
        Type::Borrow(x) => {
            val.data_type = *x;
            pre_op(val, op, ctx)
        },
        Type::Reference(x, false) => if op == "&" {
            val.data_type = Type::Pointer(x, false);
            Some(val)
        }
        else if op == "&&" {
            val.data_type = Type::Pointer(x, false);
            pre_op(val, "&", ctx)
        }
        else {
            val.data_type = *x;
            if !ctx.is_const.get() && val.data_type.register() {
                if let Some(v) = val.comp_val {
                    val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            pre_op(val, op, ctx)
        },
        Type::Reference(x, true) => if op == "&" {
            val.data_type = Type::Pointer(x, true);
            Some(val)
        }
        else if op == "&&" {
            val.data_type = Type::Pointer(x, true);
            pre_op(val, "&", ctx)
        }
        else {
            match *x {
                Type::IntLiteral => panic!("There shouldn't be a reference to an integer literal"),
                x @ Type::Int(..) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, x.llvm_type(ctx).unwrap().into_int_type().const_int(1, false), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Some(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, x.llvm_type(ctx).unwrap().into_int_type().const_int(1, false), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Some(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(val, op, ctx)
                    }
                },
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_float_value();
                            let v2 = ctx.builder.build_float_add(v1, x.llvm_type(ctx).unwrap().into_float_type().const_float(1.0), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Some(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_float_value();
                            let v2 = ctx.builder.build_float_sub(v1, x.llvm_type(ctx).unwrap().into_float_type().const_float(1.0), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Some(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(val, op, ctx)
                    }
                },
                Type::Pointer(b, m) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), SizeType::Static(x), false) = (val.comp_val, b.size(), ctx.is_const.get()) {
                            let pt = ctx.context.i64_type();
                            let v1 = ctx.builder.build_load(v, "").into_pointer_value();
                            let v2 = ctx.builder.build_ptr_to_int(v1, pt, "");
                            let v3 = ctx.builder.build_int_add(v2, pt.const_int(x, false), "");
                            let v4 = ctx.builder.build_int_to_ptr(v3, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), "");
                            ctx.builder.build_store(v, v4);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Some(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), SizeType::Static(x), false) = (val.comp_val, b.size(), ctx.is_const.get()) {
                            let pt = ctx.context.i64_type();
                            let v1 = ctx.builder.build_load(v, "").into_pointer_value();
                            let v2 = ctx.builder.build_ptr_to_int(v1, pt, "");
                            let v3 = ctx.builder.build_int_sub(v2, pt.const_int(x, false), "");
                            let v4 = ctx.builder.build_int_to_ptr(v3, b.llvm_type(ctx).unwrap().ptr_type(inkwell::AddressSpace::from(0u16)), "");
                            ctx.builder.build_store(v, v4);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Some(val)
                    },
                    _ => {
                        val.data_type = Type::Pointer(b, m);
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(val, op, ctx)
                    }
                },
                x => {
                    val.data_type = x;
                    if !ctx.is_const.get() && val.data_type.register() {
                        if let Some(v) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    pre_op(val, op, ctx)
                }
            }
        },
        Type::IntLiteral => match op {
            "+" => {
                val.data_type = Type::IntLiteral;
                Some(val)
            },
            "-" => Some(Variable {
                comp_val: if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_int_neg(v, "")))} else {None},
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(-v))} else {None},
                data_type: Type::IntLiteral,
                export: true
            }),
            "~" => Some(Variable {
                comp_val: if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_xor(v, ctx.context.i64_type().const_all_ones(), "")))} else {None},
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(!v))} else {None},
                data_type: Type::IntLiteral,
                export: true
            }),
            _ => None
        },
        Type::Int(s, u) => match op {
            "+" => {
                val.data_type = Type::Int(s, u);
                Some(val)
            },
            "-" => Some(Variable {
                comp_val: if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_int_neg(v, "")))} else {None},
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(-v))} else {None},
                data_type: Type::Int(s, u),
                export: true
            }),
            "~" => Some(Variable {
                comp_val: if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_xor(v, ctx.context.custom_width_int_type(s as u32).const_all_ones(), "")))} else {None},
                inter_val: if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(!v))} else {None},
                data_type: Type::Int(s, u),
                export: true
            }),
            _ => None
        },
        x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
            "+" => {
                val.data_type = x;
                Some(val)
            },
            "-" => Some(Variable {
                comp_val: if let (Some(FloatValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(FloatValue(ctx.builder.build_float_neg(v, "")))} else {None},
                inter_val: if let Some(InterData::Float(v)) = val.inter_val {Some(InterData::Float(-v))} else {None},
                data_type: x,
                export: true
            }),
            _ => None
        }
        Type::Pointer(b, m) => match op {
            "*" => {
                val.data_type = Type::Reference(b, m);
                Some(val)
            },
            _ => None
        },
        _ => None
    }
}
pub fn post_op<'ctx>(val: Variable<'ctx>, _op: &str, _ctx: &CompCtx<'ctx>) -> Option<Variable<'ctx>> {
    match val.data_type { // The only posfix operators are ? and !, and they're for error handling
        _ => None
    }
}
pub fn impl_convert<'ctx>(mut val: Variable<'ctx>, target: Type, ctx: &CompCtx<'ctx>) -> Option<Variable<'ctx>> {
    if val.data_type == target {Some(val)}
    else {
        match val.data_type {
            Type::Borrow(b) => {
                val.data_type = *b;
                impl_convert(val, target, ctx)
            },
            Type::Reference(b, true) => {
                if &target == &Type::Reference(b.clone(), false) {Some(Variable {data_type: Type::Reference(b, false), ..val})}
                else {
                    if !ctx.is_const.get() && b.register() {
                        if let Some(PointerValue(v)) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(v, ""));
                        }
                    }
                    val.data_type = *b;
                    impl_convert(val, target, ctx)
                }
            },
            Type::Reference(b, false) => {
                if !ctx.is_const.get() && b.register() {
                    if let Some(PointerValue(v)) = val.comp_val {
                        val.comp_val = Some(ctx.builder.build_load(v, ""));
                    }
                }
                val.data_type = *b;
                impl_convert(val, target, ctx)
            },
            Type::IntLiteral => match target {
                x @ Type::Int(..) => Some(Variable {
                    comp_val: if let Some(InterData::Int(v)) = val.inter_val {Some(IntValue(x.llvm_type(ctx).unwrap().into_int_type().const_int(v as u64, true)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(IntValue(ctx.builder.build_int_z_extend(v, x.llvm_type(ctx).unwrap().into_int_type(), "")))}
                              else {None},
                    data_type: x,
                    ..val
                }),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Some(Variable {
                    comp_val: if let Some(InterData::Int(v)) = val.inter_val {Some(FloatValue(x.llvm_type(ctx).unwrap().into_float_type().const_float(v as f64)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(FloatValue(ctx.builder.build_signed_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "")))}
                              else {None},
                    inter_val: if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    data_type: x,
                    export: true
                }),
                _ => None
            },
            _ => None
        }
    }
}
pub fn expl_convert<'ctx>(val: Variable<'ctx>, target: Type, ctx: &CompCtx<'ctx>) -> Option<Variable<'ctx>> {impl_convert(val, target, ctx)}
fn prep_asm<'ctx>(mut arg: Variable<'ctx>, ctx: &CompCtx<'ctx>) -> Option<(BasicMetadataTypeEnum<'ctx>, BasicMetadataValueEnum<'ctx>)> {
    let i64_ty = ctx.context.i64_type();
    let i32_ty = ctx.context.i32_type();
    let i16_ty = ctx.context.i16_type();
    let i8_ty = ctx.context.i8_type();
    match arg.data_type {
        Type::Borrow(b) => {
            arg.data_type = *b;
            prep_asm(arg, ctx)
        },
        Type::Reference(b, _) => {
            arg.data_type = *b;
            if let (Some(PointerValue(val)), true) = (arg.value(ctx), arg.data_type.register()) {
                arg.comp_val = Some(ctx.builder.build_load(val, ""));
            }
            prep_asm(arg, ctx)
        },
        Type::IntLiteral => Some((i64_ty.into(), arg.value(ctx)?.into())),
        Type::Int(64, _) => Some((i64_ty.into(), arg.value(ctx)?.into())),
        Type::Int(33..=63, true) => Some((i64_ty.into(), ctx.builder.build_int_z_extend(arg.value(ctx)?.into_int_value(), i64_ty, "").into())),
        Type::Int(33..=63, false) => Some((i64_ty.into(), ctx.builder.build_int_s_extend(arg.value(ctx)?.into_int_value(), i64_ty, "").into())),
        Type::Int(32, _) => Some((i32_ty.into(), arg.value(ctx)?.into())),
        Type::Int(17..=31, true) => Some((i32_ty.into(), ctx.builder.build_int_z_extend(arg.value(ctx)?.into_int_value(), i32_ty, "").into())),
        Type::Int(17..=31, false) => Some((i32_ty.into(), ctx.builder.build_int_s_extend(arg.value(ctx)?.into_int_value(), i32_ty, "").into())),
        Type::Int(16, _) => Some((i64_ty.into(), arg.value(ctx)?.into())),
        Type::Int(9..=15, true) => Some((i16_ty.into(), ctx.builder.build_int_z_extend(arg.value(ctx)?.into_int_value(), i16_ty, "").into())),
        Type::Int(9..=15, false) => Some((i16_ty.into(), ctx.builder.build_int_s_extend(arg.value(ctx)?.into_int_value(), i16_ty, "").into())),
        Type::Int(8, _) => Some((i64_ty.into(), arg.value(ctx)?.into())),
        Type::Int(1..=7, true) => Some((i8_ty.into(), ctx.builder.build_int_z_extend(arg.value(ctx)?.into_int_value(), i8_ty, "").into())),
        Type::Int(1..=7, false) => Some((i8_ty.into(), ctx.builder.build_int_s_extend(arg.value(ctx)?.into_int_value(), i8_ty, "").into())),
        t @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 | Type::Pointer(..)) => Some((t.llvm_type(ctx).unwrap().into(), arg.comp_val.or_else(|| arg.inter_val.and_then(|x| x.into_compiled(ctx)))?.into())),
        _ => None
    }
}
pub fn call<'ctx>(mut target: Variable<'ctx>, loc: Location, cparen: Location, mut args: Vec<(Variable<'ctx>, Location)>, ctx: &CompCtx<'ctx>) -> Result<Variable<'ctx>, Diagnostic> {
    match target.data_type {
        Type::Borrow(b) => {
            target.data_type = *b;
            call(target, loc, cparen, args, ctx)
        },
        Type::Reference(b, _) => {
            if !ctx.is_const.get() && b.register() {
                if let Some(PointerValue(v)) = target.comp_val {
                    target.comp_val = Some(ctx.builder.build_load(v, ""));
                }
            }
            target.data_type = *b;
            call(target, loc, cparen, args, ctx)
        },
        Type::Function(ret, params) => {
            let mut err = Diagnostic::error(loc.clone(), 313, Some(format!("function type is {}", Type::Function(ret.clone(), params.clone())))).note(loc.clone(), {
                let mut out = format!("argument types are (");
                args.iter().for_each(|(Variable {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
                out.truncate(out.len() - 2);
                out.push(')');
                out
            });
            let suffixes = ["st", "nd", "rd", "th", "th", "th", "th", "th", "th", "th"]; // 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 0th
            let mut good = true;
            let p = params.len();
            let mut a = args.len();
            if a > p {
                err.add_note(cparen.clone(), format!("expected {p} parameters, got {a}"));
                args.truncate(p);
                a = p;
            }
            let (c, r) = args.into_iter().chain(if let Some(InterData::Function(FnData {defaults, ..})) = target.inter_val {
                let d = defaults.len();
                defaults.iter().zip(params.iter().skip(p - d)).skip(a + d - p).map(|(v, (t, c))| (Variable {
                    comp_val: if *c {None} else {v.into_compiled(ctx)},
                    inter_val: Some(v.clone()),
                    data_type: t.clone(),
                    export: true
                }, cparen.clone())).collect()
            } else {vec![]}).zip(params.iter()).enumerate().map(|(n, ((v, l), (t, c)))| {
                let e = format!("expected value of type {t} in {}{} argument, got {}", n + 1, if  n % 100 / 10 == 1 {"th"} else {suffixes[n % 10]}, v.data_type);
                (if let Some(val) = impl_convert(v.clone(), t.clone(), ctx) {
                    if *c && val.inter_val.is_none() {
                        good = false;
                        err.add_note(l.clone(), format!("{}{} argument must be const, but argument is not", n + 1, if  n % 100 / 10 == 1 {"th"} else {suffixes[n % 10]}));
                    }
                    val
                }
                else {
                    good = false;
                    err.add_note(l.clone(), e);
                    Variable::error()
                }, c)
            }).partition::<Vec<_>, _>(|(_, c)| **c);
            if !good {return Err(err)}
            if c.len() > 0 {return Err(Diagnostic::error(loc.clone(), 900, None))}
            good = true;
            let val: Option<inkwell::values::CallableValue> = if let Some(PointerValue(v)) = target.comp_val {v.try_into().ok()} else {None};
            let args: Vec<inkwell::values::BasicMetadataValueEnum> = r.into_iter().filter_map(|(Variable {comp_val, ..}, _)| comp_val.map(|v| v.into()).or_else(|| {good = false; None})).collect();
            Ok(Variable { // maybe there should be an error if val or args fails
                comp_val: if good {val.and_then(|v| ctx.builder.build_call(v, args.as_slice(), "").try_as_basic_value().left())} else {None},
                inter_val: None,
                data_type: *ret,
                export: true
            })
        },
        Type::InlineAsm => if let (Some(InterData::InlineAsm(r, c, b)), false) = (target.inter_val, ctx.is_const.get()) {
            let mut params = Vec::with_capacity(args.len());
            let mut comp_args = Vec::with_capacity(args.len());
            let suffixes = ["st", "nd", "rd", "th", "th", "th", "th", "th", "th", "th"]; // 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, 0th
            let mut good = true;
            let mut err = Diagnostic::error(loc, 432, None);
            for (n, (arg, l)) in args.into_iter().enumerate() {
                let e = format!("cannot pass value of type {}({}{} argument) to assembly", arg.data_type, n + 1, if  n % 100 / 10 == 1 {"th"} else {suffixes[n % 10]});
                if let Some((ty, val)) = prep_asm(arg, ctx) {
                    params.push(ty);
                    comp_args.push(val);
                }
                else {
                    good = false;
                    err.add_note(l, e);
                }
            }
            if !good {return Err(err)}
            if let Some(llt) = r.llvm_type(ctx) {
                let fty = llt.fn_type(&params, false);
                let asm = ctx.context.create_inline_asm(fty, b, c, true, true, None, false);
                let ret = ctx.builder.build_call(CallableValue::try_from(asm).unwrap(), &comp_args, "");
                Ok(Variable {
                    comp_val: ret.try_as_basic_value().left(),
                    inter_val: None,
                    data_type: *r,
                    export: true
                })
            }
            else {
                let fty = ctx.context.void_type().fn_type(&params, false);
                let asm = ctx.context.create_inline_asm(fty, b, c, true, true, None, false);
                ctx.builder.build_call(CallableValue::try_from(asm).unwrap(), &comp_args, "");
                Ok(Variable::null(None))
            }
        } else {Ok(Variable::error())},
        t => Err(Diagnostic::error(loc.clone(), 313, Some(format!("target type is {t}"))).info({
            let mut out = format!("argument types are (");
            args.iter().for_each(|(Variable {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
            out.truncate(out.len() - 2);
            out.push(')');
            out
        }))
    }
}
pub fn common(lhs: &Type, rhs: &Type) -> Option<Type> {
    if lhs == rhs {return Some(lhs.clone())}
    match (lhs, rhs) {
        (lhs, &Type::Reference(ref base, _) | &Type::Borrow(ref base)) if lhs == &**base => Some(lhs.clone()),
        (&Type::Reference(ref base, _) | &Type::Borrow(ref base), rhs) if rhs == &**base => Some(rhs.clone()),
        (Type::IntLiteral, x @ Type::Int(..)) | (x @ Type::Int(..), Type::IntLiteral) => Some(x.clone()),
        (Type::IntLiteral | Type::Int(..), x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) | (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), Type::IntLiteral | Type::Int(..)) => Some(x.clone()),
        (Type::Float32, Type::Float16) | (Type::Float16, Type::Float32) => Some(Type::Float32),
        (Type::Float64, Type::Float16 | Type::Float32) | (Type::Float16 | Type::Float32, Type::Float64) => Some(Type::Float64),
        (Type::Float128, Type::Float16 | Type::Float32 | Type::Float64) | (Type::Float16 | Type::Float32 | Type::Float64, Type::Float128) => Some(Type::Float128),
        _ => None
    }
}
