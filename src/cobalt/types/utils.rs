use crate::*;
use std::cmp::{min, max};
use inkwell::values::{BasicValueEnum::*, BasicMetadataValueEnum, CallableValue};
use inkwell::types::{BasicType, BasicMetadataTypeEnum};
use inkwell::{
    IntPredicate::{SLT, ULT, SGT, UGT, SLE, ULE, SGE, UGE, EQ, NE},
    FloatPredicate::{OLT, OGT, OLE, OGE, OEQ, ONE}
};
pub fn impl_convertible(base: Type, target: Type) -> bool {
    base == target || target == Type::Null || target == Type::Error || match base {
        Type::Borrow(b) => impl_convertible(*b, target),
        Type::Reference(b, true) =>
            if target == Type::Reference(b.clone(), false) {true}
            else {
                match *b {
                    Type::Array(b, _) => target == Type::Pointer(b.clone(), true) || target == Type::Pointer(b, false),
                    b => impl_convertible(b, target)
                }
            },
        Type::Reference(b, false) => match *b {
            Type::Array(b, _) => target == Type::Pointer(b.clone(), true) || target == Type::Pointer(b, false),
            b => impl_convertible(b, target)
        },
        Type::IntLiteral => matches!(target, Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128),
        Type::Int(s, _) => match target {Type::Int(s2, _) if s2 >= s => true, Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 => true, _ => false},
        Type::Float16 => matches!(target, Type::Float32 | Type::Float64 | Type::Float128),
        Type::Float32 => matches!(target, Type::Float64 | Type::Float128),
        Type::Float64 => target == Type::Float128,
        Type::Pointer(_, false) => target == Type::Pointer(Box::new(Type::Null), false),
        Type::Pointer(b, true) => target == Type::Pointer(Box::new(Type::Null), false) || target == Type::Pointer(Box::new(Type::Null), true) || target == Type::Pointer(b, false),
        Type::Error => true,
        _ => false
    }
}
pub fn expl_convertible(base: Type, target: Type) -> bool {
    base == target || target == Type::Null || target == Type::Error || match base {
        Type::Borrow(b) => expl_convertible(*b, target),
        Type::Reference(b, true) =>
            if target == Type::Reference(b.clone(), false) {true}
            else {
                match *b {
                    Type::Array(b, _) => target == Type::Pointer(b.clone(), true) || target == Type::Pointer(b, false),
                    b => expl_convertible(b, target)
                }
            },
        Type::Reference(b, false) => match *b {
            Type::Array(b, _) => target == Type::Pointer(b.clone(), true) || target == Type::Pointer(b, false),
            b => expl_convertible(b, target)
        },
        Type::IntLiteral => matches!(target, Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128),
        Type::Int(..) => matches!(target, Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 | Type::Pointer(..)),
        Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 => matches!(target, Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128),
        Type::Pointer(ref b, true) if **b == Type::Null => matches!(target, Type::Pointer(..)),
        Type::Pointer(ref b, false) if **b == Type::Null => matches!(target, Type::Pointer(_, false)),
        Type::Pointer(_, false) => target == Type::Pointer(Box::new(Type::Null), false),
        Type::Pointer(b, true) => target == Type::Pointer(Box::new(Type::Null), false) || target == Type::Pointer(Box::new(Type::Null), true) || target == Type::Pointer(b, false),
        Type::Error => true,
        _ => false
    }
}
pub fn bin_type(lhs: Type, rhs: Type, op: &str) -> Type {
    match (lhs, rhs) {
        (l, Type::Reference(x, _) | Type::Borrow(x)) => bin_type(l, *x, op),
        (Type::Int(ls, lu), Type::Int(rs, ru)) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" => Type::Int(max(ls, rs), lu && ru),
            _ => Type::Error
        },
        (x @ Type::Int(..), Type::IntLiteral) | (Type::IntLiteral, x @ Type::Int(..)) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" => x,
            _ => Type::Error
        },
        (Type::IntLiteral, Type::IntLiteral) => match op {
            "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" => Type::IntLiteral,
            _ => Type::Error
        },
        (Type::Int(..) | Type::IntLiteral, x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) | (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), Type::Int(..) | Type::IntLiteral) => match op {
            "+" | "-" | "*" | "/" | "%" => x,
            _ => Type::Error
        },
        (Type::Float16, Type::Float16) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float16,
            _ => Type::Error
        },
        (Type::Float32, Type::Float16 | Type::Float32) | (Type::Float16, Type::Float32) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float32,
            _ => Type::Error
        },
        (Type::Float64, Type::Float16 | Type::Float32 | Type::Float64) | (Type::Float16 | Type::Float32, Type::Float64) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float64,
            _ => Type::Error
        },
        (Type::Float128, Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) | (Type::Float16 | Type::Float32 | Type::Float64, Type::Float128) => match op {
            "+" | "-" | "*" | "/" | "%" => Type::Float128,
            _ => Type::Error
        },
        (x @ Type::Pointer(..), Type::IntLiteral | Type::Int(..)) | (Type::IntLiteral | Type::Int(..), x @ Type::Pointer(..)) => match op {
            "+" | "-" => x,
            _ => Type::Error
        }
        (Type::Reference(x, true), r) => match (*x, r) {
            (Type::IntLiteral, _) => panic!("There shouldn't be a reference to an integer literal"),
            (x @ Type::Int(..), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => Type::Reference(Box::new(x), true),
                _ => bin_type(x, r, op)
            },
            (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) => match op {
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" => Type::Reference(Box::new(x), true),
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
        _ => Type::Error
    }
}
pub fn pre_type(val: Type, op: &str) -> Type {
    match val {
        Type::Reference(x, true) => match *x {
            Type::IntLiteral => panic!("There shouldn't be a reference to an integer literal"),
            x @ (Type::Int(..) | Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 | Type::Pointer(..)) => match op {
                "++" | "--" => Type::Reference(Box::new(x), true),
                _ => Type::Error
            }
            x => pre_type(x, op)
        }
        Type::Reference(x, false) | Type::Borrow(x) => pre_type(*x, op),
        x @ (Type::IntLiteral | Type::Int(..)) => match op {
            "+" | "-" | "~" => x,
            _ => Type::Error
        },
        x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
            "+" | "-" => x,
            _ => Type::Error
        }
        Type::Pointer(b, c) => match op {
            "+" | "-" => Type::Pointer(b, c),
            "*" => Type::Reference(b, c),
            _ => Type::Error
        }
        _ => Type::Error
    }
}
pub fn post_type(val: Type, op: &str) -> Type {
    match val {
        Type::Reference(x, _) | Type::Borrow(x) => post_type(*x, op),
        Type::TypeData | Type::Null => match op {
            "mut&" | "mut*" | "const&" | "const*" | "^" => Type::TypeData,
            _ => Type::Error
        },
        Type::Tuple(v) if v.iter().all(|v| v == &Type::TypeData) => match op {
            "mut&" | "mut*" | "const&" | "const*" | "^" => Type::TypeData,
            _ => Type::Error
        },
        _ => Type::Error
    }
}
pub fn sub_type(val: Type, idx: Type) -> Type {
    match idx {
        Type::Borrow(x) | Type::Reference(x, _) => sub_type(val, *x),
        i => match val {
            Type::Borrow(x) => sub_type(*x, i),
            Type::Pointer(b, m) => match i {Type::IntLiteral | Type::Int(..) => Type::Reference(b, m), _ => Type::Error},
            Type::Reference(b, m) => match *b {
                Type::Array(b, _) => match i {Type::IntLiteral | Type::Int(..) => Type::Reference(b, m), _ => Type::Error},
                x => sub_type(x, i)
            },
            Type::TypeData | Type::Null => match i {
                Type::Int(..) | Type::IntLiteral | Type::Null => Type::TypeData,
                _ => Type::Error
            },
            Type::Tuple(v) if v.iter().all(|v| v == &Type::TypeData) => match i {
                Type::Int(..) | Type::IntLiteral | Type::Null => Type::TypeData,
                _ => Type::Error
            },
            _ => Type::Error
        }
    }
}
pub fn call_type<'ctx>(target: Type, args: Vec<Value<'ctx>>) -> Type {
    match target {
        Type::Function(ret, _) => *ret,
        Type::InlineAsm(b) => *b,
        Type::Borrow(b) => if matches!(*b, Type::Tuple(..)) {Type::Borrow(Box::new(call_type(*b, args)))} else {call_type(*b, args)},
        Type::Reference(b, m) => if matches!(*b, Type::Tuple(..)) {Type::Reference(Box::new(call_type(*b, args)), m)} else {call_type(*b, args)},
        Type::Tuple(v) => if args.len() == 1 {
            let mut val = args.into_iter().next().unwrap();
            match val.data_type {
                Type::Borrow(b) | Type::Reference(b, _) => {
                    val.data_type = *b;
                    call_type(Type::Tuple(v), vec![val])
                },
                Type::IntLiteral | Type::Int(..) => if let Some(InterData::Int(i)) = val.inter_val {v.get(i as usize).cloned().unwrap_or(Type::Error)} else {Type::Error},
                _ => Type::Error
            }
        } else {Type::Error},
        _ => Type::Error
    }
}
pub fn attr_type(target: Type, attr: &str) -> Type {
    match target {
        Type::Borrow(b) | Type::Reference(b, _) => attr_type(*b, attr),
        _ => Type::Error
    }
}
pub fn bin_op<'ctx>(loc: Location, (mut lhs, lloc): (Value<'ctx>, Location), (mut rhs, rloc): (Value<'ctx>, Location), op: &str, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let ln = lhs.data_type.to_string();
    let rn = rhs.data_type.to_string();
    let err = Diagnostic::error(loc.clone(), 310, Some(format!("binary operator {op} is not defined for values of types {ln} and {rn}")))
        .note(lloc.clone(), format!("left value is of type {ln}"))
        .note(rloc.clone(), format!("right value is of type {rn}"));
    match (lhs.data_type, rhs.data_type) {
        (Type::Borrow(l), r) => {
            lhs.data_type = *l;
            rhs.data_type = r;
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (l, Type::Borrow(r)) => {
            lhs.data_type = l;
            rhs.data_type = *r;
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Reference(l, false), r) => {
            lhs.data_type = *l;
            rhs.data_type = r;
            if !ctx.is_const.get() && lhs.data_type.register() {
                if let Some(v) = lhs.comp_val {
                    lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (l, Type::Reference(r, _)) => {
            lhs.data_type = l;
            rhs.data_type = *r;
            if !ctx.is_const.get() && rhs.data_type.register() {
                if let Some(v) = rhs.comp_val {
                    rhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Reference(l, true), r) => match (*l, r) {
            (Type::IntLiteral, _) => panic!("There shouldn't be a reference to an integer literal"),
            (l @ Type::Int(..), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        ctx.builder.build_store(l, r);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "+=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_int_add(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "-=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_int_sub(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "*=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_int_mul(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "/=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = if unsigned {ctx.builder.build_int_unsigned_div(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_div(v1, r.into_int_value(), "")};
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "%=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = if unsigned {ctx.builder.build_int_unsigned_rem(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_rem(v1, r.into_int_value(), "")};
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "&=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_and(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "|=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_or(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "^=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_xor(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "<<=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_left_shift(v1, r.into_int_value(), "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                ">>=" => {
                    lhs.data_type = Type::Reference(Box::new(l), true);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = ctx.builder.build_load(l, "").into_int_value();
                        let v2 = ctx.builder.build_right_shift(v1, r.into_int_value(), false, "");
                        ctx.builder.build_store(l, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = l;
                    rhs.data_type = r;
                    if !ctx.is_const.get() && lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            },
            (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x.clone()), true);
                    rhs.data_type = r.clone();
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        ctx.builder.build_store(l, v1);
                    }
                    Ok(lhs)
                },
                "+=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x.clone()), true);
                    rhs.data_type = r.clone();
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(l, "").into_float_value();
                        let v3 = ctx.builder.build_float_add(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "-=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x.clone()), true);
                    rhs.data_type = r.clone();
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(l, "").into_float_value();
                        let v3 = ctx.builder.build_float_sub(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "*=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x.clone()), true);
                    rhs.data_type = r.clone();
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(l, "").into_float_value();
                        let v3 = ctx.builder.build_float_mul(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "/=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Reference(Box::new(x.clone()), true);
                    rhs.data_type = r.clone();
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(l, "").into_float_value();
                        let v3 = ctx.builder.build_float_div(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "%=" => Err(err), // TODO: implement fmod
                _ => Err(err)
            },
            (x @ Type::Pointer(..), y) if x == y => match op {
                "=" => {
                    lhs.data_type = x;
                    rhs.data_type = y;
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        ctx.builder.build_store(l, r);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = x;
                    rhs.data_type = y;
                    if lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            }
            (Type::Pointer(b, m), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "+=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Pointer(b.clone(), m);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) = (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                        unsafe {
                            let lv = ctx.builder.build_load(l, "").into_pointer_value();
                            let v = ctx.builder.build_gep(lv, &[r], "");
                            ctx.builder.build_store(l, v);
                        }
                    }
                    Ok(lhs)
                },
                "-=" => {
                    lhs.inter_val = None;
                    lhs.data_type = Type::Pointer(b.clone(), m);
                    rhs.data_type = r;
                    if let (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) = (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                        unsafe {
                            let lv = ctx.builder.build_load(l, "").into_pointer_value();
                            let rv = ctx.builder.build_int_neg(r, "");
                            let v = ctx.builder.build_gep(lv, &[rv], "");
                            ctx.builder.build_store(l, v);
                        }
                    }
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = Type::Pointer(b, m);
                    rhs.data_type = r;
                    if !ctx.is_const.get() && lhs.data_type.register() {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
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
                bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
            }
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls > rs => {
            if let (Some(IntValue(val)), false) = (rhs.comp_val, ctx.is_const.get()) {
                rhs.comp_val = Some(IntValue(if ru {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}));
            }
            lhs.data_type = Type::Int(ls, lu);
            rhs.data_type = Type::Int(ls, ru);
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls < rs => {
            if let (Some(IntValue(val)), false) = (lhs.comp_val, ctx.is_const.get()) {
                lhs.comp_val = Some(IntValue(if ru {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}));
            }
            lhs.data_type = Type::Int(rs, lu);
            rhs.data_type = Type::Int(rs, ru);
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls == rs => match op {
            "+" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_add(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l + r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu && ru)
            )),
            "-" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_sub(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l - r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu && ru)
            )),
            "*" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_mul(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l * r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu && ru)
            )),
            "/" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(if ru {ctx.builder.build_int_unsigned_div(l, r, "")} else {ctx.builder.build_int_signed_div(l, r, "")})),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l / r)),
                    _ => None
                },
                Type::Int(max(ls, rs), ru)
            )),
            "%" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(if ru {ctx.builder.build_int_unsigned_rem(l, r, "")} else {ctx.builder.build_int_signed_rem(l, r, "")})),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l % r)),
                    _ => None
                },
                Type::Int(max(ls, rs), ru)
            )),
            "&" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_and(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l & r)),
                    _ => None
                },
                Type::Int(min(ls, rs), lu || ru)
            )),
            "|" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_or(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l | r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu || ru)
            )),
            "^" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_xor(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l ^ r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu || ru)
            )),
            "<<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_left_shift(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l << r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu || ru)
            )),
            ">>" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_right_shift(l, r, false, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l >> r)),
                    _ => None
                },
                Type::Int(max(ls, rs), lu || ru)
            )),
            "<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {ULT} else {SLT}, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l < r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {UGT} else {SGT}, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l > r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "<=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {ULE} else {SLE}, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l <= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(if lu && ru {UGE} else {SGE}, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l >= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "==" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(EQ, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l == r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "!=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(NE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l != r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            _ => Err(err)
        },
        (x @ Type::Int(..), Type::IntLiteral) => bin_op(loc, (Value {data_type: x.clone(), ..lhs}, lloc), (impl_convert((0, 0..0), (Value {data_type: Type::IntLiteral, ..rhs}, None), (x, None), ctx).unwrap(), rloc), op, ctx),
        (Type::IntLiteral, x @ Type::Int(..)) => {
            let t = x.clone();
            lhs.data_type = Type::IntLiteral;
            bin_op(loc, (impl_convert((0, 0..0), (lhs, None), (x, None), ctx).unwrap(), lloc), (Value {data_type: t, ..rhs}, rloc), op, ctx)
        },
        (Type::IntLiteral, Type::IntLiteral) => match op {
            "+" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_add(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l + r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "-" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_sub(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l - r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "*" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_mul(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l * r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "/" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_signed_div(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l / r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "%" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_signed_rem(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l % r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "&" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_and(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l & r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "|" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_or(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l | r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "^" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_xor(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l ^ r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "<<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_left_shift(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l << r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            ">>" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_right_shift(l, r, false, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(l >> r)),
                    _ => None
                },
                Type::IntLiteral
            )),
            "<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SLT, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l < r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SGT, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l > r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "<=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SLE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l <= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(SGE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l >= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "==" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(EQ, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l == r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "!=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(IntValue(l)), Some(IntValue(r)), false) => Some(IntValue(ctx.builder.build_int_compare(NE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Int(l)), Some(InterData::Int(r))) => Some(InterData::Int(i128::from(l != r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            _ => Err(err)
        },
        (Type::Pointer(b, s), Type::Int(..) | Type::IntLiteral) => match op {
            "+" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(l, &[r], "").into()}),
                    _ => None
                },
                None,
                Type::Pointer(b, s)
            )),
            "-" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, b.size(), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {
                        let v = ctx.builder.build_int_neg(r, "");
                        ctx.builder.build_gep(l, &[v], "").into()
                    }),
                    _ => None
                },
                None,
                Type::Pointer(b, s)
            )),
            _ => Err(err)
        },
        (Type::Int(..) | Type::IntLiteral, Type::Pointer(b, s)) => match op {
            "+" => Ok(Value::new(
                match (rhs.comp_val, lhs.comp_val, b.size(), ctx.is_const.get()) { // I just swapped the sides here
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(l, &[r], "").into()}),
                    _ => None
                },
                None,
                Type::Pointer(b, s)
            )),
            _ => Err(err)
        },
        (l @ Type::Pointer(..), r @ Type::Pointer(..)) => match op {
            "-" if l == r => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_sub(v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(64, false)
            )),
            "<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(ULT, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            ">" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(UGT, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            "<=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(ULE, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            ">=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(UGE, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            "==" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(EQ, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            "!=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(PointerValue(r)), false) => {
                        let pt = ctx.context.custom_width_int_type(ctx.flags.word_size as u32 * 8);
                        let v1 = ctx.builder.build_ptr_to_int(l, pt, "");
                        let v2 = ctx.builder.build_ptr_to_int(r, pt, "");
                        Some(IntValue(ctx.builder.build_int_compare(NE, v1, v2, "")))
                    },
                    _ => None
                },
                None,
                Type::Int(1, false)
            )),
            _ => Err(err)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) if l.size() < r.size() => {
            lhs.comp_val = match (lhs.comp_val, ctx.is_const.get()) {
                (Some(FloatValue(l)), false) => Some(FloatValue(ctx.builder.build_float_cast(l, r.llvm_type(ctx).unwrap().into_float_type(), ""))),
                _ => None
            };
            lhs.data_type = r.clone();
            rhs.data_type = r;
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::Float16 | Type::Float32 | Type::Float64)) if l.size() > r.size() => {
            rhs.comp_val = match (rhs.comp_val, ctx.is_const.get()) {
                (Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_cast(r, l.llvm_type(ctx).unwrap().into_float_type(), ""))),
                _ => None
            };
            lhs.data_type = l.clone();
            rhs.data_type = l;
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) if l == r => match op {
            "+" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_add(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l + r)),
                    _ => None
                },
                l
            )),
            "-" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_sub(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l - r)),
                    _ => None
                },
                l
            )),
            "*" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_mul(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l * r)),
                    _ => None
                },
                l
            )),
            "/" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(FloatValue(ctx.builder.build_float_div(l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l / r)),
                    _ => None
                },
                l
            )),
            "%" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(_l)), Some(FloatValue(_r)), false) => None, // TODO: implement fmod
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Float(l.rem_euclid(r))),
                    _ => None
                },
                l
            )),
            "<" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OLT, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l < r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OGT, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l > r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "<=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OLE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l <= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            ">=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OGE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l >= r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "==" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(OEQ, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l == r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            "!=" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, ctx.is_const.get()) {
                    (Some(FloatValue(l)), Some(FloatValue(r)), false) => Some(IntValue(ctx.builder.build_float_compare(ONE, l, r, ""))),
                    _ => None
                },
                match (lhs.inter_val, rhs.inter_val) {
                    (Some(InterData::Float(l)), Some(InterData::Float(r))) => Some(InterData::Int(i128::from(l != r))),
                    _ => None
                },
                Type::Int(1, false)
            )),
            _ => Err(err)
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
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
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
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        _ => Err(err)
    }
}
pub fn pre_op<'ctx>(loc: Location, (mut val, vloc): (Value<'ctx>, Location), op: &str, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let n = val.data_type.to_string();
    let err = Diagnostic::error(loc.clone(), 310, Some(format!("postfix operator {op} is not defined for value of type {n}")))
        .note(vloc.clone(), format!("value is of type {n}"));
    match val.data_type {
        Type::Borrow(x) => {
            val.data_type = *x;
            pre_op(loc, (val, vloc), op, ctx)
        },
        Type::Reference(x, false) => if op == "&" {
            val.data_type = Type::Pointer(x, false);
            Ok(val)
        }
        else {
            val.data_type = *x;
            if !ctx.is_const.get() && val.data_type.register() {
                if let Some(v) = val.comp_val {
                    val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                }
            }
            pre_op(loc, (val, vloc), op, ctx)
        },
        Type::Reference(x, true) => if op == "&" {
            val.data_type = Type::Pointer(x, true);
            Ok(val)
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
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, x.llvm_type(ctx).unwrap().into_int_type().const_int(1, false), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
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
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_float_value();
                            let v2 = ctx.builder.build_float_sub(v1, x.llvm_type(ctx).unwrap().into_float_type().const_float(1.0), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
                    }
                },
                Type::Pointer(b, m) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), SizeType::Static(_), false) = (val.comp_val, b.size(), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_pointer_value();
                            let v2 = unsafe {ctx.builder.build_gep(v1, &[ctx.context.i8_type().const_int(1, true)], "")};
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), SizeType::Static(_), false) = (val.comp_val, b.size(), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(v, "").into_pointer_value();
                            let v2 = unsafe {ctx.builder.build_gep(v1, &[ctx.context.i8_type().const_int(u64::MAX, true)], "")};
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Ok(val)
                    },
                    _ => {
                        val.data_type = Type::Pointer(b, m);
                        if !ctx.is_const.get() && val.data_type.register() {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
                    }
                },
                x => {
                    val.data_type = x;
                    if !ctx.is_const.get() && val.data_type.register() {
                        if let Some(v) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(v.into_pointer_value(), ""));
                        }
                    }
                    pre_op(loc, (val, vloc), op, ctx)
                }
            }
        },
        Type::IntLiteral => match op {
            "+" => {
                val.data_type = Type::IntLiteral;
                Ok(val)
            },
            "-" => Ok(Value::new(
                if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_int_neg(v, "")))} else {None},
                if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(-v))} else {None},
                Type::IntLiteral
            )),
            "~" => Ok(Value::new(
                if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_xor(v, ctx.context.i64_type().const_all_ones(), "")))} else {None},
                if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(!v))} else {None},
                Type::IntLiteral
            )),
            _ => Err(err)
        },
        Type::Int(s, u) => match op {
            "+" => {
                val.data_type = Type::Int(s, u);
                Ok(val)
            },
            "-" => Ok(Value::new(
                if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_int_neg(v, "")))} else {None},
                if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(-v))} else {None},
                Type::Int(s, u)
            )),
            "~" => Ok(Value::new(
                if let (Some(IntValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(IntValue(ctx.builder.build_xor(v, ctx.context.custom_width_int_type(s as u32).const_all_ones(), "")))} else {None},
                if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(!v))} else {None},
                Type::Int(s, u)
            )),
            _ => Err(err)
        },
        x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
            "+" => {
                val.data_type = x;
                Ok(val)
            },
            "-" => Ok(Value::new(
                if let (Some(FloatValue(v)), false) = (val.comp_val, ctx.is_const.get()) {Some(FloatValue(ctx.builder.build_float_neg(v, "")))} else {None},
                if let Some(InterData::Float(v)) = val.inter_val {Some(InterData::Float(-v))} else {None},
                x
            )),
            _ => Err(err)
        }
        Type::Pointer(b, m) => match op {
            "*" => {
                val.data_type = Type::Reference(b, m);
                Ok(val)
            },
            _ => Err(err)
        },
        _ => Err(err)
    }
}
pub fn post_op<'ctx>(loc: Location, (val, vloc): (Value<'ctx>, Location), op: &str, _ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let n = val.data_type.to_string();
    let err = Diagnostic::error(loc, 310, Some(format!("prefix operator {op} is not defined for value of type {n}")))
        .note(vloc, format!("value is of type {n}"));
    match val.data_type {
        Type::TypeData => match op {
            "mut&" => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Reference(t, true)))} else {Err(err)},
            "mut*" => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Pointer(t, true)))} else {Err(err)},
            "const&" => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Reference(t, false)))} else {Err(err)},
            "const*" => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Pointer(t, false)))} else {Err(err)},
            "^" => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Borrow(t)))} else {Err(err)},
            _ => Err(err)
        },
        Type::Null => match op {
            "mut&" => Ok(Value::make_type(Type::Reference(Box::new(Type::Null), true))),
            "mut*" => Ok(Value::make_type(Type::Pointer(Box::new(Type::Null), true))),
            "const&" => Ok(Value::make_type(Type::Reference(Box::new(Type::Null), false))),
            "const*" => Ok(Value::make_type(Type::Pointer(Box::new(Type::Null), false))),
            "^" => Ok(Value::make_type(Type::Borrow(Box::new(Type::Null)))),
            _ => Err(err)
        },
        Type::Tuple(v) => {
            if let Some(InterData::Array(a)) = val.inter_val {
                let mut vec = Vec::with_capacity(v.len());
                for t in a.into_iter().zip(v) {
                    if let (InterData::Type(t), Type::TypeData) = t {vec.push(*t);}
                    else {return Err(err);}
                }
                match op {
                    "mut&" => Ok(Value::make_type(Type::Reference(Box::new(Type::Tuple(vec)), true))),
                    "mut*" => Ok(Value::make_type(Type::Pointer(Box::new(Type::Tuple(vec)), true))),
                    "const&" => Ok(Value::make_type(Type::Reference(Box::new(Type::Tuple(vec)), false))),
                    "const*" => Ok(Value::make_type(Type::Pointer(Box::new(Type::Tuple(vec)), false))),
                    "^" => Ok(Value::make_type(Type::Borrow(Box::new(Type::Tuple(vec))))),
                    _ => Err(err)
                }
            }
            else {Err(err)}
        },
        _ => Err(err)
    }
}
pub fn subscript<'ctx>((mut val, vloc): (Value<'ctx>, Location), (mut idx, iloc): (Value<'ctx>, Location), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let b = val.data_type.to_string();
    let s = idx.data_type.to_string();
    let err = Diagnostic::error(vloc.clone(), 318, Some(format!("cannot subscript value of type {b} with value of type {s}"))).note(vloc.clone(), format!("base type is {b}")).note(iloc.clone(), format!("subscript type is {s}"));
    match idx.data_type {
        Type::Borrow(x) => {
            idx.data_type = *x;
            subscript((val, vloc), (idx, iloc), ctx)
        },
        Type::Reference(x, _) => {
            if x.register() && !ctx.is_const.get() {
                if let Some(PointerValue(v)) = idx.comp_val {
                    idx.comp_val = Some(ctx.builder.build_load(v, ""));
                }
            }
            idx.data_type = *x;
            subscript((val, vloc), (idx, iloc), ctx)
        },
        a => {
            idx.data_type = a;
            match val.data_type.clone() {
                Type::Pointer(b, m) => match idx.data_type.clone() {
                    Type::IntLiteral | Type::Int(..) => Ok(Value::new(
                        match (val.comp_val, idx.value(ctx), b.size(), ctx.is_const.get()) {
                            (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(l, &[r], "").into()}),
                            _ => None
                        },
                        None,
                        Type::Reference(b, m)
                    )),
                    _ => Err(err)
                },
                Type::Reference(b, m) => match *b {
                    Type::Array(b, None) => match idx.data_type.clone() {
                        Type::IntLiteral | Type::Int(_, true) => Ok(Value::new(
                            if let (Some(StructValue(sv)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(), ctx.is_const.get()) {
                                let raw = ctx.builder.build_extract_value(sv, 0, "").unwrap().into_pointer_value();
                                if ctx.flags.bounds_checks {
                                    let len = ctx.builder.build_extract_value(sv, 1, "").unwrap().into_int_value();
                                    let f = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
                                    let ge0 = ctx.context.append_basic_block(f, "idx.ge0"); // greater than or equal to 0
                                    let ltm = ctx.context.append_basic_block(f, "idx.ltm"); // less than max
                                    let bad = ctx.context.append_basic_block(f, "idx.bad");
                                    let merge = ctx.context.append_basic_block(f, "merge");
                                    let lt0cmp = ctx.builder.build_int_compare(SLT, iv, idx.data_type.llvm_type(ctx).unwrap().const_zero().into_int_value(), "");
                                    ctx.builder.build_conditional_branch(lt0cmp, bad, ge0);
                                    ctx.builder.position_at_end(ge0);
                                    let gtmcmp = ctx.builder.build_int_compare(SLT, iv, len, "");
                                    ctx.builder.build_conditional_branch(gtmcmp, ltm, bad);
                                    ctx.builder.position_at_end(ltm);
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(raw, &[iv], ""))};
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(bad);
                                    if let Some(ef) = ctx.module.get_function("cobalt.funcs.array_bounds") {
                                        let i64t = ctx.context.i64_type();
                                        ctx.builder.build_call(ef, &[ctx.builder.build_int_cast(iv, i64t, "").into(), ctx.builder.build_int_cast(len, i64t, "").into()], "");
                                    }
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(merge);
                                    let phi = ctx.builder.build_phi(val.get_type(), "");
                                    phi.add_incoming(&[(&val, ltm), (&val.get_type().const_zero(), bad)]);
                                    Some(phi.as_basic_value())
                                }
                                else {Some(unsafe {ctx.builder.build_gep(raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        Type::Int(_, false) => Ok(Value::new(
                            if let (Some(StructValue(sv)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(), ctx.is_const.get()) {
                                let raw = ctx.builder.build_extract_value(sv, 0, "").unwrap().into_pointer_value();
                                if ctx.flags.bounds_checks {
                                    let len = ctx.builder.build_extract_value(sv, 1, "").unwrap().into_int_value();
                                    let f = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
                                    let ge0 = ctx.context.append_basic_block(f, "idx.ge0"); // greater than or equal to 0
                                    let ltm = ctx.context.append_basic_block(f, "idx.ltm"); // less than max
                                    let bad = ctx.context.append_basic_block(f, "idx.bad");
                                    let merge = ctx.context.append_basic_block(f, "merge");
                                    let lt0cmp = ctx.builder.build_int_compare(ULT, iv, idx.data_type.llvm_type(ctx).unwrap().const_zero().into_int_value(), "");
                                    ctx.builder.build_conditional_branch(lt0cmp, bad, ge0);
                                    ctx.builder.position_at_end(ge0);
                                    let gtmcmp = ctx.builder.build_int_compare(ULT, iv, len, "");
                                    ctx.builder.build_conditional_branch(gtmcmp, ltm, bad);
                                    ctx.builder.position_at_end(ltm);
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(raw, &[iv], ""))};
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(bad);
                                    if let Some(ef) = ctx.module.get_function("cobalt.funcs.array_bounds") {
                                        let i64t = ctx.context.i64_type();
                                        ctx.builder.build_call(ef, &[ctx.builder.build_int_cast(iv, i64t, "").into(), ctx.builder.build_int_cast(len, i64t, "").into()], "");
                                    }
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(merge);
                                    let phi = ctx.builder.build_phi(val.get_type(), "");
                                    phi.add_incoming(&[(&val, ltm), (&val.get_type().const_zero(), bad)]);
                                    Some(phi.as_basic_value())
                                }
                                else {Some(unsafe {ctx.builder.build_gep(raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        _ => Err(err)
                    },
                    Type::Array(b, Some(s)) => match idx.data_type.clone() {
                        Type::IntLiteral | Type::Int(_, true) => Ok(Value::new(
                            if let (Some(PointerValue(raw)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(), ctx.is_const.get()) {
                                if ctx.flags.bounds_checks {
                                    let len = idx.data_type.llvm_type(ctx).unwrap().into_int_type().const_int(s as u64, false);
                                    let f = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
                                    let ge0 = ctx.context.append_basic_block(f, "idx.ge0"); // greater than or equal to 0
                                    let ltm = ctx.context.append_basic_block(f, "idx.ltm"); // less than max
                                    let bad = ctx.context.append_basic_block(f, "idx.bad");
                                    let merge = ctx.context.append_basic_block(f, "merge");
                                    let lt0cmp = ctx.builder.build_int_compare(SLT, iv, idx.data_type.llvm_type(ctx).unwrap().const_zero().into_int_value(), "");
                                    ctx.builder.build_conditional_branch(lt0cmp, bad, ge0);
                                    ctx.builder.position_at_end(ge0);
                                    let gtmcmp = ctx.builder.build_int_compare(SLT, iv, len, "");
                                    ctx.builder.build_conditional_branch(gtmcmp, ltm, bad);
                                    ctx.builder.position_at_end(ltm);
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(raw, &[iv], ""))};
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(bad);
                                    if let Some(ef) = ctx.module.get_function("cobalt.funcs.array_bounds") {
                                        let i64t = ctx.context.i64_type();
                                        ctx.builder.build_call(ef, &[ctx.builder.build_int_cast(iv, i64t, "").into(), ctx.builder.build_int_cast(len, i64t, "").into()], "");
                                    }
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(merge);
                                    let phi = ctx.builder.build_phi(val.get_type(), "");
                                    phi.add_incoming(&[(&val, ltm), (&val.get_type().const_zero(), bad)]);
                                    Some(phi.as_basic_value())
                                }
                                else {Some(unsafe {ctx.builder.build_gep(raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        Type::Int(_, false) => Ok(Value::new(
                            if let (Some(PointerValue(raw)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(), ctx.is_const.get()) {
                                if ctx.flags.bounds_checks {
                                    let len = idx.data_type.llvm_type(ctx).unwrap().into_int_type().const_int(s as u64, false);
                                    let f = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
                                    let ge0 = ctx.context.append_basic_block(f, "idx.ge0"); // greater than or equal to 0
                                    let ltm = ctx.context.append_basic_block(f, "idx.ltm"); // less than max
                                    let bad = ctx.context.append_basic_block(f, "idx.bad");
                                    let merge = ctx.context.append_basic_block(f, "merge");
                                    let lt0cmp = ctx.builder.build_int_compare(ULT, iv, idx.data_type.llvm_type(ctx).unwrap().const_zero().into_int_value(), "");
                                    ctx.builder.build_conditional_branch(lt0cmp, bad, ge0);
                                    ctx.builder.position_at_end(ge0);
                                    let gtmcmp = ctx.builder.build_int_compare(ULT, iv, len, "");
                                    ctx.builder.build_conditional_branch(gtmcmp, ltm, bad);
                                    ctx.builder.position_at_end(ltm);
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(raw, &[iv], ""))};
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(bad);
                                    if let Some(ef) = ctx.module.get_function("cobalt.funcs.array_bounds") {
                                        let i64t = ctx.context.i64_type();
                                        ctx.builder.build_call(ef, &[ctx.builder.build_int_cast(iv, i64t, "").into(), ctx.builder.build_int_cast(len, i64t, "").into()], "");
                                    }
                                    ctx.builder.build_unconditional_branch(merge);
                                    ctx.builder.position_at_end(merge);
                                    let phi = ctx.builder.build_phi(val.get_type(), "");
                                    phi.add_incoming(&[(&val, ltm), (&val.get_type().const_zero(), bad)]);
                                    Some(phi.as_basic_value())
                                }
                                else {Some(unsafe {ctx.builder.build_gep(raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        _ => Err(err)
                    },
                    x => {
                        if !ctx.is_const.get() || x.register() {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v, ""));
                            }
                        }
                        val.data_type = x;
                        subscript((val, vloc), (idx, iloc), ctx)
                    }
                },
                Type::TypeData => match idx.data_type {
                    Type::Null => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Array(t, None)))} else {unreachable!()},
                    Type::Int(..) | Type::IntLiteral => if let (Some(InterData::Type(t)), Some(InterData::Int(v))) = (val.inter_val, idx.inter_val) {Ok(Value::make_type(Type::Array(t, Some(v as u32))))} else {Err(Diagnostic::error(iloc, 324, None))},
                    _ => Err(err)
                },
                Type::Null => match idx.data_type {
                    Type::Null => Ok(Value::make_type(Type::Array(Box::new(Type::Null), None))),
                    Type::Int(..) | Type::IntLiteral => if let Some(InterData::Int(v)) = idx.inter_val {Ok(Value::make_type(Type::Array(Box::new(Type::Null), Some(v as u32))))} else {Err(Diagnostic::error(iloc, 324, None))},
                    _ => Err(err)
                },
                Type::Tuple(v) => {
                    if let Some(InterData::Array(a)) = val.inter_val {
                        let mut vec = Vec::with_capacity(v.len());
                        for t in a.into_iter().zip(v) {
                            if let (InterData::Type(t), Type::TypeData) = t {vec.push(*t);}
                            else {return Err(err);}
                        }
                        match idx.data_type {
                            Type::Null => Ok(Value::make_type(Type::Array(Box::new(Type::Tuple(vec)), None))),
                            Type::Int(..) | Type::IntLiteral => if let Some(InterData::Int(v)) = idx.inter_val {Ok(Value::make_type(Type::Array(Box::new(Type::Tuple(vec)), Some(v as u32))))} else {Err(Diagnostic::error(iloc, 324, None))},
                            _ => Err(err)
                        }
                    }
                    else {Err(err)}
                },
                _ => Err(err)
            }
        }
    }
}
pub fn impl_convert<'ctx>(loc: Location, (mut val, vloc): (Value<'ctx>, Option<Location>), (target, tloc): (Type, Option<Location>), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let li = format!("source type is {}", val.data_type);
    let ri = format!("target type is {target}");
    let mut err = Diagnostic::error(loc.clone(), 311, Some(format!("cannot convert value of type {} to {target}", val.data_type)));
    if let Some(l) = vloc.clone() {err.add_note(l, li)} else {err.add_info(li)};
    if let Some(r) = tloc.clone() {err.add_note(r, ri)} else {err.add_info(ri)};
    if val.data_type == target {Ok(val)}
    else if target == Type::Null {Ok(Value::null())}
    else if target == Type::Error {Ok(Value::error())}
    else if if let Type::Reference(ref b, false) = target {**b == val.data_type} else {false} {Ok(Value::new(val.addr(ctx).map(From::from), None, target))}
    else {
        match val.data_type {
            Type::Borrow(b) => {
                val.data_type = *b;
                impl_convert(loc, (val, vloc), (target, tloc), ctx)
            },
            Type::Reference(b, true) => {
                if target == Type::Reference(b.clone(), false) {return Ok(Value::new(val.comp_val, val.inter_val, Type::Reference(b, false)));}
                match *b {
                    Type::Array(b, Some(l)) => match target {
                        Type::Pointer(b2, m) if b == b2 => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, m))),
                        Type::Reference(b2, m) => if let Type::Array(b2, None) = *b2 {
                            if b == b2 {
                                let at = Type::Reference(Box::new(Type::Array(b2, None)), m);
                                Ok(Value::new(
                                    if let Some(PointerValue(v)) = val.comp_val {
                                        let alloca = ctx.builder.build_alloca(at.llvm_type(ctx).unwrap(), "");
                                        ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 0, "").unwrap(), v);
                                        ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 1, "").unwrap(), ctx.context.i64_type().const_int(l as u64, false));
                                        Some(ctx.builder.build_load(alloca, ""))
                                    } else {None},
                                    val.inter_val,
                                    at
                                ))
                            }
                            else {Err(err)}
                        } else {Err(err)},
                        _ => Err(err)
                    },
                    Type::Array(b, None) => match target {
                        Type::Pointer(b2, m) if b == b2 => {
                            val.data_type = Type::Reference(Box::new(Type::Array(b.clone(), None)), true);
                            if let Some(StructValue(sv)) = val.value(ctx) {
                                val.comp_val = ctx.builder.build_extract_value(sv, 0, "");
                            }
                            Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, m)))
                        },
                        _ => Err(err)
                    },
                    b => {
                        if !ctx.is_const.get() && b.register() {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v, ""));
                                val.address.set(Some(v));
                            }
                        }
                        val.data_type = b;
                        impl_convert(loc, (val, vloc), (target, tloc), ctx)
                    }
                }
            },
            Type::Reference(b, false) => match *b {
                Type::Array(b, Some(l)) => match target {
                    Type::Pointer(b2, false) if b == b2 => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, false))),
                    Type::Reference(b2, false) => if let Type::Array(b2, None) = *b2 {
                        if b == b2 {
                            let at = Type::Reference(Box::new(Type::Array(b2, None)), false);
                            Ok(Value::new(
                                if let Some(PointerValue(v)) = val.comp_val {
                                    let alloca = ctx.builder.build_alloca(at.llvm_type(ctx).unwrap(), "");
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 0, "").unwrap(), v);
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 1, "").unwrap(), ctx.context.i64_type().const_int(l as u64, false));
                                    Some(ctx.builder.build_load(alloca, ""))
                                } else {None},
                                val.inter_val,
                                at
                            ))
                        }
                        else {Err(err)}
                    } else {Err(err)},
                    _ => Err(err)
                },
                Type::Array(b, None) => match target {
                    Type::Pointer(b2, false) if b == b2 => {
                        val.data_type = Type::Reference(Box::new(Type::Array(b.clone(), None)), false);
                        if let Some(StructValue(sv)) = val.value(ctx) {
                            val.comp_val = ctx.builder.build_extract_value(sv, 0, "");
                        }
                        Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, false)))
                    },
                    _ => Err(err)
                },
                b => {
                    if !ctx.is_const.get() && b.register() {
                        if let Some(PointerValue(v)) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(v, ""));
                            val.address.set(Some(v));
                        }
                    }
                    val.data_type = b;
                    impl_convert(loc, (val, vloc), (target, tloc), ctx)
                }
            },
            Type::IntLiteral => match target {
                x @ Type::Int(..) => Ok(Value::new(
                    if let Some(InterData::Int(v)) = val.inter_val {Some(IntValue(x.llvm_type(ctx).unwrap().into_int_type().const_int(v as u64, true)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(IntValue(ctx.builder.build_int_z_extend(v, x.llvm_type(ctx).unwrap().into_int_type(), "")))}
                              else {None},
                    val.inter_val,
                    x
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let Some(InterData::Int(v)) = val.inter_val {Some(FloatValue(x.llvm_type(ctx).unwrap().into_float_type().const_float(v as f64)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(FloatValue(ctx.builder.build_signed_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "")))}
                              else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                _ => Err(err)
            },
            Type::Int(ls, true) => match target {
                Type::Int(1, false) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_compare(NE, v, ctx.context.custom_width_int_type(ls as u32).const_zero(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(if v == 0 {0} else {1}))} else {None},
                    Type::Int(1, false)
                )),
                Type::Int(rs, true) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_z_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, true)
                )),
                Type::Int(rs, false) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_s_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, false)
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_unsigned_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                _ => Err(err)
            },
            Type::Int(ls, false) => match target {
                Type::Int(1, false) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_compare(NE, v, ctx.context.custom_width_int_type(ls as u32).const_zero(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(if v == 0 {0} else {1}))} else {None},
                    Type::Int(1, false)
                )),
                Type::Int(rs, ru) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_s_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, ru)
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_signed_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                _ => Err(err)
            },
            Type::Pointer(_, false) => match target {
                Type::Pointer(rb, false) if *rb == Type::Null => Ok(Value::new(
                    val.value(ctx).map(|v| ctx.builder.build_bitcast(v, ctx.null_type.ptr_type(Default::default()), "")),
                    None,
                    Type::Pointer(Box::new(Type::Null), false)
                )),
                _ => Err(err)
            },
            Type::Pointer(ref lb, true) => match target {
                Type::Pointer(rb, false) => match *rb {
                    Type::Null => Ok(Value::new(
                        val.value(ctx).map(|v| ctx.builder.build_bitcast(v, ctx.null_type.ptr_type(Default::default()), "")),
                        None,
                        Type::Pointer(Box::new(Type::Null), false)
                    )),
                    x if x == **lb => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(Box::new(x), false))),
                    _ => Err(err)
                },
                _ => Err(err)
            },
            Type::Tuple(v) => {
                if target == Type::TypeData {
                    if let Some(InterData::Array(a)) = val.inter_val {
                        let mut vec = Vec::with_capacity(v.len());
                        for t in a.into_iter().zip(v) {
                            if let (InterData::Type(t), Type::TypeData) = t {vec.push(*t);}
                            else {return Err(err);}
                        }
                        Ok(Value::make_type(Type::Tuple(vec)))
                    }
                    else {Err(err)}
                }
                else {Err(err)}
            },
            Type::Null => if target == Type::TypeData {Ok(Value::make_type(Type::Null))} else {Err(err)},
            Type::Error => Ok(Value::error()),
            _ => Err(err)
        }
    }
}
pub fn expl_convert<'ctx>(loc: Location, (mut val, vloc): (Value<'ctx>, Option<Location>), (target, tloc): (Type, Option<Location>), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let li = format!("source type is {}", val.data_type);
    let ri = format!("target type is {target}");
    let mut err = Diagnostic::error(loc.clone(), 312, Some(format!("cannot convert value of type {} to {target}", val.data_type)));
    if let Some(l) = vloc.clone() {err.add_note(l, li)} else {err.add_info(li)};
    if let Some(r) = tloc.clone() {err.add_note(r, ri)} else {err.add_info(ri)};
    if val.data_type == target {Ok(val)}
    else if target == Type::Null {Ok(Value::null())}
    else if target == Type::Error {Ok(Value::error())}
    else if if let Type::Reference(ref b, false) = target {**b == val.data_type} else {false} {Ok(Value::new(val.addr(ctx).map(From::from), None, target))}
    else {
        match val.data_type {
            Type::Borrow(b) => {
                val.data_type = *b;
                expl_convert(loc, (val, vloc), (target, tloc), ctx)
            },
            Type::Reference(b, true) => {
                if target == Type::Reference(b.clone(), false) {return Ok(Value::new(val.comp_val, val.inter_val, Type::Reference(b, false)));}
                match *b {
                    Type::Array(b, Some(l)) => match target {
                        Type::Pointer(b2, m) if b == b2 => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, m))),
                        Type::Reference(b2, m) => if let Type::Array(b2, None) = *b2 {
                        if b == b2 {
                            let at = Type::Reference(Box::new(Type::Array(b2, None)), m);
                            Ok(Value::new(
                                if let Some(PointerValue(v)) = val.comp_val {
                                    let alloca = ctx.builder.build_alloca(at.llvm_type(ctx).unwrap(), "");
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 0, "").unwrap(), v);
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 1, "").unwrap(), ctx.context.i64_type().const_int(l as u64, false));
                                    Some(ctx.builder.build_load(alloca, ""))
                                } else {None},
                                val.inter_val,
                                at
                            ))
                        }
                        else {Err(err)}
                    } else {Err(err)},
                        _ => Err(err)
                    },
                    Type::Array(b, None) => match target {
                        Type::Pointer(b2, m) if b == b2 => {
                            val.data_type = Type::Reference(Box::new(Type::Array(b.clone(), None)), true);
                            if let Some(StructValue(sv)) = val.value(ctx) {
                                val.comp_val = ctx.builder.build_extract_value(sv, 0, "");
                            }
                            Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, m)))
                        },
                        _ => Err(err)
                    },
                    b => {
                        if !ctx.is_const.get() && b.register() {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(v, ""));
                            }
                        }
                        val.data_type = b;
                        expl_convert(loc, (val, vloc), (target, tloc), ctx)
                    }
                }
            },
            Type::Reference(b, false) => match *b {
                Type::Array(b, Some(l)) => match target {
                    Type::Pointer(b2, false) if b == b2 => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, false))),
                    Type::Reference(b2, false) => if let Type::Array(b2, None) = *b2 {
                        if b == b2 {
                            let at = Type::Reference(Box::new(Type::Array(b2, None)), false);
                            Ok(Value::new(
                                if let Some(PointerValue(v)) = val.comp_val {
                                    let alloca = ctx.builder.build_alloca(at.llvm_type(ctx).unwrap(), "");
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 0, "").unwrap(), v);
                                    ctx.builder.build_store(ctx.builder.build_struct_gep(alloca, 1, "").unwrap(), ctx.context.i64_type().const_int(l as u64, false));
                                    Some(ctx.builder.build_load(alloca, ""))
                                } else {None},
                                val.inter_val,
                                at
                            ))
                        }
                        else {Err(err)}
                    } else {Err(err)},
                    _ => Err(err)
                },
                Type::Array(b, None) => match target {
                    Type::Pointer(b2, false) if b == b2 => {
                        val.data_type = Type::Reference(Box::new(Type::Array(b.clone(), None)), false);
                        if let Some(StructValue(sv)) = val.value(ctx) {
                            val.comp_val = ctx.builder.build_extract_value(sv, 0, "");
                        }
                        Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(b, false)))
                    },
                    _ => Err(err)
                },
                b => {
                    if !ctx.is_const.get() && b.register() {
                        if let Some(PointerValue(v)) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(v, ""));
                        }
                    }
                    val.data_type = b;
                    expl_convert(loc, (val, vloc), (target, tloc), ctx)
                }
            },
            Type::IntLiteral => match target {
                x @ Type::Int(..) => Ok(Value::new(
                    if let Some(InterData::Int(v)) = val.inter_val {Some(IntValue(x.llvm_type(ctx).unwrap().into_int_type().const_int(v as u64, true)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(IntValue(ctx.builder.build_int_z_extend(v, x.llvm_type(ctx).unwrap().into_int_type(), "")))}
                              else {None},
                    val.inter_val,
                    x
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let Some(InterData::Int(v)) = val.inter_val {Some(FloatValue(x.llvm_type(ctx).unwrap().into_float_type().const_float(v as f64)))}
                              else if let Some(IntValue(v)) = val.comp_val {Some(FloatValue(ctx.builder.build_signed_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "")))}
                              else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                _ => Err(err)
            },
            Type::Int(ls, true) => match target {
                Type::Int(rs, ru) if ls == rs => Ok(Value::new(val.comp_val, val.inter_val, Type::Int(rs, ru))),
                Type::Int(1, false) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_compare(NE, v, ctx.context.custom_width_int_type(ls as u32).const_zero(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(if v == 0 {0} else {1}))} else {None},
                    Type::Int(1, false)
                )),
                Type::Int(rs, true) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_z_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, true)
                )),
                Type::Int(rs, false) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_s_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, false)
                )),
                Type::Int(rs, ru) if ls > rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_truncate(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, ru)
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_unsigned_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                Type::Pointer(b, m) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_to_ptr(v, Type::Pointer(b.clone(), m).llvm_type(ctx).unwrap().into_pointer_type(), "").into())} else {None},
                    None,
                    Type::Pointer(b, m)
                )),
                _ => Err(err)
            },
            Type::Int(ls, false) => match target {
                Type::Int(rs, ru) if ls == rs => Ok(Value::new(val.comp_val, val.inter_val, Type::Int(rs, ru))),
                Type::Int(1, false) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_compare(NE, v, ctx.context.custom_width_int_type(ls as u32).const_zero(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(if v == 0 {0} else {1}))} else {None},
                    Type::Int(1, false)
                )),
                Type::Int(rs, ru) if ls < rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_s_extend(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, ru)
                )),
                Type::Int(rs, ru) if ls > rs => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_truncate(v, ctx.context.custom_width_int_type(rs as u32), "").into())} else {None},
                    val.inter_val,
                    Type::Int(rs, ru)
                )),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_signed_int_to_float(v, x.llvm_type(ctx).unwrap().into_float_type(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Float(v as f64))} else {None},
                    x
                )),
                Type::Pointer(b, m) => Ok(Value::new(
                    if let (Some(IntValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_int_to_ptr(v, Type::Pointer(b.clone(), m).llvm_type(ctx).unwrap().into_pointer_type(), "").into())} else {None},
                    None,
                    Type::Pointer(b, m)
                )),
                _ => Err(err)
            },
            ref x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match target {
                Type::Int(1, false) => Ok(Value::new(
                    if let (Some(FloatValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_float_compare(ONE, v, x.llvm_type(ctx).unwrap().into_float_type().const_zero(), "").into())} else {None},
                    if let Some(InterData::Int(v)) = val.inter_val {Some(InterData::Int(if v == 0 {0} else {1}))} else {None},
                    Type::Int(1, false)
                )),
                Type::Int(s, false) => Ok(Value::new(
                    if let (Some(FloatValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_float_to_signed_int(v, ctx.context.custom_width_int_type(s as u32), "").into())} else {None},
                    if let Some(InterData::Float(v)) = val.inter_val {Some(InterData::Int(v as i128))} else {None},
                    Type::Int(s, false)
                )),
                Type::Int(s, true) => Ok(Value::new(
                    if let (Some(FloatValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_float_to_unsigned_int(v, ctx.context.custom_width_int_type(s as u32), "").into())} else {None},
                    if let Some(InterData::Float(v)) = val.inter_val {Some(InterData::Int(v as i128))} else {None},
                    Type::Int(s, false)
                )),
                r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::new(
                    if let (Some(FloatValue(v)), false) = (val.value(ctx), ctx.is_const.get()) {Some(ctx.builder.build_float_cast(v, r.llvm_type(ctx).unwrap().into_float_type(), "").into())} else {None},
                    val.inter_val,
                    x.clone()
                )),
                _ => Err(err)
            },
            Type::Pointer(ref lb, false) => match target {
                Type::Pointer(rb, false) if *rb == Type::Null => Ok(Value::new(
                    val.value(ctx).map(|v| ctx.builder.build_bitcast(v, ctx.null_type.ptr_type(Default::default()), "")),
                    None,
                    Type::Pointer(Box::new(Type::Null), false)
                )),
                Type::Pointer(rb, false) if **lb == Type::Null && !matches!(*rb, Type::Array(_, None)) => {
                    let pt = Type::Pointer(rb, false);
                    Ok(Value::new(
                        val.value(ctx).and_then(|v| Some(ctx.builder.build_bitcast(v, pt.llvm_type(ctx)?, ""))),
                        None,
                        pt
                    ))
                },
                _ => Err(err)
            },
            Type::Pointer(ref lb, true) => match target {
                Type::Pointer(rb, m) if *rb == Type::Null => Ok(Value::new(
                    val.value(ctx).map(|v| ctx.builder.build_bitcast(v, ctx.null_type.ptr_type(Default::default()), "")),
                    None,
                    Type::Pointer(Box::new(Type::Null), m)
                )),
                Type::Pointer(rb, m) if **lb == Type::Null && !matches!(*rb, Type::Array(_, None)) => {
                    let pt = Type::Pointer(rb, m);
                    Ok(Value::new(
                        val.value(ctx).and_then(|v| Some(ctx.builder.build_bitcast(v, pt.llvm_type(ctx)?, ""))),
                        None,
                        pt
                    ))
                },
                Type::Pointer(x, false) if x == *lb => Ok(Value::new(val.comp_val, val.inter_val, Type::Pointer(x, false))),
                _ => Err(err)
            },
            Type::Tuple(v) => {
                if target == Type::TypeData {
                    if let Some(InterData::Array(a)) = val.inter_val {
                        let mut vec = Vec::with_capacity(v.len());
                        for t in a.into_iter().zip(v) {
                            if let (InterData::Type(t), Type::TypeData) = t {vec.push(*t);}
                            else {return Err(err);}
                        }
                        Ok(Value::make_type(Type::Tuple(vec)))
                    }
                    else {Err(err)}
                }
                else {Err(err)}
            },
            Type::Null => match target {
                Type::TypeData => Ok(Value::make_type(Type::Null)),
                x @ (Type::IntLiteral | Type::Int(..)) => Ok(Value::interpreted(x.llvm_type(ctx).unwrap().into_int_type().const_int(0, false).into(), InterData::Int(0), x)),
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => Ok(Value::interpreted(x.llvm_type(ctx).unwrap().into_float_type().const_float(0.0).into(), InterData::Float(0.0), x)),
                x @ Type::Pointer(..) => Ok(Value::compiled(x.llvm_type(ctx).unwrap().const_zero(), x)),
                _ => Err(err)
            },
            Type::Error => Ok(Value::error()),
            _ => Err(err)
        }
    }
}
pub fn attr<'ctx>((mut val, vloc): (Value<'ctx>, Location), (id, iloc): (&str, Location), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    let err = Diagnostic::error((vloc.0, vloc.1.start..iloc.1.end), 328, Some(format!("no attribute {id} on value of type {}", val.data_type))).note(vloc.clone(), format!("object type is {}", val.data_type)).note(iloc.clone(), format!("attribute is {id}"));
    match val.data_type {
        Type::Borrow(b) => {
            val.data_type = *b;
            attr((val, vloc), (id, iloc), ctx)
        },
        Type::Reference(b, _) => {
            val.data_type = *b;
            if val.data_type.register() {
                if let Some(PointerValue(v)) = val.value(ctx) {
                    val.comp_val = Some(ctx.builder.build_load(v, ""));
                }
            }
            attr((val, vloc), (id, iloc), ctx)
        },
        _ => Err(err)
    }
}
fn prep_asm<'ctx>(mut arg: Value<'ctx>, ctx: &CompCtx<'ctx>) -> Option<(BasicMetadataTypeEnum<'ctx>, BasicMetadataValueEnum<'ctx>)> {
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
pub fn call<'ctx>(mut target: Value<'ctx>, loc: Location, cparen: Location, mut args: Vec<(Value<'ctx>, Location)>, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, Diagnostic> {
    match target.data_type {
        Type::Error => Ok(Value::error()),
        Type::Borrow(b) => {
            match *b {
                Type::Tuple(v) => {
                    let err = Diagnostic::error(loc.clone(), 313, Some({
                        let mut out = "target type is (".to_string();
                        for t in &v {out += &format!("{t}, ");}
                        out.truncate(out.len() - 2);
                        out += ")^";
                        out
                    })).info({
                        let mut out = "argument types are (".to_string();
                        args.iter().for_each(|(Value {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
                        out.truncate(out.len() - 2);
                        out.push(')');
                        out
                    });
                    match args.as_slice() {
                        [(Value {data_type: Type::IntLiteral | Type::Int(..), inter_val, ..}, aloc)] => {
                            if let Some(InterData::Int(idx)) = inter_val {
                                let idx = *idx as usize;
                                if let Some(t) = v.get(idx) {
                                    Ok(Value::new(
                                        target.comp_val.and_then(|v| ctx.builder.build_extract_value(v.into_struct_value(), idx as u32, "")),
                                        if let Some(InterData::Array(v)) = target.inter_val {v.get(idx).cloned()} else {None},
                                        Type::Borrow(Box::new(t.clone()))
                                    ))
                                }
                                else {Err(Diagnostic::error(aloc.clone(), 381, Some(format!("index is {idx}"))).note(loc, format!("tuple length is {}", v.len())))}
                            }
                            else {Err(Diagnostic::error(aloc.clone(), 380, Some("argument is not const".to_string())))}
                        },
                        _ => Err(err)
                    }
                },
                b => {
                    target.data_type = b;
                    call(target, loc, cparen, args, ctx)
                }
            }
        },
        Type::Reference(b, m) => {
            match *b {
                Type::Tuple(v) => {
                    let err = Diagnostic::error(loc.clone(), 313, Some({
                        let mut out = "target type is (".to_string();
                        for t in &v {out += &format!("{t}, ");}
                        out.truncate(out.len() - 2);
                        out += ") ";
                        out += if m {"mut"} else {"const"};
                        out.push('&');
                        out
                    })).info({
                        let mut out = "argument types are (".to_string();
                        args.iter().for_each(|(Value {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
                        out.truncate(out.len() - 2);
                        out.push(')');
                        out
                    });
                    match args.as_slice() {
                        [(Value {data_type: Type::IntLiteral | Type::Int(..), inter_val, ..}, aloc)] => {
                            if let Some(InterData::Int(idx)) = inter_val {
                                let idx = *idx as usize;
                                if let Some(t) = v.get(idx) {
                                    Ok(Value::new(
                                        target.comp_val.and_then(|v| ctx.builder.build_struct_gep(v.into_pointer_value(), idx as u32, "").ok().map(From::from)),
                                        if let Some(InterData::Array(v)) = target.inter_val {v.get(idx).cloned()} else {None},
                                        Type::Reference(Box::new(t.clone()), m)
                                    ))
                                }
                                else {Err(Diagnostic::error(aloc.clone(), 381, Some(format!("index is {idx}"))).note(loc, format!("tuple length is {}", v.len())))}
                            }
                            else {Err(Diagnostic::error(aloc.clone(), 380, Some("argument is not const".to_string())))}
                        },
                        _ => Err(err)
                    }
                },
                b => {
                    if !ctx.is_const.get() && b.register() {
                        if let Some(PointerValue(v)) = target.comp_val {
                            target.comp_val = Some(ctx.builder.build_load(v, ""));
                        }
                    }
                    target.data_type = b;
                    call(target, loc, cparen, args, ctx)
                }
            }
        },
        Type::Function(ret, params) => {
            let mut err = Diagnostic::error(loc.clone(), 313, Some(format!("function type is {}", Type::Function(ret.clone(), params.clone())))).note(loc.clone(), {
                let mut out = "argument types are (".to_string();
                args.iter().for_each(|(Value {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
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
                defaults.iter().zip(params.iter().skip(p - d)).skip(a + d - p).map(|(v, (t, c))| (Value::new(
                    if *c {None} else {v.into_compiled(ctx)},
                    Some(v.clone()),
                    t.clone()
                ), cparen.clone())).collect()
            } else {vec![]}).zip(params.iter()).enumerate().map(|(n, ((v, l), (t, c)))| {
                let e = format!("expected value of type {t} in {}{} argument, got {}", n + 1, if  n % 100 / 10 == 1 {"th"} else {suffixes[n % 10]}, v.data_type);
                (if let Ok(val) = impl_convert((0, 0..0), (v.clone(), None), (t.clone(), None), ctx) {
                    if *c && val.inter_val.is_none() {
                        good = false;
                        err.add_note(l, format!("{}{} argument must be const, but argument is not", n + 1, if  n % 100 / 10 == 1 {"th"} else {suffixes[n % 10]}));
                    }
                    val
                }
                else {
                    good = false;
                    err.add_note(l, e);
                    Value::error()
                }, c)
            }).partition::<Vec<_>, _>(|(_, c)| **c);
            if !good {return Err(err)}
            if !c.is_empty() {return Err(Diagnostic::error(loc, 900, None))}
            good = true;
            let val: Option<inkwell::values::CallableValue> = if let Some(PointerValue(v)) = target.comp_val {v.try_into().ok()} else {None};
            let args: Vec<inkwell::values::BasicMetadataValueEnum> = r.into_iter().filter_map(|(Value {comp_val, ..}, _)| comp_val.map(|v| v.into()).or_else(|| {good = false; None})).collect();
            Ok(Value::new(
                val.and_then(|v| ctx.builder.build_call(v, args.as_slice(), "").try_as_basic_value().left()),
                None,
                *ret
            ))
        },
        Type::InlineAsm(r) => if let (Some(InterData::InlineAsm(c, b)), false) = (target.inter_val, ctx.is_const.get()) {
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
                Ok(Value::new(
                    ret.try_as_basic_value().left(),
                    None,
                    *r
                ))
            }
            else {
                let fty = ctx.context.void_type().fn_type(&params, false);
                let asm = ctx.context.create_inline_asm(fty, b, c, true, true, None, false);
                ctx.builder.build_call(CallableValue::try_from(asm).unwrap(), &comp_args, "");
                Ok(Value::null())
            }
        } else {Ok(Value::error())},
        Type::Tuple(v) => {
            let err = Diagnostic::error(loc.clone(), 313, Some({
                let mut out = "target type is (".to_string();
                for t in &v {out += &format!("{t}, ");}
                out.truncate(out.len() - 2);
                out.push(')');
                out
            })).info({
                let mut out = "argument types are (".to_string();
                args.iter().for_each(|(Value {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
                out.truncate(out.len() - 2);
                out.push(')');
                out
            });
            match args.as_slice() {
                [(Value {data_type: Type::IntLiteral | Type::Int(..), inter_val, ..}, aloc)] => {
                    if let Some(InterData::Int(idx)) = inter_val {
                        let idx = *idx as usize;
                        if let Some(t) = v.get(idx) {
                            Ok(Value::new(
                                target.comp_val.and_then(|v| ctx.builder.build_extract_value(v.into_struct_value(), idx as u32, "")),
                                if let Some(InterData::Array(v)) = target.inter_val {v.get(idx).cloned()} else {None},
                                t.clone()
                            ))
                        }
                        else {Err(Diagnostic::error(aloc.clone(), 381, Some(format!("index is {idx}"))).note(loc, format!("tuple length is {}", v.len())))}
                    }
                    else {Err(Diagnostic::error(aloc.clone(), 380, Some("argument is not const".to_string())))}
                },
                _ => Err(err)
            }
        },
        t => Err(Diagnostic::error(loc, 313, Some(format!("target type is {t}"))).info({
            let mut out = "argument types are (".to_string();
            args.iter().for_each(|(Value {data_type, ..}, _)| out += format!("{data_type}, ").as_str());
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
