use crate::*;
use std::cmp::{min, max, Ordering};
use std::collections::VecDeque;
use inkwell::values::{BasicValueEnum::{self, *}, BasicMetadataValueEnum, BasicValue};
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::StructType};
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
            (Type::Tuple(xv), Type::Tuple(yv)) if xv == yv && op == "=" => Type::Reference(Box::new(Type::Tuple(xv)), true),
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
pub fn attr_type<'ctx>(target: Type, attr: &str, ctx: &CompCtx<'ctx>) -> Type {
    match target {
        Type::Borrow(b) => attr_type(*b, attr, ctx),
        Type::Reference(b, m) =>
            if let Type::Nominal(ref n) = *b {
                ctx.nominals.borrow()[n].2.get(attr).map_or(Type::Error, |v| if let Value {data_type: Type::Function(ret, args), inter_val: Some(InterData::Function(FnData {mt, ..})), ..} = v {
                    match mt {
                        MethodType::Normal => Type::BoundMethod(Box::new(Type::Nominal(n.clone())), ret.clone(), args.clone(), m),
                        MethodType::Static => Type::Error,
                        MethodType::Getter => (**ret).clone()
                    }
                } else {Type::Error})
            } else {attr_type(*b, attr, ctx)},
        Type::Nominal(n) => ctx.nominals.borrow()[&n].2.get(attr).map_or(Type::Error, |v| if let Value {data_type: Type::Function(ret, args), inter_val: Some(InterData::Function(FnData {mt, ..})), ..} = v {
            match mt {
                MethodType::Normal => Type::BoundMethod(Box::new(Type::Nominal(n)), ret.clone(), args.clone(), false),
                MethodType::Static => Type::Error,
                MethodType::Getter => (**ret).clone()
            }
        } else {Type::Error}),
        _ => Type::Error
    }
}
pub fn bin_op<'ctx>(loc: SourceSpan, (mut lhs, lloc): (Value<'ctx>, SourceSpan), (mut rhs, rloc): (Value<'ctx>, SourceSpan), op: &str, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::BinOpNotDefined {
        lhs: lhs.data_type.to_string(),
        rhs: rhs.data_type.to_string(),
        op: op.to_string(),
        lloc, rloc, oloc: loc
    };
    match (lhs.data_type.clone(), rhs.data_type.clone()) {
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
        (Type::Reference(l, false), _r) => {
            lhs.data_type = *l;
            if !ctx.is_const.get() && lhs.data_type.register(ctx) {
                if let (Some(t), Some(PointerValue(v))) = (lhs.data_type.llvm_type(ctx), lhs.comp_val) {
                    lhs.comp_val = Some(ctx.builder.build_load(t, v, ""));
                }
                else {
                    lhs.comp_val = None;
                }
            }
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (_l, Type::Reference(r, _)) => {
            rhs.data_type = *r;
            if !ctx.is_const.get() && rhs.data_type.register(ctx) {
                if let (Some(t), Some(PointerValue(v))) = (rhs.data_type.llvm_type(ctx), rhs.comp_val) {
                    rhs.comp_val = Some(ctx.builder.build_load(t, v, ""));
                }
                else {
                    rhs.comp_val = None;
                }
            }
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Reference(l, true), r) => match (*l, r) {
            (Type::IntLiteral, _) => panic!("There shouldn't be a reference to an integer literal"),
            (l @ Type::Int(..), r @ Type::IntLiteral) => match op {
                "=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        ctx.builder.build_store(lv, rv);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "+=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_int_add(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "-=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_int_sub(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "*=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_int_mul(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "/=" => {
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = if unsigned {ctx.builder.build_int_unsigned_div(v1, rv, "")} else {ctx.builder.build_int_signed_div(v1, rv, "")};
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "%=" => {
                    let unsigned = if let Type::Int(s, true) = r {rhs.data_type = Type::Int(s, true); true}
                    else {rhs.data_type = r; false};
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = if unsigned {ctx.builder.build_int_unsigned_rem(v1, rv, "")} else {ctx.builder.build_int_signed_rem(v1, rv, "")};
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "&=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_and(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "|=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_or(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "^=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_xor(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                "<<=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_left_shift(v1, rv, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                ">>=" => {
                    if let (Some(PointerValue(lv)), Some(InterData::Int(r)), false) = (lhs.value(ctx), rhs.inter_val, ctx.is_const.get()) {
                        let rv = l.llvm_type(ctx).unwrap().into_int_type().const_int(r as u64, true);
                        let v1 = ctx.builder.build_load(l.llvm_type(ctx).unwrap(), lv, "").into_int_value();
                        let v2 = ctx.builder.build_right_shift(v1, rv, false, "");
                        ctx.builder.build_store(lv, v2);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = l;
                    if !ctx.is_const.get() && lhs.data_type.register(ctx) {
                        if let Some(PointerValue(v)) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(lhs.data_type.llvm_type(ctx).unwrap(), v, ""));
                        }
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            },
            (Type::Int(ls, lu), Type::Int(rs, ru)) => {
                if op == "=" {
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        ctx.builder.build_store(l, r);
                    }
                    lhs.inter_val = None;
                    return Ok(lhs);
                }
                match ls.cmp(&rs) {
                    Ordering::Less => return Err(err),
                    Ordering::Greater => if let Some(IntValue(rv)) = rhs.comp_val {
                        let lt = ctx.context.custom_width_int_type(ls as u32);
                        rhs.comp_val = Some(if ru {ctx.builder.build_int_z_extend(rv, lt, "")} else {ctx.builder.build_int_s_extend(rv, lt, "")}.into());
                    },
                    Ordering::Equal => {}
                }
                match op {
                    "+=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "-=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "*=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_int_mul(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "/=" => {
                        if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = if ru {ctx.builder.build_int_unsigned_div(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_div(v1, r.into_int_value(), "")};
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "%=" => {
                        if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = if ru {ctx.builder.build_int_unsigned_rem(v1, r.into_int_value(), "")} else {ctx.builder.build_int_signed_rem(v1, r.into_int_value(), "")};
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "&=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_and(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "|=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_or(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "^=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_xor(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    "<<=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_left_shift(v1, r.into_int_value(), "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    ">>=" => {
                        if let (Some(PointerValue(l)), Some(r), false) =  (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                            let v1 = ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), l, "").into_int_value();
                            let v2 = ctx.builder.build_right_shift(v1, r.into_int_value(), false, "");
                            ctx.builder.build_store(l, v2);
                        }
                        lhs.inter_val = None;
                        Ok(lhs)
                    },
                    _ => {
                        lhs.data_type = Type::Int(ls, lu);
                        if !ctx.is_const.get() && lhs.data_type.register(ctx) {
                            if let Some(PointerValue(v)) = lhs.comp_val {
                                lhs.comp_val = Some(ctx.builder.build_load(ctx.context.custom_width_int_type(ls as u32), v, ""));
                            }
                        }
                        bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                    }
                }
            },
            (x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::IntLiteral | Type::Int(..))) => match op {
                "=" => {
                    lhs.inter_val = None;
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
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(x.llvm_type(ctx).unwrap(), l, "").into_float_value();
                        let v3 = ctx.builder.build_float_add(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "-=" => {
                    lhs.inter_val = None;
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(x.llvm_type(ctx).unwrap(), l, "").into_float_value();
                        let v3 = ctx.builder.build_float_sub(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "*=" => {
                    lhs.inter_val = None;
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(x.llvm_type(ctx).unwrap(), l, "").into_float_value();
                        let v3 = ctx.builder.build_float_mul(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "/=" => {
                    lhs.inter_val = None;
                    if let (Some(PointerValue(l)), Some(IntValue(rv)), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        let v1 = match r {
                            Type::IntLiteral | Type::Int(_, false) => ctx.builder.build_signed_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), ""),
                            _ => ctx.builder.build_unsigned_int_to_float(rv, x.llvm_type(ctx).unwrap().into_float_type(), "")
                        };
                        let v2 = ctx.builder.build_load(x.llvm_type(ctx).unwrap(), l, "").into_float_value();
                        let v3 = ctx.builder.build_float_div(v1, v2, "");
                        ctx.builder.build_store(l, v3);
                    }
                    Ok(lhs)
                },
                "%=" => Err(err), // TODO: implement fmod
                _ => {
                    lhs.data_type = x;
                    if let Some(v) = lhs.comp_val {
                        lhs.comp_val = Some(ctx.builder.build_load(lhs.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            },
            (x @ Type::Pointer(..), y) if x == y => match op {
                "=" => {
                    if let (Some(PointerValue(l)), Some(r), false) = (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
                        ctx.builder.build_store(l, r);
                    }
                    lhs.inter_val = None;
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = x;
                    if let Some(v) = lhs.comp_val {
                        lhs.comp_val = Some(ctx.builder.build_load(lhs.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            }
            (Type::Pointer(b, m), Type::IntLiteral | Type::Int(..)) => match op {
                "+=" => {
                    lhs.inter_val = None;
                    if let (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) = (lhs.comp_val, rhs.comp_val, b.size(ctx), ctx.is_const.get()) {
                        unsafe {
                            let lv = ctx.builder.build_load(ctx.null_type.ptr_type(Default::default()), l, "").into_pointer_value();
                            let v = ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), lv, &[r], "");
                            ctx.builder.build_store(l, v);
                        }
                    }
                    Ok(lhs)
                },
                "-=" => {
                    lhs.inter_val = None;
                    if let (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) = (lhs.comp_val, rhs.comp_val, b.size(ctx), ctx.is_const.get()) {
                        unsafe {
                            let lv = ctx.builder.build_load(ctx.null_type.ptr_type(Default::default()), l, "").into_pointer_value();
                            let rv = ctx.builder.build_int_neg(r, "");
                            let v = ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), lv, &[rv], "");
                            ctx.builder.build_store(l, v);
                        }
                    }
                    Ok(lhs)
                },
                _ => {
                    lhs.data_type = Type::Pointer(b, m);
                    if !ctx.is_const.get() && lhs.data_type.register(ctx) {
                        if let Some(v) = lhs.comp_val {
                            lhs.comp_val = Some(ctx.builder.build_load(ctx.null_type.ptr_type(Default::default()), v.into_pointer_value(), ""));
                        }
                    }
                    bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
                }
            },
            (Type::Tuple(xv), Type::Tuple(yv)) if xv == yv && op == "=" => {
                if let (Some(PointerValue(lv)), Some(rv)) = (lhs.value(ctx), rhs.value(ctx)) {
                    if !ctx.is_const.get() {
                        ctx.builder.build_store(lv, rv);
                    }
                }
                lhs.inter_val = None;
                Ok(lhs)
            },
            (l, _) => {
                lhs.data_type = l;
                if !ctx.is_const.get() && lhs.data_type.register(ctx) {
                    if let Some(PointerValue(v)) = lhs.comp_val {
                        lhs.comp_val = Some(ctx.builder.build_load(lhs.data_type.llvm_type(ctx).unwrap(), v, ""));
                    }
                    else {
                        rhs.comp_val = None;
                    }
                }
                bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
            }
        },
        (Type::Int(ls, _), Type::Int(rs, ru)) if ls > rs => {
            if let (Some(IntValue(val)), false) = (rhs.value(ctx), ctx.is_const.get()) {
                rhs.comp_val = Some(IntValue(if ru {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(ls as u32), "")}));
            }
            rhs.data_type = Type::Int(ls, ru);
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, _)) if ls < rs => {
            if let (Some(IntValue(val)), false) = (lhs.value(ctx), ctx.is_const.get()) {
                lhs.comp_val = Some(IntValue(if lu {ctx.builder.build_int_z_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}
                else {ctx.builder.build_int_s_extend(val, ctx.context.custom_width_int_type(rs as u32), "")}));
            }
            lhs.data_type = Type::Int(rs, lu);
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (Type::Int(ls, lu), Type::Int(rs, ru)) if ls == rs => match op {
            "+" => Ok(Value::new(
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
        (x @ Type::Int(..), Type::IntLiteral) => bin_op(loc, (Value {data_type: x.clone(), ..lhs}, lloc), (impl_convert(unreachable_span(), (Value {data_type: Type::IntLiteral, ..rhs}, None), (x, None), ctx).unwrap(), rloc), op, ctx),
        (Type::IntLiteral, x @ Type::Int(..)) => {
            let t = x.clone();
            lhs.data_type = Type::IntLiteral;
            bin_op(loc, (impl_convert(unreachable_span(), (lhs, None), (x, None), ctx).unwrap(), lloc), (Value {data_type: t, ..rhs}, rloc), op, ctx)
        },
        (Type::IntLiteral, Type::IntLiteral) => match op {
            "+" => Ok(Value::new(
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.comp_val, rhs.comp_val, b.size(ctx), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(ctx.null_type.ptr_type(Default::default()), l, &[r], "").into()}),
                    _ => None
                },
                None,
                Type::Pointer(b, s)
            )),
            "-" => Ok(Value::new(
                match (lhs.comp_val, rhs.comp_val, b.size(ctx), ctx.is_const.get()) {
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {
                        let v = ctx.builder.build_int_neg(r, "");
                        ctx.builder.build_gep(ctx.null_type.ptr_type(Default::default()), l, &[v], "").into()
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
                match (rhs.comp_val, lhs.comp_val, b.size(ctx), ctx.is_const.get()) { // I just swapped the sides here
                    (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(ctx.null_type.ptr_type(Default::default()), l, &[r], "").into()}),
                    _ => None
                },
                None,
                Type::Pointer(b, s)
            )),
            _ => Err(err)
        },
        (l @ Type::Pointer(..), r @ Type::Pointer(..)) => match op {
            "-" if l == r => Ok(Value::new(
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
        (l @ (Type::Float16 | Type::Float32 | Type::Float64), r @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128)) if l.size(ctx) < r.size(ctx) => {
            lhs.comp_val = match (lhs.comp_val, ctx.is_const.get()) {
                (Some(FloatValue(l)), false) => Some(FloatValue(ctx.builder.build_float_cast(l, r.llvm_type(ctx).unwrap().into_float_type(), ""))),
                _ => None
            };
            lhs.data_type = r.clone();
            rhs.data_type = r;
            bin_op(loc, (lhs, lloc), (rhs, rloc), op, ctx)
        },
        (l @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128), r @ (Type::Float16 | Type::Float32 | Type::Float64)) if l.size(ctx) > r.size(ctx) => {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
                match (lhs.value(ctx), rhs.value(ctx), ctx.is_const.get()) {
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
pub fn pre_op<'ctx>(loc: SourceSpan, (mut val, vloc): (Value<'ctx>, SourceSpan), op: &str, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::PreOpNotDefined {
        val: val.data_type.to_string(),
        op: op.to_string(),
        vloc, oloc: loc
    };
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
            if !ctx.is_const.get() && val.data_type.register(ctx) {
                if let Some(v) = val.comp_val {
                    val.comp_val = Some(ctx.builder.build_load(val.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
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
                            let it = x.llvm_type(ctx).unwrap().into_int_type();
                            let v1 = ctx.builder.build_load(it, v, "").into_int_value();
                            let v2 = ctx.builder.build_int_add(v1, it.const_int(1, false), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let it = x.llvm_type(ctx).unwrap().into_int_type();
                            let v1 = ctx.builder.build_load(it, v, "").into_int_value();
                            let v2 = ctx.builder.build_int_sub(v1, it.const_int(1, false), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register(ctx) {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(val.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
                    }
                },
                x @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let ft = x.llvm_type(ctx).unwrap().into_float_type();
                            let v1 = ctx.builder.build_load(ft, v, "").into_float_value();
                            let v2 = ctx.builder.build_float_add(v1, ft.const_float(1.0), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), false) = (val.comp_val, ctx.is_const.get()) {
                            let ft = x.llvm_type(ctx).unwrap().into_float_type();
                            let v1 = ctx.builder.build_load(ft, v, "").into_float_value();
                            let v2 = ctx.builder.build_float_sub(v1, ft.const_float(1.0), "");
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = x;
                        Ok(val)
                    },
                    _ => {
                        val.data_type = x;
                        if !ctx.is_const.get() && val.data_type.register(ctx) {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(val.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
                    }
                },
                Type::Pointer(b, m) => match op {
                    "++" => {
                        if let (Some(PointerValue(v)), SizeType::Static(_), false) = (val.comp_val, b.size(ctx), ctx.is_const.get()) {
                            let pt = ctx.null_type.ptr_type(Default::default());
                            let v1 = ctx.builder.build_load(pt, v, "").into_pointer_value();
                            let v2 = unsafe {ctx.builder.build_gep(pt, v1, &[ctx.context.i8_type().const_int(1, true)], "")};
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Ok(val)
                    },
                    "--" => {
                        if let (Some(PointerValue(v)), SizeType::Static(_), false) = (val.comp_val, b.size(ctx), ctx.is_const.get()) {
                            let pt = ctx.null_type.ptr_type(Default::default());
                            let v1 = ctx.builder.build_load(pt, v, "").into_pointer_value();
                            let v2 = unsafe {ctx.builder.build_gep(pt, v1, &[ctx.context.i8_type().const_int(u64::MAX, true)], "")};
                            ctx.builder.build_store(v, v2);
                        }
                        val.inter_val = None;
                        val.data_type = Type::Pointer(b, m);
                        Ok(val)
                    },
                    _ => {
                        val.data_type = Type::Pointer(b, m);
                        if !ctx.is_const.get() && val.data_type.register(ctx) {
                            if let Some(v) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(ctx.null_type.ptr_type(Default::default()), v.into_pointer_value(), ""));
                            }
                        }
                        pre_op(loc, (val, vloc), op, ctx)
                    }
                },
                x => {
                    val.data_type = x;
                    if !ctx.is_const.get() && val.data_type.register(ctx) {
                        if let Some(v) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(val.data_type.llvm_type(ctx).unwrap(), v.into_pointer_value(), ""));
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
pub fn post_op<'ctx>(loc: SourceSpan, (val, vloc): (Value<'ctx>, SourceSpan), op: &str, _ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::PostOpNotDefined {
        val: val.data_type.to_string(),
        op: op.to_string(),
        vloc, oloc: loc
    };
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
pub fn subscript<'ctx>((mut val, vloc): (Value<'ctx>, SourceSpan), (mut idx, iloc): (Value<'ctx>, SourceSpan), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::SubscriptNotDefined {
        val: val.data_type.to_string(),
        sub: idx.data_type.to_string(),
        vloc, sloc: iloc
    };
    match idx.data_type {
        Type::Borrow(x) => {
            idx.data_type = *x;
            subscript((val, vloc), (idx, iloc), ctx)
        },
        Type::Reference(x, _) => {
            if x.register(ctx) && !ctx.is_const.get() {
                if let Some(PointerValue(v)) = idx.comp_val {
                    idx.comp_val = Some(ctx.builder.build_load(x.llvm_type(ctx).unwrap(), v, ""));
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
                        match (val.comp_val, idx.value(ctx), b.size(ctx), ctx.is_const.get()) {
                            (Some(PointerValue(l)), Some(IntValue(r)), SizeType::Static(_), false) => Some(unsafe {ctx.builder.build_gep(ctx.null_type.ptr_type(Default::default()), l, &[r], "").into()}),
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
                            if let (Some(StructValue(sv)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(ctx), ctx.is_const.get()) {
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
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], ""))};
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
                                else {Some(unsafe {ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        Type::Int(_, false) => Ok(Value::new(
                            if let (Some(StructValue(sv)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(ctx), ctx.is_const.get()) {
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
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], ""))};
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
                                else {Some(unsafe {ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        _ => Err(err)
                    },
                    Type::Array(b, Some(s)) => match idx.data_type.clone() {
                        Type::IntLiteral | Type::Int(_, true) => Ok(Value::new(
                            if let (Some(PointerValue(raw)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(ctx), ctx.is_const.get()) {
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
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], ""))};
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
                                else {Some(unsafe {ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        Type::Int(_, false) => Ok(Value::new(
                            if let (Some(PointerValue(raw)), Some(IntValue(iv)), SizeType::Static(_), false) = (val.value(ctx), idx.value(ctx), b.size(ctx), ctx.is_const.get()) {
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
                                    let val = unsafe {PointerValue(ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], ""))};
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
                                else {Some(unsafe {ctx.builder.build_gep(b.llvm_type(ctx).unwrap(), raw, &[iv], "").into()})}
                            } else {None},
                            if let (Some(InterData::Array(vals)), Some(InterData::Int(val))) = (val.inter_val, idx.inter_val) {vals.get(val as usize).cloned()} else {None},
                            Type::Reference(b, m)
                        )),
                        _ => Err(err)
                    },
                    x => {
                        if !ctx.is_const.get() || x.register(ctx) {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(x.llvm_type(ctx).unwrap(), v, ""));
                            }
                        }
                        val.data_type = x;
                        subscript((val, vloc), (idx, iloc), ctx)
                    }
                },
                Type::TypeData => match idx.data_type {
                    Type::Null => if let Some(InterData::Type(t)) = val.inter_val {Ok(Value::make_type(Type::Array(t, None)))} else {unreachable!()},
                    Type::Int(..) | Type::IntLiteral => if let (Some(InterData::Type(t)), Some(InterData::Int(v))) = (val.inter_val, idx.inter_val) {Ok(Value::make_type(Type::Array(t, Some(v as u32))))} else {Err(CobaltError::NotCompileTime {loc: iloc})},
                    _ => Err(err)
                },
                Type::Null => match idx.data_type {
                    Type::Null => Ok(Value::make_type(Type::Array(Box::new(Type::Null), None))),
                    Type::Int(..) | Type::IntLiteral => if let Some(InterData::Int(v)) = idx.inter_val {Ok(Value::make_type(Type::Array(Box::new(Type::Null), Some(v as u32))))} else {Err(CobaltError::NotCompileTime {loc: iloc})},
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
                            Type::Int(..) | Type::IntLiteral => if let Some(InterData::Int(v)) = idx.inter_val {Ok(Value::make_type(Type::Array(Box::new(Type::Tuple(vec)), Some(v as u32))))} else {Err(CobaltError::NotCompileTime {loc: iloc})},
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
pub fn impl_convert<'ctx>(loc: SourceSpan, (mut val, vloc): (Value<'ctx>, Option<SourceSpan>), (target, tloc): (Type, Option<SourceSpan>), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::InvalidConversion {
        is_expl: false,
        val: val.data_type.to_string(),
        ty: target.to_string(),
        vloc, tloc,
        oloc: loc
    };
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
                                        let llt = at.llvm_type(ctx).unwrap().into_struct_type();
                                        let v1 = ctx.builder.build_insert_value(llt.const_zero(), v, 0, "").unwrap();
                                        let v2 = ctx.builder.build_insert_value(v1, ctx.context.i64_type().const_int(l as u64, false), 1, "").unwrap();
                                        Some(v2.into_struct_value().into())
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
                        if !ctx.is_const.get() && b.register(ctx) {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
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
                                    let llt = at.llvm_type(ctx).unwrap().into_struct_type();
                                    let v1 = ctx.builder.build_insert_value(llt.const_zero(), v, 0, "").unwrap();
                                    let v2 = ctx.builder.build_insert_value(v1, ctx.context.i64_type().const_int(l as u64, false), 1, "").unwrap();
                                    Some(v2.into_struct_value().into())
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
                    if !ctx.is_const.get() && b.register(ctx) {
                        if let Some(PointerValue(v)) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
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
pub fn expl_convert<'ctx>(loc: SourceSpan, (mut val, vloc): (Value<'ctx>, Option<SourceSpan>), (target, tloc): (Type, Option<SourceSpan>), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::InvalidConversion {
        is_expl: true,
        val: val.data_type.to_string(),
        ty: target.to_string(),
        vloc, tloc,
        oloc: loc
    };
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
                                    let llt = at.llvm_type(ctx).unwrap().into_struct_type();
                                    let v1 = ctx.builder.build_insert_value(llt.const_zero(), v, 0, "").unwrap();
                                    let v2 = ctx.builder.build_insert_value(v1, ctx.context.i64_type().const_int(l as u64, false), 1, "").unwrap();
                                    Some(v2.into_struct_value().into())
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
                        if !ctx.is_const.get() && b.register(ctx) {
                            if let Some(PointerValue(v)) = val.comp_val {
                                val.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
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
                                    let llt = at.llvm_type(ctx).unwrap().into_struct_type();
                                    let v1 = ctx.builder.build_insert_value(llt.const_zero(), v, 0, "").unwrap();
                                    let v2 = ctx.builder.build_insert_value(v1, ctx.context.i64_type().const_int(l as u64, false), 1, "").unwrap();
                                    Some(v2.into_struct_value().into())
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
                    if !ctx.is_const.get() && b.register(ctx) {
                        if let Some(PointerValue(v)) = val.comp_val {
                            val.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
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
pub fn attr<'ctx>((mut val, vloc): (Value<'ctx>, SourceSpan), (id, iloc): (&str, SourceSpan), ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    let err = CobaltError::AttrNotDefined {
        val: val.data_type.to_string(),
        attr: id.to_string(),
        vloc, aloc: iloc
    };
    match val.data_type.clone() {
        Type::Borrow(b) => {
            val.data_type = *b;
            attr((val, vloc), (id, iloc), ctx)
        },
        Type::Reference(ref b, m) => {
            if let Type::Nominal(ref n) = **b {
                ctx.nominals.borrow()[n].2.get(id).ok_or_else(|| err.clone()).and_then(|v| if let Value {data_type: Type::Function(ret, args), inter_val: Some(iv @ InterData::Function(FnData {mt, ..})), comp_val, ..} = &v {
                    match mt {
                        MethodType::Normal => {
                            let bm = Type::BoundMethod(Box::new(Type::Nominal(n.clone())), ret.clone(), args.clone(), m);
                            let mut v = Value::metaval(iv.clone(), bm);
                            if let (Some(vv), Some(f), Some(StructType(llt))) = (val.value(ctx), comp_val, v.data_type.llvm_type(ctx)) {
                                let v1 = ctx.builder.build_insert_value(llt.const_zero(), vv, 0, "").unwrap().into_struct_value();
                                let v2 = ctx.builder.build_insert_value(v1, *f, 1, "").unwrap().into_struct_value();
                                v.comp_val = Some(v2.into());
                            }
                            Ok(v)
                        },
                        MethodType::Static => Err(err),
                        MethodType::Getter => types::utils::call(Value::new(comp_val.clone(), Some(iv.clone()), Type::Function(ret.clone(), args.clone())), iloc, None, vec![(Value {data_type: Type::Reference(b.clone(), m), ..val.clone()}, vloc)], ctx)
                    }
                } else {Err(err)})
            }
            else {
                val.data_type = (**b).clone();
                if val.data_type.register(ctx) {
                    if let Some(PointerValue(v)) = val.value(ctx) {
                        val.comp_val = Some(ctx.builder.build_load(val.data_type.llvm_type(ctx).unwrap(), v, ""));
                    }
                }
                attr((val, vloc), (id, iloc), ctx)
            }
        },
        Type::Nominal(n) => {
            ctx.nominals.borrow()[&n].2.get(id).ok_or_else(|| err.clone()).and_then(|v| if let Value {data_type: Type::Function(ret, args), inter_val: Some(iv @ InterData::Function(FnData {mt, ..})), comp_val, ..} = v {
                match mt {
                    MethodType::Normal => {
                        let bm = Type::BoundMethod(Box::new(Type::Nominal(n.clone())), ret.clone(), args.clone(), false);
                        let mut v = Value::metaval(iv.clone(), bm);
                        if let (Some(vv), Some(f), Some(StructType(llt))) = (val.addr(ctx), comp_val, v.data_type.llvm_type(ctx)) {
                            let v0 = llt.get_undef();
                            let v1 = ctx.builder.build_insert_value(v0, vv, 0, "").unwrap();
                            let v2 = ctx.builder.build_insert_value(v1, *f, 1, "").unwrap();
                            v.comp_val = Some(v2.as_basic_value_enum());
                        }
                        Ok(v)
                    },
                    MethodType::Static => Err(err),
                    MethodType::Getter => {
                        val.comp_val = val.addr(ctx).map(From::from);
                        val.data_type = Type::Reference(Box::new(val.data_type.clone()), false);
                        types::utils::call(Value::new(comp_val.clone(), Some(iv.clone()), Type::Function(ret.clone(), args.clone())), iloc, None, vec![(val.clone(), vloc)], ctx)
                    }
                }
            } else {Err(err)})
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
            if let (Some(PointerValue(val)), true) = (arg.value(ctx), arg.data_type.register(ctx)) {
                arg.comp_val = Some(ctx.builder.build_load(arg.data_type.llvm_type(ctx).unwrap(), val, ""));
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
        t @ (Type::Float16 | Type::Float32 | Type::Float64 | Type::Float128 | Type::Pointer(..)) => Some((t.llvm_type(ctx).unwrap().into(), arg.comp_val.or_else(|| arg.inter_val.and_then(|x| t.into_compiled(&x, ctx)))?.into())),
        _ => None
    }
}
pub fn call<'ctx>(mut target: Value<'ctx>, loc: SourceSpan, cparen: Option<SourceSpan>, mut args: Vec<(Value<'ctx>, SourceSpan)>, ctx: &CompCtx<'ctx>) -> Result<Value<'ctx>, CobaltError> {
    match target.data_type {
        Type::Error => Ok(Value::error()),
        Type::Borrow(b) => {
            match *b {
                Type::Tuple(v) => {
                    let err = CobaltError::CannotCallWithArgs {
                        val: format!("({})^", v.iter().map(Type::to_string).collect::<Vec<_>>().join(", ")),
                        loc: cparen.map_or(loc, |cp| merge_spans(loc, cp)),
                        args: args.iter().map(|(v, _)| v.data_type.to_string()).collect(),
                        aloc: args.last().map(|(_, l)| merge_spans(args[0].1, *l)),
                        nargs: vec![]
                    };
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
                                else {Err(CobaltError::TupleIdxOutOfBounds {
                                    idx, len: v.len(),
                                    tloc: loc, iloc: *aloc
                                })}
                            }
                            else {Err(CobaltError::NotCompileTime {loc: *aloc})}
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
                    let err = CobaltError::CannotCallWithArgs {
                        val: format!("({}) {}&", v.iter().map(Type::to_string).collect::<Vec<_>>().join(", "), if m {"mut"} else {"const"}),
                        loc: cparen.map_or(loc, |cp| merge_spans(loc, cp)),
                        args: args.iter().map(|(v, _)| v.data_type.to_string()).collect(),
                        aloc: args.last().map(|(_, l)| merge_spans(args[0].1, *l)),
                        nargs: vec![]
                    };
                    match args.as_slice() {
                        [(Value {data_type: Type::IntLiteral | Type::Int(..), inter_val, ..}, aloc)] => {
                            if let Some(InterData::Int(idx)) = inter_val {
                                let idx = *idx as usize;
                                if let Some(t) = v.get(idx) {
                                    Ok(Value::new(
                                        target.comp_val.and_then(|llv| ctx.builder.build_struct_gep(types::tuple_type(&v, ctx)?, llv.into_pointer_value(), idx as u32, "").ok().map(From::from)),
                                        if let Some(InterData::Array(v)) = target.inter_val {v.get(idx).cloned()} else {None},
                                        Type::Reference(Box::new(t.clone()), m)
                                    ))
                                }
                                else {Err(CobaltError::TupleIdxOutOfBounds {
                                    idx, len: v.len(),
                                    tloc: loc, iloc: *aloc
                                })}
                            }
                            else {Err(CobaltError::NotCompileTime {loc: *aloc})}
                        },
                        _ => Err(err)
                    }
                },
                b => {
                    if !ctx.is_const.get() && b.register(ctx) {
                        if let Some(PointerValue(v)) = target.comp_val {
                            target.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
                        }
                    }
                    target.data_type = b;
                    call(target, loc, cparen, args, ctx)
                }
            }
        },
        Type::Function(ret, params) => {
            let mut err = CobaltError::CannotCallWithArgs {
                val: format!("fn ({}): {ret}", params.iter().map(|(t, _)| t.to_string()).collect::<Vec<_>>().join(", ")),
                loc: cparen.map_or(loc, |cp| merge_spans(loc, cp)),
                args: args.iter().map(|(v, _)| v.data_type.to_string()).collect(),
                aloc: args.last().map(|(_, l)| merge_spans(args[0].1, *l)),
                nargs: vec![]
            };
            let mut push_arg = |arg: ArgError| {if let CobaltError::CannotCallWithArgs {nargs, ..} = &mut err {nargs.push(arg)}};
            let mut good = true;
            let p = params.len();
            let mut a = args.len();
            let defaults = if let Some(InterData::Function(FnData {defaults, ..})) = target.inter_val {defaults} else {vec![]};
            if a > p {
                push_arg(ArgError::WrongNumArgs {found: a, expected: p, loc: args[p].1});
                a = p;
            }
            if a < p {
                let d = defaults.len();
                args.extend(&mut defaults.into_iter().enumerate().skip(p - a).map(|(n, v)| {
                    let (pty, pc) = &params[p - d + n];
                    (Value::new(pc.then(|| pty.into_compiled(&v, ctx)).flatten(), Some(v), pty.clone()), cparen.unwrap_or(loc))
                }));
                a = args.len();
            }
            if a < p {
                push_arg(ArgError::WrongNumArgs {found: a, expected: p, loc: cparen.unwrap_or(loc)});
                args.resize_with(p, || (Value::error(), cparen.unwrap_or(loc)));
            }
            let (c, r) = args.iter().zip(params.iter()).enumerate().map(|(n, ((v, l), (t, c)))| {
                (if let Ok(val) = impl_convert(unreachable_span(), (v.clone(), None), (t.clone(), None), ctx) {
                    if *c && val.inter_val.is_none() {
                        good = false;
                        push_arg(ArgError::ArgMustBeConst {n, loc: *l})
                    }
                    val
                }
                else {
                    good = false;
                    push_arg(ArgError::InvalidArg {n, val: v.data_type.to_string(), ty: t.to_string(), loc: *l});
                    Value::error()
                }, c)
            }).partition::<Vec<_>, _>(|(_, c)| **c);
            if !good {return Err(err)}
            if !c.is_empty() {return Err(CobaltError::ConstFnsArentSupported {loc})}
            good = true;
            let val: Option<inkwell::values::PointerValue> = target.comp_val.and_then(|v| v.try_into().ok());
            let args: Vec<BasicMetadataValueEnum> = r.into_iter().filter_map(|(Value {comp_val, ..}, _)| comp_val.map(|v| v.into()).or_else(|| {good = false; None})).collect();
            let aty: Vec<BasicMetadataTypeEnum> = args.iter().map(|v| BasicValueEnum::try_from(*v).unwrap().get_type().into()).collect();
            let fty = if ret.size(ctx) == SizeType::Static(0) {Some(ctx.context.void_type().fn_type(&aty, false))} else {ret.llvm_type(ctx).map(|t| t.fn_type(&aty, false))};
            Ok(Value::new(
                val.and_then(|v| ctx.builder.build_indirect_call(fty?, v, args.as_slice(), "").try_as_basic_value().left()),
                None,
                *ret
            ))
        },
        Type::BoundMethod(base, ret, params, m) => {
            let mut avec = Vec::with_capacity(args.len() + 1);
            avec.push((Value::new(None, None, Type::Reference(base, m)), loc));
            avec.append(&mut args);
            if let Some(StructValue(sv)) = target.comp_val {
                let tv = ctx.builder.build_extract_value(sv, 0, "").unwrap();
                let fv = ctx.builder.build_extract_value(sv, 1, "").unwrap();
                avec[0].0.comp_val = Some(tv);
                target.comp_val = Some(fv);
            }
            target.data_type = Type::Function(ret, params);
            call(target, loc, cparen, avec, ctx)
        },
        Type::InlineAsm(r) => if let (Some(InterData::InlineAsm(c, b)), false) = (target.inter_val, ctx.is_const.get()) {
            let mut params = Vec::with_capacity(args.len());
            let mut comp_args = Vec::with_capacity(args.len());
            let mut err = CobaltError::InvalidInlineAsmCall {loc, args: vec![]};
            let mut push_arg = |arg: InvalidAsmArg| if let CobaltError::InvalidInlineAsmCall {args, ..} = &mut err {args.push(arg)};
            let mut good = true;
            for (arg, l) in args {
                let n = arg.data_type.to_string();
                if let Some((ty, val)) = prep_asm(arg, ctx) {
                    params.push(ty);
                    comp_args.push(val);
                }
                else {
                    good = false;
                    push_arg(InvalidAsmArg(n, l));
                }
            }
            if !good {return Err(err)}
            if let Some(llt) = r.llvm_type(ctx) {
                let fty = llt.fn_type(&params, false);
                let asm = ctx.context.create_inline_asm(fty, b, c, true, true, None, false);
                let ret = ctx.builder.build_indirect_call(fty, asm, &comp_args, "");
                Ok(Value::new(
                    ret.try_as_basic_value().left(),
                    None,
                    *r
                ))
            }
            else {
                let fty = ctx.context.void_type().fn_type(&params, false);
                let asm = ctx.context.create_inline_asm(fty, b, c, true, true, None, false);
                ctx.builder.build_indirect_call(fty, asm, &comp_args, "");
                Ok(Value::null())
            }
        } else {Ok(Value::error())},
        Type::Tuple(v) => {
            let err = CobaltError::CannotCallWithArgs {
                val: format!("({})", v.iter().map(Type::to_string).collect::<Vec<_>>().join(", ")),
                loc: cparen.map_or(loc, |cp| merge_spans(loc, cp)),
                args: args.iter().map(|(v, _)| v.data_type.to_string()).collect(),
                aloc: args.last().map(|(_, l)| merge_spans(args[0].1, *l)),
                nargs: vec![]
            };
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
                        else {Err(CobaltError::TupleIdxOutOfBounds {
                            idx, len: v.len(),
                            tloc: loc, iloc: *aloc
                        })}
                    }
                    else {Err(CobaltError::NotCompileTime {loc: *aloc})}
                },
                _ => Err(err)
            }
        },
        Type::Intrinsic(name) => match name.as_str() {
            "asm" => {
                let mut args = args.into_iter().collect::<VecDeque<_>>();
                match args.len() {
                    2 => {
                        let (a0, loc0) = args.pop_front().unwrap();
                        let (a1, loc1) = args.pop_front().unwrap();
                        if is_str(&a0.data_type) && is_str(&a1.data_type) {
                            match (a0, a1) {
                                (
                                    Value {inter_val: Some(InterData::Array(c)), ..},
                                    Value {inter_val: Some(InterData::Array(b)), ..}
                                ) => {
                                    let mut errs = vec![];
                                    let c = String::from_utf8(c.into_iter().map(|x| if let InterData::Int(v) = x {v as u8} else {unreachable!()}).collect::<Vec<_>>()).unwrap_or_else(|e| {
                                        errs.push(CobaltError::NonUtf8String {
                                            pos: e.utf8_error().valid_up_to(),
                                            loc: loc0
                                        });
                                        String::new()
                                    });
                                    let b = String::from_utf8(b.into_iter().map(|x| if let InterData::Int(v) = x {v as u8} else {unreachable!()}).collect::<Vec<_>>()).unwrap_or_else(|e| {
                                        errs.push(CobaltError::NonUtf8String {
                                            pos: e.utf8_error().valid_up_to(),
                                            loc: loc1
                                        });
                                        String::new()
                                    });
                                    if !errs.is_empty() {
                                        return Err(CobaltError::InvalidIntrinsicCall {
                                            name: "asm",
                                            loc, errs
                                        });
                                    }
                                    Ok(Value::metaval(InterData::InlineAsm(c, b), Type::InlineAsm(Box::new(Type::Null))))
                                },
                                (a0, a1) => Err(CobaltError::InvalidInlineAsm2 {
                                    loc1: loc0, type1: a0.data_type.to_string(), const1: a0.inter_val.is_some(),
                                    loc2: loc1, type2: a1.data_type.to_string(), const2: a1.inter_val.is_some()
                                })
                            }
                        }
                        else {
                            Err(CobaltError::InvalidInlineAsm2 {
                                loc1: loc0, type1: a0.data_type.to_string(), const1: a0.inter_val.is_some(),
                                loc2: loc1, type2: a1.data_type.to_string(), const2: a1.inter_val.is_some()
                            })
                        }
                    },
                    3 => {
                        let (a0, loc0) = args.pop_front().unwrap();
                        let (a1, loc1) = args.pop_front().unwrap();
                        let (a2, loc2) = args.pop_front().unwrap();
                        if let Value {data_type: Type::TypeData, inter_val: Some(InterData::Type(r)), ..} = a0 {
                            if is_str(&a1.data_type) && is_str(&a2.data_type) {
                                match (a1, a2) {
                                    (
                                        Value {inter_val: Some(InterData::Array(c)), ..},
                                        Value {inter_val: Some(InterData::Array(b)), ..}
                                    ) => {
                                        let mut errs = vec![];
                                        let c = String::from_utf8(c.into_iter().map(|x| if let InterData::Int(v) = x {v as u8} else {unreachable!()}).collect::<Vec<_>>()).unwrap_or_else(|e| {
                                            errs.push(CobaltError::NonUtf8String {
                                                pos: e.utf8_error().valid_up_to(),
                                                loc: loc1
                                            });
                                            String::new()
                                        });
                                        let b = String::from_utf8(b.into_iter().map(|x| if let InterData::Int(v) = x {v as u8} else {unreachable!()}).collect::<Vec<_>>()).unwrap_or_else(|e| {
                                            errs.push(CobaltError::NonUtf8String {
                                                pos: e.utf8_error().valid_up_to(),
                                                loc: loc2
                                            });
                                            String::new()
                                        });
                                        if !errs.is_empty() {
                                            return Err(CobaltError::InvalidIntrinsicCall {
                                                name: "asm",
                                                loc, errs
                                            });
                                        }
                                        Ok(Value::metaval(InterData::InlineAsm(c, b), Type::InlineAsm(r)))
                                    },
                                    (a1, a2) => Err(CobaltError::InvalidInlineAsm3 {
                                        loc1: loc0, type1: "type".to_string(), const1: true,
                                        loc2: loc1, type2: a1.data_type.to_string(), const2: a1.inter_val.is_some(),
                                        loc3: loc2, type3: a2.data_type.to_string(), const3: a2.inter_val.is_some()
                                    })
                                }
                            }
                            else {
                                Err(CobaltError::InvalidInlineAsm3 {
                                    loc1: loc0, type1: "type".to_string(), const1: true,
                                    loc2: loc1, type2: a1.data_type.to_string(), const2: a1.inter_val.is_some(),
                                    loc3: loc2, type3: a2.data_type.to_string(), const3: a2.inter_val.is_some()
                                })
                            }
                        }
                        else {
                            Err(CobaltError::InvalidInlineAsm3 {
                                loc1: loc0, type1: a0.data_type.to_string(), const1: a0.inter_val.is_some(),
                                loc2: loc1, type2: a1.data_type.to_string(), const2: a1.inter_val.is_some(),
                                loc3: loc2, type3: a2.data_type.to_string(), const3: a2.inter_val.is_some()
                            })
                        }
                    },
                    x => {
                        Err(CobaltError::InvalidInlineAsm {
                            nargs: x,
                            loc
                        })
                    }
                }
            },
            "alloca" => {
                let mut args = args.into_iter().collect::<VecDeque<_>>();
                if args.is_empty() {
                    return Err(CobaltError::AllocaNeedsArgs {loc})
                }
                let loc0 = args.front().unwrap().1;
                let ty = (args.front().unwrap().0.data_type == Type::TypeData).then(|| args.pop_front().unwrap().0.into_type()).flatten();
                if args.is_empty() {
                    if let Some(ty) = ty {
                        if let Some(llt) = ty.llvm_type(ctx) {
                            Ok(Value::compiled(ctx.builder.build_alloca(llt, "").into(), Type::Pointer(Box::new(ty), true)))
                        }
                        else {
                            Err(CobaltError::NonRuntimeAllocaType {ty: ty.to_string(), loc: loc0})
                        }
                    }
                    else {
                        unreachable!()
                    }
                }
                else {
                    let mut val = None;
                    let mut errs = vec![];
                    for (mut arg, loc) in args {
                        loop {
                            match arg.data_type {
                                Type::Borrow(b) => arg.data_type = *b,
                                Type::Reference(b, _) => {
                                    if b.register(ctx) && !ctx.is_const.get() {
                                        if let Some(PointerValue(v)) = arg.comp_val {
                                            arg.comp_val = Some(ctx.builder.build_load(b.llvm_type(ctx).unwrap(), v, ""));
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
                                    errs.push(CobaltError::NonIntegralAllocaArg {ty: x.to_string(), loc});
                                    break;
                                }
                            }
                        }
                    }
                    if let Some(ty) = ty {
                        if let Some(llt) = ty.llvm_type(ctx) {
                            if !errs.is_empty() {
                                return Err(CobaltError::InvalidIntrinsicCall {
                                    name: "alloca",
                                    loc, errs
                                })
                            }
                            Ok(Value::compiled(ctx.builder.build_array_alloca(llt, val.unwrap(), "").into(), Type::Pointer(Box::new(ty), true)))
                        }
                        else {
                            errs.push(CobaltError::NonRuntimeAllocaType {ty: ty.to_string(), loc: loc0});
                            Err(CobaltError::InvalidIntrinsicCall {
                                name: "alloca",
                                loc, errs
                            })
                        }
                    }
                    else {
                        if !errs.is_empty() {
                            return Err(CobaltError::InvalidIntrinsicCall {
                                name: "alloca",
                                loc, errs
                            })
                        }
                        Ok(Value::compiled(ctx.builder.build_array_alloca(ctx.context.i8_type(), val.unwrap(), "").into(), Type::Pointer(Box::new(Type::Null), true)))
                    }
                }
            },
            x => Err(CobaltError::UnknownIntrinsic {loc, name: x.to_string()})
        },
        t => Err(CobaltError::CannotCallWithArgs {
            val: t.to_string(),
            loc: cparen.map_or(loc, |cp| merge_spans(loc, cp)),
            args: args.iter().map(|(v, _)| v.data_type.to_string()).collect(),
            aloc: args.last().map(|(_, l)| merge_spans(args[0].1, *l)),
            nargs: vec![]
        })
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
fn is_str(ty: &Type) -> bool {
    match ty {
        Type::Pointer(b, _) => **b == Type::Int(8, true),
        Type::Array(b, _) => **b == Type::Int(8, true),
        Type::Reference(b, _) => if let Type::Array(ref b, _) = **b {**b == Type::Int(8, true)} else {false}
        _ => false
    }
}
