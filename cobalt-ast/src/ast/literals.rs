use crate::*;
use inkwell::values::BasicValueEnum::*;
use inkwell::types::BasicType;
use bstr::ByteSlice;
#[derive(Debug, Clone)]
pub struct IntLiteralAST {
    loc: SourceSpan,
    pub val: i128,
    pub suffix: Option<(String, SourceSpan)>
}
impl IntLiteralAST {
    pub fn new(loc: SourceSpan, val: i128, suffix: Option<(String, SourceSpan)>) -> Self {IntLiteralAST {loc, val, suffix}}
}
impl AST for IntLiteralAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn is_const(&self) -> bool {true}
    fn res_type(&self, _ctx: &CompCtx) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => Type::IntLiteral,
            Some(("isize", _)) => Type::Int(64, false),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some(("usize", _)) => Type::Int(64, true),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => (Value::metaval(InterData::Int(self.val), Type::IntLiteral), vec![]),
            Some(("isize", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(64, false)), vec![]),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (Value::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(size, false)), vec![])
            },
            Some(("usize", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(64, true)), vec![]),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (Value::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val), Type::Int(size, true)), vec![])
            },
            Some((x, loc)) => (Value::error(), vec![CobaltError::UnknownLiteralSuffix {loc: *loc, lit: "integer", suf: x.to_string()}])
        }
    }
    fn to_code(&self) -> String {
        if let Some((ref suf, _)) = self.suffix {
            format!("{}{}", self.val, suf)
        }
        else {
            self.val.to_string()
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        write!(f, "int: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
#[derive(Debug, Clone)]
pub struct FloatLiteralAST {
    loc: SourceSpan,
    pub val: f64,
    pub suffix: Option<(String, SourceSpan)>
}
impl FloatLiteralAST {
    pub fn new(loc: SourceSpan, val: f64, suffix: Option<(String, SourceSpan)>) -> Self {FloatLiteralAST {loc, val, suffix}}
}
impl AST for FloatLiteralAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn is_const(&self) -> bool {true}
    fn res_type(&self, _ctx: &CompCtx) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("f64", _)) => Type::Float64,
            Some(("f16", _)) => Type::Float16,
            Some(("f32", _)) => Type::Float32,
            Some(("f128", _)) => Type::Float64,
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("f64", _)) => (Value::interpreted(FloatValue(ctx.context.f64_type().const_float(self.val)), InterData::Float(self.val), Type::Float64), vec![]),
            Some(("f16", _)) => (Value::interpreted(FloatValue(ctx.context.f16_type().const_float(self.val)), InterData::Float(self.val), Type::Float16), vec![]),
            Some(("f32", _)) => (Value::interpreted(FloatValue(ctx.context.f32_type().const_float(self.val)), InterData::Float(self.val), Type::Float32), vec![]),
            Some(("f128", _)) => (Value::interpreted(FloatValue(ctx.context.f128_type().const_float(self.val)), InterData::Float(self.val), Type::Float128), vec![]),
            Some((x, loc)) => (Value::error(), vec![CobaltError::UnknownLiteralSuffix {loc: *loc, lit: "floating-point", suf: x.to_string()}])
        }
    }
    fn to_code(&self) -> String {
        if let Some((ref suf, _)) = self.suffix {
            format!("{}{}", self.val, suf)
        }
        else {
            self.val.to_string()
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        write!(f, "float: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
#[derive(Debug, Clone)]
pub struct CharLiteralAST {
    loc: SourceSpan,
    pub val: u32,
    pub suffix: Option<(String, SourceSpan)>
}
impl CharLiteralAST {
    pub fn new(loc: SourceSpan, val: u32, suffix: Option<(String, SourceSpan)>) -> Self {CharLiteralAST {loc, val, suffix}}
}
impl AST for CharLiteralAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn is_const(&self) -> bool {true}
    fn res_type(&self, _ctx: &CompCtx) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => Type::Int(32, true),
            Some(("isize", _)) => Type::Int(64, false),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some(("usize", _)) => Type::Int(64, true),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(32, true)), vec![]),
            Some(("isize", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(64, false)), vec![]),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (Value::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(size, false)), vec![])
            },
            Some(("usize", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(64, true)), vec![]),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (Value::interpreted(IntValue(ctx.context.custom_width_int_type(size as u32).const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Int(size, true)), vec![])
            },
            Some((x, loc)) => (Value::error(), vec![CobaltError::UnknownLiteralSuffix {loc: *loc, lit: "character", suf: x.to_string()}])
        }
    }
    fn to_code(&self) -> String {
        if let Some((ref suf, _)) = self.suffix {
            format!("{:?}{}", self.val, suf)
        }
        else {
            format!("{:?}", self.val)
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        if let Some(c) = char::from_u32(self.val) {write!(f, "char: {c:?}")} else {write!(f, "char: \\u{{{:0>X}}}", self.val)}?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
#[derive(Debug, Clone)]
pub struct StringLiteralAST {
    loc: SourceSpan,
    pub val: Vec<u8>,
    pub suffix: Option<(String, SourceSpan)>
}
impl StringLiteralAST {
    pub fn new(loc: SourceSpan, val: Vec<u8>, suffix: Option<(String, SourceSpan)>) -> Self {StringLiteralAST {loc, val, suffix}}
}
impl AST for StringLiteralAST {
    fn loc(&self) -> SourceSpan {self.loc}
    fn is_const(&self) -> bool {true}
    fn res_type(&self, _ctx: &CompCtx) -> Type {
        match self.suffix {
            None => Type::Reference(Box::new(Type::Array(Box::new(Type::Int(8, true)), Some(self.val.len() as u32)))),
            Some(_) => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        match self.suffix.as_ref().map(|(s, l)| (s.as_str(), *l)) {
            None | Some(("c" | "C", _)) => {
                let cs = ctx.context.const_string(&self.val, self.suffix.is_some());
                let gv = ctx.module.add_global(cs.get_type(), None, "cobalt.str");
                gv.set_initializer(&cs);
                gv.set_constant(true);
                gv.set_linkage(inkwell::module::Linkage::Private);
                (Value::interpreted(
                    gv.as_pointer_value().const_cast(ctx.context.i8_type().ptr_type(Default::default())).into(),
                    InterData::Array(self.val.iter().map(|&c| InterData::Int(c as i128)).collect()),
                    Type::Reference(Box::new(Type::Array(Box::new(Type::Int(8, true)), Some(self.val.len() as u32))))
                ), vec![])
            },
            Some((x, loc)) => (Value::error(), vec![CobaltError::UnknownLiteralSuffix {loc, lit: "string", suf: x.to_string()}])
        }
    }
    fn to_code(&self) -> String {
        if let Some((ref suf, _)) = self.suffix {
            format!("{:?}{}", self.val.as_bstr(), suf)
        }
        else {
            format!("{:?}", self.val.as_bstr())
        }
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix, _file: Option<CobaltFile>) -> std::fmt::Result {
        write!(f, "string: {:?}",self.val.as_bstr())?;
        if let Some((ref s, _)) = self.suffix {write!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
#[derive(Debug, Clone)]
pub struct ArrayLiteralAST {
    pub start: SourceSpan,
    pub end: SourceSpan,
    pub vals: Vec<Box<dyn AST>>,
}
impl ArrayLiteralAST {
    pub fn new(start: SourceSpan, end: SourceSpan, vals: Vec<Box<dyn AST>>) -> Self {ArrayLiteralAST {start, end, vals}}
}
impl AST for ArrayLiteralAST {
    fn loc(&self) -> SourceSpan {merge_spans(self.start, self.end)}
    fn nodes(&self) -> usize {self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn res_type(&self, ctx: &CompCtx) -> Type {
        let mut elem = self.vals.get(0).map_or(Type::Null, |x| ops::decay(x.res_type(ctx)));
        for val in self.vals.iter() {
            if let Some(c) = ops::common(&elem, &ops::decay(val.res_type(ctx))) {elem = c;}
            else {
                elem = Type::Error;
                break;
            }
        }
        Type::Array(Box::new(elem), Some(self.vals.len().try_into().unwrap_or(u32::MAX)))
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut elems = vec![];
        let mut ty = Type::Null;
        let mut first = true;
        let mut elem_loc = unreachable_span();
        let mut errs = vec![];
        for val in self.vals.iter() {
            let (v, mut es) = val.codegen(ctx);
            let dt = ops::decay(v.data_type.clone());
            errs.append(&mut es);
            if first {
                first = false;
                elem_loc = val.loc();
                ty = dt;
            }
            else if ty != dt {
                if let Some(t) = ops::common(&ty, &dt) {
                    ty = t;
                    elem_loc = val.loc();
                }
                else {
                    errs.push(CobaltError::ArrayElementsDontMatch {
                        loc: val.loc(),
                        prev: elem_loc,
                        current: ty.to_string(),
                        new: dt.to_string()
                    });
                }
            }
            elems.push(v);
        }
        if elems.len() > u32::MAX as usize {
            errs.push(CobaltError::ArrayTooLong {loc: self.vals[u32::MAX as usize + 1].loc(), len: elems.len()});
            elems.truncate(u32::MAX as usize);
        }
        let elems = elems.into_iter().enumerate().filter_map(|(n, v)| ops::impl_convert(self.vals[n].loc(), (v, None), (ty.clone(), None), ctx).map_err(|e| errs.push(e)).ok()).collect::<Vec<_>>();
        let len = elems.len();
        (Value::new(
            if let (Some(llt), false) = (ty.llvm_type(ctx), ctx.is_const.get()) {
                let arr_ty = llt.array_type(elems.len() as u32);
                elems.iter().enumerate().try_fold(arr_ty.get_undef(), |val, (n, v)| ctx.builder.build_insert_value(val, v.comp_val?, n as _, "").map(|v| v.into_array_value())).map(Into::into)
            } else {None},
            elems.into_iter().map(|v| v.inter_val).collect::<Option<_>>().map(InterData::Array),
            Type::Array(Box::new(ty), Some(len as u32))
        ), errs)
    }
    fn to_code(&self) -> String {
        let mut out = "[".to_string();
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if len != 1 {
                out += ", ";
                len -= 1;
            }
        }
        out + "]"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "array")?;
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, len == 1, file)?;
            len -= 1;
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct TupleLiteralAST {
    pub vals: Vec<Box<dyn AST>>
}
impl TupleLiteralAST {
    pub fn new(vals: Vec<Box<dyn AST>>) -> Self {
        assert_ne!(vals.len(), 0);
        TupleLiteralAST {vals}
    }
}
impl AST for TupleLiteralAST {
    fn loc(&self) -> SourceSpan {
        let start = self.vals.first().unwrap().loc();
        let end = self.vals.last().unwrap().loc();
        merge_spans(start, end)
    }
    fn nodes(&self) -> usize {self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1}
    fn res_type(&self, ctx: &CompCtx) -> Type {
        Type::Tuple(self.vals.iter().map(|x| ops::decay(x.res_type(ctx))).collect())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<CobaltError>) {
        let mut errs = vec![];
        let (comps, (inters, types)): (Vec<_>, (Vec<_>, Vec<_>)) = self.vals.iter().map(|x| {
            let mut v = x.codegen_errs(ctx, &mut errs);
            let decayed = ops::decay(v.data_type.clone());
            v = ops::impl_convert(unreachable_span(), (v, None), (decayed, None), ctx).unwrap();
            v
        }).map(|Value {comp_val, inter_val, data_type, ..}| (comp_val, (inter_val, data_type))).unzip();
        let mut val = Value::null();
        val.data_type = Type::Tuple(types);
        if comps.iter().all(Option::is_some) {
            let llt = val.data_type.llvm_type(ctx).unwrap().into_struct_type();
            val.comp_val = comps.into_iter().map(Option::unwrap).enumerate().try_fold(llt.get_undef(), |sv, (n, v)| ctx.builder.build_insert_value(sv, v, n as u32, "").map(|x| x.into_struct_value())).map(From::from);
        };
        if inters.iter().all(Option::is_some) {val.inter_val = Some(InterData::Array(inters.into_iter().map(Option::unwrap).collect()));};
        (val, errs)
    }
    fn to_code(&self) -> String {
        let mut out = "(".to_string();
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            out += &val.to_code();
            if len != 1 {
                out += ", ";
                len -= 1;
            }
        }
        out + ")"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix, file: Option<CobaltFile>) -> std::fmt::Result {
        writeln!(f, "tuple")?;
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, len == 1, file)?;
            len -= 1;
        }
        Ok(())
    }
}
