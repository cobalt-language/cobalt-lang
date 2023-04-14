use crate::*;
use inkwell::values::BasicValueEnum::*;
use inkwell::types::BasicType;
pub struct IntLiteralAST {
    loc: Location,
    pub val: i128,
    pub suffix: Option<(String, Location)>
}
impl IntLiteralAST {
    pub fn new(loc: Location, val: i128, suffix: Option<(String, Location)>) -> Self {IntLiteralAST {loc, val, suffix}}
}
impl AST for IntLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => Type::IntLiteral,
            Some(("isize", _)) => Type::Int(64, false),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some(("usize", _)) => Type::Int(64, true),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
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
            Some((x, loc)) => (Value::error(), vec![Diagnostic::error(loc.clone(), 390, Some(format!("unknown suffix {x} for integer literal")))])
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "int: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct FloatLiteralAST {
    loc: Location,
    pub val: f64,
    pub suffix: Option<(String, Location)>
}
impl FloatLiteralAST {
    pub fn new(loc: Location, val: f64, suffix: Option<(String, Location)>) -> Self {FloatLiteralAST {loc, val, suffix}}
}
impl AST for FloatLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("f64", _)) => Type::Float64,
            Some(("f16", _)) => Type::Float16,
            Some(("f32", _)) => Type::Float32,
            Some(("f128", _)) => Type::Float64,
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("f64", _)) => (Value::interpreted(FloatValue(ctx.context.f64_type().const_float(self.val)), InterData::Float(self.val), Type::Float64), vec![]),
            Some(("f16", _)) => (Value::interpreted(FloatValue(ctx.context.f16_type().const_float(self.val)), InterData::Float(self.val), Type::Float16), vec![]),
            Some(("f32", _)) => (Value::interpreted(FloatValue(ctx.context.f32_type().const_float(self.val)), InterData::Float(self.val), Type::Float32), vec![]),
            Some(("f128", _)) => (Value::interpreted(FloatValue(ctx.context.f128_type().const_float(self.val)), InterData::Float(self.val), Type::Float128), vec![]),
            Some((x, loc)) => (Value::error(), vec![Diagnostic::error(loc.clone(), 390, Some(format!("unknown suffix {x} for float literal")))])
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "float: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct CharLiteralAST {
    loc: Location,
    pub val: char,
    pub suffix: Option<(String, Location)>
}
impl CharLiteralAST {
    pub fn new(loc: Location, val: char, suffix: Option<(String, Location)>) -> Self {CharLiteralAST {loc, val, suffix}}
}
impl AST for CharLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => Type::Char,
            Some(("isize", _)) => Type::Int(64, false),
            Some((x, _)) if x.as_bytes()[0] == 0x69 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), false),
            Some(("usize", _)) => Type::Int(64, true),
            Some((x, _)) if x.as_bytes()[0] == 0x75 && x[1..].chars().all(char::is_numeric) => Type::Int(x[1..].parse().unwrap_or(0), true),
            _ => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match self.suffix.as_ref().map(|(x, y)| (x.as_str(), y)) {
            None | Some(("", _)) => (Value::interpreted(IntValue(ctx.context.i64_type().const_int(self.val as u64, false)), InterData::Int(self.val as i128), Type::Char), vec![]),
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
            Some((x, loc)) => (Value::error(), vec![Diagnostic::error(loc.clone(), 390, Some(format!("unknown suffix {x} for character literal")))])
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "char: {:?}", self.val)?;
        if let Some((ref s, _)) = self.suffix {writeln!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct StringLiteralAST {
    loc: Location,
    pub val: String,
    pub suffix: Option<(String, Location)>
}
impl StringLiteralAST {
    pub fn new(loc: Location, val: String, suffix: Option<(String, Location)>) -> Self {StringLiteralAST {loc, val, suffix}}
}
impl AST for StringLiteralAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn is_const(&self) -> bool {true}
    fn res_type<'ctx>(&self, _ctx: &CompCtx<'ctx>) -> Type {
        match self.suffix {
            None => Type::Reference(Box::new(Type::Array(Box::new(Type::Int(8, true)), Some(self.val.len() as u32))), false),
            Some(_) => Type::Null
        }
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        match &self.suffix {
            None => {
                let cs = ctx.context.const_string(self.val.as_bytes(), true);
                let gv = ctx.module.add_global(cs.get_type(), None, "cobalt.str");
                gv.set_initializer(&cs);
                gv.set_constant(true);
                gv.set_linkage(inkwell::module::Linkage::Private);
                (Value::interpreted(gv.as_pointer_value().const_cast(ctx.context.i8_type().ptr_type(inkwell::AddressSpace::from(0u16))).into(), InterData::Str(self.val.clone()), Type::Reference(Box::new(Type::Array(Box::new(Type::Int(8, false)), Some(self.val.len() as u32))), false)), vec![])
            },
            Some((x, loc)) => (Value::error(), vec![Diagnostic::error(loc.clone(), 390, Some(format!("unknown suffix {x} for string literal")))])
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, _pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "string: {:?}", self.val)?;
        if let Some((ref s, _)) = self.suffix {write!(f, ", suffix: {}", s)}
        else {writeln!(f)}
    }
}
pub struct ArrayLiteralAST {
    pub start: Location,
    pub end: Location,
    pub vals: Vec<Box<dyn AST>>,
}
impl ArrayLiteralAST {
    pub fn new(start: Location, end: Location, vals: Vec<Box<dyn AST>>) -> Self {ArrayLiteralAST {start, end, vals}}
}
impl AST for ArrayLiteralAST {
    fn loc(&self) -> Location {(self.start.0, self.start.1.start..self.end.1.end)}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        let mut elem = self.vals.get(0).map_or(Type::Null, |x| match x.res_type(ctx) {
            Type::IntLiteral => Type::Int(64, false),
            Type::Reference(b, m) => match *b {
                x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                x => x
            },
            x => x
        });
        for val in self.vals.iter() {
            if let Some(c) = types::utils::common(&elem, &match val.res_type(ctx) {
                Type::IntLiteral => Type::Int(64, false),
                Type::Reference(b, m) => match *b {
                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                    x => x
                },
                x => x
            }) {elem = c;}
            else {
                elem = Type::Error;
                break;
            }
        }
        Type::Reference(Box::new(Type::Array(Box::new(elem), Some(self.vals.len().try_into().unwrap_or(u32::MAX)))), true)
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut elems = vec![];
        let mut ty = Type::Null;
        let mut first = true;
        let mut elem_loc: Location = (0, 0..0);
        let mut errs = vec![];
        for val in self.vals.iter() {
            let (v, mut es) = val.codegen(ctx);
            let dt = match v.data_type.clone() {
                Type::IntLiteral => Type::Int(64, false),
                Type::Reference(b, m) => match *b {
                    x @ Type::Array(..) => Type::Reference(Box::new(x), m),
                    x => x
                },
                x => x
            };
            errs.append(&mut es);
            if first {
                first = false;
                elem_loc = val.loc();
                ty = dt;
            }
            else if ty != dt {
                if let Some(t) = types::utils::common(&ty, &dt) {
                    ty = t;
                    elem_loc = val.loc();
                }
                else {
                    errs.push(Diagnostic::error(val.loc(), 300, Some(format!("expected {ty}, got value of type {dt}"))).note(elem_loc.clone(), format!("type set to {ty} here")));
                }
            }
            elems.push(v);
        }
        if elems.len() > u32::MAX as usize {
            errs.push(Diagnostic::error(self.loc(), 300, Some(format!("this array has {} elements, the max is 4294967295", elems.len()))));
            elems.truncate(u32::MAX as usize);
        }
        let elems = elems.into_iter().enumerate().filter_map(|(n, v)| types::utils::impl_convert(self.vals[n].loc(), (v, None), (ty.clone(), None), ctx).map_err(|e| errs.push(e)).ok()).collect::<Vec<_>>();
        let len = elems.len();
        (Value::new(
            if let (Some(llt), false) = (ty.llvm_type(ctx), ctx.is_const.get()) {
                let arr_ty = llt.array_type(elems.len() as u32);
                let alloca = 
                    if ctx.global.get() {
                        let gv = ctx.module.add_global(arr_ty, None, "cobalt.arr");
                        gv.set_linkage(inkwell::module::Linkage::Private);
                        gv.set_initializer(&arr_ty.const_zero());
                        gv.as_pointer_value()
                    }
                    else {ctx.builder.build_alloca(llt.array_type(elems.len() as u32), "")};
                let llv = ctx.builder.build_pointer_cast(alloca, llt.ptr_type(inkwell::AddressSpace::from(0u16)), "");
                for (n, elem) in elems.iter().enumerate() {
                    let gep = unsafe {ctx.builder.build_in_bounds_gep(llv, &[ctx.context.i64_type().const_int(n as u64, false)], "")};
                    ctx.builder.build_store(gep, elem.value(ctx).unwrap_or_else(|| llt.const_zero()));
                }
                Some(llv.into())
            } else {None},
            Some(InterData::Array(elems.into_iter().map(|v| v.inter_val.unwrap_or(InterData::Null)).collect())),
            Type::Reference(Box::new(Type::Array(Box::new(ty), Some(len as u32))), true)
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "array")?;
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, len == 1)?;
            len -= 1;
        }
        Ok(())
    }
}
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
    fn loc(&self) -> Location {
        let start = self.vals.first().unwrap().loc();
        let end = self.vals.last().unwrap().loc();
        (start.0, start.1.start..end.1.end)
    }
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        Type::Tuple(self.vals.iter().map(|x| match x.res_type(ctx) {
            Type::IntLiteral => Type::Int(64, false),
            Type::Reference(b, m) => if b.register(ctx) {
                if x.expl_type(ctx) {Type::Reference(b, m)}
                else {*b}
            } else {Type::Reference(b, m)}
            x => x
        }).collect())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Value<'ctx>, Vec<Diagnostic>) {
        let mut errs = vec![];
        let (comps, (inters, types)): (Vec<_>, (Vec<_>, Vec<_>)) = self.vals.iter().map(|x| {
            let mut v = x.codegen_errs(ctx, &mut errs);
            match v.data_type {
                Type::IntLiteral => Value {data_type: Type::Int(64, false), ..v},
                Type::Reference(b, m) => if b.register(ctx) {
                    if x.expl_type(ctx) {Value {data_type: Type::Reference(b, m), ..v}}
                    else {
                        if !ctx.is_const.get() {
                            if let Some(PointerValue(pv)) = v.comp_val {
                                v.comp_val = Some(ctx.builder.build_load(pv, ""));
                            }
                        }
                        v.data_type = *b;
                        v
                    }
                } else {Value {data_type: Type::Reference(b, m), ..v}}
                x => Value {data_type: x, ..v}
            }
        }).map(|Value {comp_val, inter_val, data_type, ..}| (comp_val, (inter_val, data_type))).unzip();
        let mut val = Value::null();
        val.data_type = Type::Tuple(types);
        if comps.iter().all(Option::is_some) {
            let llt = val.data_type.llvm_type(ctx).unwrap();
            let alloca = ctx.builder.build_alloca(llt, "");
            let comps = comps.into_iter().map(Option::unwrap).collect::<Vec<_>>();
            comps.into_iter().enumerate().for_each(|(n, v)| {
                let gep = ctx.builder.build_struct_gep(alloca, n as u32, "").unwrap();
                ctx.builder.build_store(gep, v);
            });
            val.comp_val = Some(ctx.builder.build_load(alloca, ""));
            val.address.set(Some(alloca));
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
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "tuple")?;
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, len == 1)?;
            len -= 1;
        }
        Ok(())
    }
}
