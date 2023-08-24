use crate::*;
use bstr::ByteSlice;
use inkwell::types::BasicType;
use inkwell::values::BasicValueEnum::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub struct IntLiteralAST<'src> {
    loc: SourceSpan,
    pub val: i128,
    pub suffix: Option<(Cow<'src, str>, SourceSpan)>,
}
impl<'src> IntLiteralAST<'src> {
    pub fn new(loc: SourceSpan, val: i128, suffix: Option<(Cow<'src, str>, SourceSpan)>) -> Self {
        IntLiteralAST { loc, val, suffix }
    }
}
impl<'src> AST<'src> for IntLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn is_const(&self) -> bool {
        true
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.suffix.as_ref().map(|(x, y)| (&**x, y)) {
            None | Some(("", _)) => (
                Value::metaval(InterData::Int(self.val), types::IntLiteral::new()),
                vec![],
            ),
            Some(("isize", _)) => {
                let bits = ctx.flags.word_size * 8;
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(bits as _)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val),
                        types::Int::signed(bits),
                    ),
                    vec![],
                )
            }
            Some((x, _)) if x.as_bytes()[0] == b'i' && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(size as u32)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val),
                        types::Int::signed(size),
                    ),
                    vec![],
                )
            }
            Some(("usize", _)) => {
                let bits = ctx.flags.word_size * 8;
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(bits as _)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val),
                        types::Int::unsigned(bits),
                    ),
                    vec![],
                )
            }
            Some((x, _)) if x.as_bytes()[0] == b'u' && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(size as u32)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val),
                        types::Int::signed(size),
                    ),
                    vec![],
                )
            }
            Some((_, loc)) => (
                Value::error(),
                vec![CobaltError::UnknownLiteralSuffix {
                    loc: *loc,
                    lit: "integer",
                    suf: self.suffix.clone().unwrap().0,
                }],
            ),
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        write!(f, "int: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {
            writeln!(f, ", suffix: {s}")
        } else {
            writeln!(f)
        }
    }
}
#[derive(Debug, Clone)]
pub struct FloatLiteralAST<'src> {
    loc: SourceSpan,
    pub val: f64,
    pub suffix: Option<(Cow<'src, str>, SourceSpan)>,
}
impl<'src> FloatLiteralAST<'src> {
    pub fn new(loc: SourceSpan, val: f64, suffix: Option<(Cow<'src, str>, SourceSpan)>) -> Self {
        FloatLiteralAST { loc, val, suffix }
    }
}
impl<'src> AST<'src> for FloatLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn is_const(&self) -> bool {
        true
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.suffix.as_ref().map(|(x, y)| (&**x, y)) {
            None | Some(("f64", _)) => (
                Value::interpreted(
                    FloatValue(ctx.context.f64_type().const_float(self.val)),
                    InterData::Float(self.val),
                    types::Float::f64(),
                ),
                vec![],
            ),
            Some(("f16", _)) => (
                Value::interpreted(
                    FloatValue(ctx.context.f16_type().const_float(self.val)),
                    InterData::Float(self.val),
                    types::Float::f16(),
                ),
                vec![],
            ),
            Some(("f32", _)) => (
                Value::interpreted(
                    FloatValue(ctx.context.f32_type().const_float(self.val)),
                    InterData::Float(self.val),
                    types::Float::f32(),
                ),
                vec![],
            ),
            Some(("f128", _)) => (
                Value::interpreted(
                    FloatValue(ctx.context.f128_type().const_float(self.val)),
                    InterData::Float(self.val),
                    types::Float::f128(),
                ),
                vec![],
            ),
            Some((_, loc)) => (
                Value::error(),
                vec![CobaltError::UnknownLiteralSuffix {
                    loc: *loc,
                    lit: "floating-point",
                    suf: self.suffix.clone().unwrap().0,
                }],
            ),
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        write!(f, "float: {}", self.val)?;
        if let Some((ref s, _)) = self.suffix {
            writeln!(f, ", suffix: {s}")
        } else {
            writeln!(f)
        }
    }
}
#[derive(Debug, Clone)]
pub struct CharLiteralAST<'src> {
    loc: SourceSpan,
    pub val: u32,
    pub suffix: Option<(Cow<'src, str>, SourceSpan)>,
}
impl<'src> CharLiteralAST<'src> {
    pub fn new(loc: SourceSpan, val: u32, suffix: Option<(Cow<'src, str>, SourceSpan)>) -> Self {
        CharLiteralAST { loc, val, suffix }
    }
}
impl<'src> AST<'src> for CharLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn is_const(&self) -> bool {
        true
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.suffix.as_ref().map(|(x, y)| (&**x, y)) {
            None | Some(("", _)) => (
                Value::metaval(InterData::Int(self.val as _), types::Int::unsigned(32)),
                vec![],
            ),
            Some(("isize", _)) => {
                let bits = ctx.flags.word_size * 8;
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(bits as _)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val as _),
                        types::Int::signed(bits),
                    ),
                    vec![],
                )
            }
            Some((x, _)) if x.as_bytes()[0] == b'i' && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(size as u32)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val as _),
                        types::Int::signed(size),
                    ),
                    vec![],
                )
            }
            Some(("usize", _)) => {
                let bits = ctx.flags.word_size * 8;
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(bits as _)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val as _),
                        types::Int::unsigned(bits),
                    ),
                    vec![],
                )
            }
            Some((x, _)) if x.as_bytes()[0] == b'u' && x[1..].chars().all(char::is_numeric) => {
                let size: u16 = x[1..].parse().unwrap_or(0);
                (
                    Value::interpreted(
                        IntValue(
                            ctx.context
                                .custom_width_int_type(size as u32)
                                .const_int(self.val as u64, false),
                        ),
                        InterData::Int(self.val),
                        types::Int::signed(size),
                    ),
                    vec![],
                )
            }
            Some((_, loc)) => (
                Value::error(),
                vec![CobaltError::UnknownLiteralSuffix {
                    loc: *loc,
                    lit: "character",
                    suf: self.suffix.clone().unwrap().0,
                }],
            ),
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        if let Some(c) = char::from_u32(self.val) {
            write!(f, "char: {c:?}")
        } else {
            write!(f, "char: \\u{{{:0>X}}}", self.val)
        }?;
        if let Some((ref s, _)) = self.suffix {
            writeln!(f, ", suffix: {s}")
        } else {
            writeln!(f)
        }
    }
}
#[derive(Debug, Clone)]
pub struct StringLiteralAST<'src> {
    loc: SourceSpan,
    pub val: Vec<u8>,
    pub suffix: Option<(Cow<'src, str>, SourceSpan)>,
}
impl<'src> StringLiteralAST<'src> {
    pub fn new(
        loc: SourceSpan,
        val: Vec<u8>,
        suffix: Option<(Cow<'src, str>, SourceSpan)>,
    ) -> Self {
        StringLiteralAST { loc, val, suffix }
    }
}
impl<'src> AST<'src> for StringLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn is_const(&self) -> bool {
        true
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        match self.suffix.as_ref().map(|(s, l)| (&**s, *l)) {
            None | Some(("c" | "C", _)) => {
                let cs = ctx.context.const_string(&self.val, true);
                let gv = ctx.module.add_global(cs.get_type(), None, "cobalt.str");
                gv.set_initializer(&cs);
                gv.set_constant(true);
                gv.set_linkage(inkwell::module::Linkage::Private);
                (
                    Value::interpreted(
                        gv.as_pointer_value()
                            .const_cast(ctx.context.i8_type().ptr_type(Default::default()))
                            .into(),
                        InterData::Array(
                            self.val
                                .iter()
                                .map(|&c| InterData::Int(c as i128))
                                .collect(),
                        ),
                        types::Reference::new(types::SizedArray::new(
                            types::Int::unsigned(8),
                            self.val.len() as u32 + self.suffix.is_some() as u32,
                        )),
                    ),
                    vec![],
                )
            }
            Some((_, loc)) => (
                Value::error(),
                vec![CobaltError::UnknownLiteralSuffix {
                    loc,
                    lit: "string",
                    suf: self.suffix.clone().unwrap().0,
                }],
            ),
        }
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        write!(f, "string: {:?}", self.val.as_bstr())?;
        if let Some((ref s, _)) = self.suffix {
            writeln!(f, ", suffix: {s}")
        } else {
            writeln!(f)
        }
    }
}
#[derive(Debug, Clone)]
pub struct ArrayLiteralAST<'src> {
    pub start: SourceSpan,
    pub end: SourceSpan,
    pub vals: Vec<BoxedAST<'src>>,
}
impl<'src> ArrayLiteralAST<'src> {
    pub fn new(start: SourceSpan, end: SourceSpan, vals: Vec<BoxedAST<'src>>) -> Self {
        ArrayLiteralAST { start, end, vals }
    }
}
impl<'src> AST<'src> for ArrayLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        merge_spans(self.start, self.end)
    }
    fn nodes(&self) -> usize {
        self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut elems = vec![];
        let mut ty = types::Null::new();
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
            } else if ty != dt {
                if let Some(t) = ops::common(&ty, &dt, ctx) {
                    ty = t;
                    elem_loc = val.loc();
                } else {
                    errs.push(CobaltError::ArrayElementsDontMatch {
                        loc: val.loc(),
                        prev: elem_loc,
                        current: ty.to_string(),
                        new: dt.to_string(),
                    });
                }
            }
            elems.push(v);
        }
        if elems.len() > u32::MAX as usize {
            errs.push(CobaltError::ArrayTooLong {
                loc: self.vals[u32::MAX as usize + 1].loc(),
                len: elems.len(),
            });
            elems.truncate(u32::MAX as usize);
        }
        let elems = elems
            .into_iter()
            .enumerate()
            .filter_map(|(n, v)| {
                ops::impl_convert(self.vals[n].loc(), (v, None), (ty.clone(), None), ctx)
                    .map_err(|e| errs.push(e))
                    .ok()
            })
            .collect::<Vec<_>>();
        let len = elems.len();
        (
            Value::new(
                if let (Some(llt), false) = (ty.llvm_type(ctx), ctx.is_const.get()) {
                    let arr_ty = llt.array_type(elems.len() as u32);
                    elems
                        .iter()
                        .enumerate()
                        .try_fold(arr_ty.get_undef(), |val, (n, v)| {
                            ctx.builder
                                .build_insert_value(val, v.comp_val?, n as _, "")
                                .map(|v| v.into_array_value())
                        })
                        .map(Into::into)
                } else {
                    None
                },
                elems
                    .into_iter()
                    .map(|v| v.inter_val)
                    .collect::<Option<_>>()
                    .map(InterData::Array),
                types::SizedArray::new(ty, len as _),
            ),
            errs,
        )
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
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
pub struct TupleLiteralAST<'src> {
    pub vals: Vec<BoxedAST<'src>>,
}
impl<'src> TupleLiteralAST<'src> {
    pub fn new(vals: Vec<BoxedAST<'src>>) -> Self {
        assert_ne!(vals.len(), 0);
        TupleLiteralAST { vals }
    }
}
impl<'src> AST<'src> for TupleLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        let start = self.vals.first().unwrap().loc();
        let end = self.vals.last().unwrap().loc();
        merge_spans(start, end)
    }
    fn nodes(&self) -> usize {
        self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let (comps, (inters, types)): (Vec<_>, (Vec<_>, Vec<_>)) = self
            .vals
            .iter()
            .map(|x| {
                let mut v = x.codegen_errs(ctx, &mut errs);
                let decayed = ops::decay(v.data_type.clone());
                v = ops::impl_convert(unreachable_span(), (v, None), (decayed, None), ctx).unwrap();
                v
            })
            .map(
                |Value {
                     comp_val,
                     inter_val,
                     data_type,
                     ..
                 }| (comp_val, (inter_val, data_type)),
            )
            .unzip();
        let mut val = Value::null();
        val.data_type = types::Tuple::new(types);
        if comps.iter().all(Option::is_some) {
            let llt = val.data_type.llvm_type(ctx).unwrap().into_struct_type();
            val.comp_val = comps
                .into_iter()
                .map(Option::unwrap)
                .enumerate()
                .try_fold(llt.get_undef(), |sv, (n, v)| {
                    ctx.builder
                        .build_insert_value(sv, v, n as u32, "")
                        .map(|x| x.into_struct_value())
                })
                .map(From::from);
        };
        if inters.iter().all(Option::is_some) {
            val.inter_val = Some(InterData::Array(
                inters.into_iter().map(Option::unwrap).collect(),
            ));
        };
        (val, errs)
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "tuple")?;
        let mut len = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, len == 1, file)?;
            len -= 1;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructLiteralAST<'src> {
    loc: SourceSpan,

    /// The key is the name of the field, the value is the value of the field.
    vals: HashMap<Cow<'src, str>, BoxedAST<'src>>,
}
impl<'src> StructLiteralAST<'src> {
    pub fn new(loc: SourceSpan, vals: HashMap<Cow<'src, str>, BoxedAST<'src>>) -> Self {
        StructLiteralAST { loc, vals }
    }
}
impl<'src> AST<'src> for StructLiteralAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.vals.values().map(|t| t.nodes()).sum::<usize>() + 1
    }
    fn codegen_impl<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];

        // Codegen for each field and collect the errors.
        let mut vec = self
            .vals
            .iter()
            .map(|(n, v)| (n, v.codegen_errs(ctx, &mut errs)))
            .collect::<Vec<_>>();

        // Sort fields to have optimal memory layout.
        vec.sort_by(|(ln, lhs), (rn, rhs)| {
            types::Struct::sort_fields((&**ln, lhs.data_type), (&**rn, rhs.data_type))
        });

        // Determine the data type of this literal just by looking at the fields it has defined.
        let mut lookup = std::collections::BTreeMap::new();
        for (n, (name, _)) in vec.iter().enumerate() {
            lookup.insert(Box::from(**name), n);
        }

        // Compute the value, if possible, of the fields.
        let comp_val = if !ctx.is_const.get() {
            let v = vec
                .iter()
                .map(|x| x.1.value(ctx))
                .collect::<Option<Vec<_>>>();
            if let Some(vals) = v {
                let llt = ctx.context.struct_type(
                    &vals.iter().map(|v| v.get_type()).collect::<Vec<_>>(),
                    false,
                );
                Some(
                    vals.into_iter()
                        .enumerate()
                        .fold(llt.get_undef(), |val, (n, elem)| {
                            ctx.builder
                                .build_insert_value(val, elem, n as _, "")
                                .unwrap()
                                .into_struct_value()
                        }),
                )
            } else {
                None
            }
        } else {
            None
        };
        let inter_val = vec
            .iter()
            .map(|v| v.1.inter_val.clone())
            .collect::<Option<_>>()
            .map(InterData::Array);
        let data_type = unsafe {
            types::Struct::new_arranged(
                vec.into_iter().map(|v| v.1.data_type).collect::<Vec<_>>(),
                lookup,
            )
        };

        (
            Value::new(comp_val.map(From::from), inter_val, data_type),
            errs,
        )
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "struct")?;
        let mut v = self.vals.iter().collect::<Vec<_>>();
        v.sort_by_key(|x| x.0);
        for (n, (name, val)) in v.into_iter().enumerate() {
            let last = n == self.vals.len() - 1;
            write!(f, "{pre}{} {name}: ", if last { "└──" } else { "├──" })?;
            if f.alternate() {
                if let Some(Err(e)) = (|| -> Option<std::fmt::Result> {
                    let file = file?;
                    let slice = val.loc();
                    let (sl, sc) = file.source_loc(slice.offset()).ok()?;
                    let (el, ec) = file.source_loc(slice.offset() + slice.len()).ok()?;
                    Some(write!(f, "({sl}:{sc}..{el}:{ec}) "))
                })() {
                    return Err(e);
                };
            }
            pre.push(last);
            val.print_impl(f, pre, file)?;
            pre.pop();
        }
        Ok(())
    }
}
