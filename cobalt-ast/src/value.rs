use crate::*;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum::*};
use inkwell::values::{BasicValueEnum, PointerValue};
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(crate = "serde_state")]
pub enum MethodType {
    Static,
    Method,
    Getter,
}
#[derive(Debug, Clone, SerializeState, DeserializeState)]
#[serde(crate = "serde_state")]
#[serde(serialize_state = "()")]
#[serde(de_parameters = "'a", deserialize_state = "&'a CompCtx<'src, 'ctx>")]
pub struct FnData<'src, 'ctx> {
    #[serde(deserialize_state)]
    pub defaults: Vec<InterData<'src, 'ctx>>,
    pub cconv: u32,
    pub mt: MethodType,
}
impl Serialize for FnData<'_, '_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize_state(serializer, &())
    }
}

/// Used for compile-time constants.
#[derive(Debug, Clone, SerializeState, DeserializeState)]
#[serde(crate = "serde_state")]
#[serde(serialize_state = "()")]
#[serde(de_parameters = "'a", deserialize_state = "&'a CompCtx<'src, 'ctx>")]
pub enum InterData<'src, 'ctx> {
    Null,
    Int(i128),
    Float(f64),
    /// Used for tuples, structs, arrays, and bound methods.
    Array(#[serde(deserialize_state)] Vec<Self>),
    /// Used for default values of function parameters.
    Function(#[serde(deserialize_state)] FnData<'src, 'ctx>),
    InlineAsm(String, String),
    Type(TypeRef),
    Module(
        #[serde(deserialize_state)] HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        Vec<(CompoundDottedName<'src>, bool)>,
        String,
    ),
}
impl Serialize for InterData<'_, '_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize_state(serializer, &())
    }
}

#[derive(Debug, Clone)]
pub struct Value<'src, 'ctx> {
    pub loc: SourceSpan,
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData<'src, 'ctx>>,
    pub data_type: TypeRef,
    pub address: Rc<Cell<Option<PointerValue<'ctx>>>>,
    pub name: Option<(Cow<'src, str>, usize)>,
    pub frozen: Option<SourceSpan>,
}
impl<'src, 'ctx> Value<'src, 'ctx> {
    pub fn error() -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: None,
            data_type: types::Error::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn null() -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: None,
            data_type: types::Null::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn new(
        comp_val: Option<BasicValueEnum<'ctx>>,
        inter_val: Option<InterData<'src, 'ctx>>,
        data_type: TypeRef,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val,
            inter_val,
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn with_addr(
        comp_val: Option<BasicValueEnum<'ctx>>,
        inter_val: Option<InterData<'src, 'ctx>>,
        data_type: TypeRef,
        addr: PointerValue<'ctx>,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val,
            inter_val,
            data_type,
            address: Rc::new(Cell::new(Some(addr))),
            name: None,
            frozen: None,
        }
    }
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: Some(comp_val),
            inter_val: None,
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn interpreted(
        comp_val: BasicValueEnum<'ctx>,
        inter_val: InterData<'src, 'ctx>,
        data_type: TypeRef,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: Some(comp_val),
            inter_val: Some(inter_val),
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn metaval(inter_val: InterData<'src, 'ctx>, data_type: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(inter_val),
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn make_type(type_: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Type(type_)),
            data_type: types::TypeData::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn empty_mod(name: String) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Module(HashMap::new(), vec![], name)),
            data_type: types::Module::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn make_mod(
        syms: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        imps: Vec<(CompoundDottedName<'src>, bool)>,
        name: String,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Module(syms, imps, name)),
            data_type: types::Module::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn make_str<S: AsRef<[u8]>>(string: S, ctx: &CompCtx<'src, 'ctx>) -> Self {
        let bytes = string.as_ref();
        Value::interpreted(
            ctx.context.const_string(bytes, true).into(),
            InterData::Array(bytes.iter().map(|b| InterData::Int(*b as _)).collect()),
            types::SizedArray::new(types::Int::unsigned(8), bytes.len() as _),
        )
    }

    pub fn addr(&self, ctx: &CompCtx<'src, 'ctx>) -> Option<PointerValue<'ctx>> {
        if self.data_type.size() == SizeType::Static(0) {
            Some(ctx.null_type.ptr_type(Default::default()).const_null())
        } else {
            self.address.get().or_else(|| {
                let ctv = self.value(ctx)?;
                let alloca = ctx.builder.build_alloca(ctv.get_type(), "");
                ctx.builder.build_store(alloca, ctv);
                self.address.set(Some(alloca));
                Some(alloca)
            })
        }
    }
    pub fn freeze(self, loc: SourceSpan) -> Value<'src, 'ctx> {
        Value {
            frozen: Some(loc),
            ..self
        }
    }
    pub fn with_loc(self, loc: SourceSpan) -> Value<'src, 'ctx> {
        Value { loc, ..self }
    }

    pub fn value(&self, ctx: &CompCtx<'src, 'ctx>) -> Option<BasicValueEnum<'ctx>> {
        self.comp_val.or_else(|| {
            self.inter_val
                .as_ref()
                .and_then(|v| self.data_type.compiled(v, ctx))
        })
    }

    pub fn ins_dtor(&self, ctx: &CompCtx<'src, 'ctx>) {
        self.data_type.ins_dtor(self, ctx)
    }

    pub fn as_type(&self) -> Option<TypeRef> {
        if let (types::TypeData::KIND, Some(InterData::Type(t))) =
            (self.data_type.kind(), &self.inter_val)
        {
            Some(*t)
        } else {
            None
        }
    }
    pub fn into_type(self, ctx: &CompCtx<'src, 'ctx>) -> Result<TypeRef, CobaltError<'src>> {
        let loc = self.loc;
        self.impl_convert((types::TypeData::new(), Some(loc)), ctx)
            .map(|v| v.as_type().unwrap_or(types::Error::new()))
    }
    pub fn into_mod(
        self,
    ) -> Option<(
        HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        Vec<(CompoundDottedName<'src>, bool)>,
        String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }
    pub fn as_mod(
        &self,
    ) -> Option<(
        &HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &Vec<(CompoundDottedName<'src>, bool)>,
        &String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), &self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }
    pub fn as_mod_mut(
        &mut self,
    ) -> Option<(
        &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &mut Vec<(CompoundDottedName<'src>, bool)>,
        &mut String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), &mut self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }

    pub fn decay(self, ctx: &CompCtx<'src, 'ctx>) -> Value<'src, 'ctx> {
        let ty = self.data_type.decay();
        let name = self.name.clone();
        let loc = self.loc;
        Value {
            name,
            loc,
            ..self.impl_convert((ty, None), ctx).unwrap()
        }
    }
    pub fn attr(
        self,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.attr(self, attr, ctx)
    }
    pub fn impl_convert(
        self,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.data_type == target.0 {
            Ok(self)
        } else if self.data_type._can_iconv_to(target.0, ctx) {
            self.data_type._iconv_to(self, target, ctx)
        } else if target.0._can_iconv_from(self.data_type, ctx) {
            target.0._iconv_from(self, target.1, ctx)
        } else {
            Err(cant_iconv(&self, target.0, target.1))
        }
    }
    pub fn expl_convert(
        self,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.data_type == target.0 {
            Ok(self)
        } else if self.data_type._can_econv_to(target.0, ctx) {
            self.data_type._econv_to(self, target, ctx)
        } else if target.0._can_econv_from(self.data_type, ctx) {
            target.0._econv_from(self, target.1, ctx)
        } else {
            let err = cant_econv(&self, target.0, target.1);
            self.impl_convert(target, ctx).map_err(|_| err)
        }
    }
    pub fn pre_op(
        self,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.pre_op(self, op.0, op.1, ctx, true)
    }
    pub fn post_op(
        self,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.post_op(self, op.0, op.1, ctx, true)
    }
    pub fn bin_op(
        self,
        op: (&'static str, SourceSpan),
        other: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self
            .data_type
            ._has_bin_lhs(other.data_type, op.0, ctx, true, true)
        {
            self.data_type._bin_lhs(self, other, op, ctx, true, true)
        } else if other
            .data_type
            ._has_bin_rhs(self.data_type, op.0, ctx, true, true)
        {
            other.data_type._bin_rhs(self, other, op, ctx, true, true)
        } else {
            Err(invalid_binop(&self, &other, op.0, op.1))
        }
    }
    pub fn call(
        self,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.call(self, cparen, args, ctx)
    }
    pub fn subscript(
        self,
        other: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.subscript(self, other, ctx)
    }
}

#[derive(SerializeState, DeserializeState)]
#[serde(serialize_state = "()")]
#[serde(de_parameters = "'a", deserialize_state = "&'a CompCtx<'src, 'ctx>")]
struct ValueShim<'b, 'src, 'ctx> {
    pub loc: SourceSpan,
    #[serde(borrow)]
    pub comp_val: Option<&'b bstr::BStr>,
    #[serde(deserialize_state)]
    pub inter_val: Option<Cow<'b, InterData<'src, 'ctx>>>,
    pub data_type: TypeRef,
    pub frozen: Option<SourceSpan>,
}
impl<'src, 'ctx> Serialize for Value<'src, 'ctx> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let Value {
            loc,
            comp_val,
            ref inter_val,
            data_type,
            frozen,
            ..
        } = *self;
        ValueShim {
            loc,
            data_type,
            frozen,
            inter_val: inter_val.as_ref().map(Cow::Borrowed),
            comp_val: comp_val.map(|v| unsafe {
                new_lifetime(v.into_pointer_value().get_name().to_bytes().into())
            }),
        }
        .serialize_state(serializer, &())
    }
}
impl<'de, 'a, 'src, 'ctx> DeserializeState<'de, &'a CompCtx<'src, 'ctx>> for Value<'src, 'ctx> {
    fn deserialize_state<D: Deserializer<'de>>(
        seed: &mut &'a CompCtx<'src, 'ctx>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        let ctx = *seed;
        ValueShim::deserialize_state(seed, deserializer).and_then(
            |ValueShim {
                 loc,
                 comp_val,
                 inter_val,
                 data_type,
                 frozen,
             }| {
                use de::Error;
                use inkwell::module::Linkage::DLLImport;
                let mut var = Value {
                    loc,
                    data_type,
                    frozen,
                    name: None,
                    comp_val: None,
                    address: Default::default(),
                    inter_val: inter_val.map(|iv| iv.into_owned()),
                };
                let Some(comp_val) = comp_val else {
                    return Ok(var);
                };
                if let Some(ty) = var
                    .data_type
                    .downcast::<types::Reference>()
                    .and_then(|ty| ty.base().downcast::<types::Function>())
                {
                    let mut good = true;
                    let ps = ty
                        .params()
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
                    if good {
                        if let Some(llt) = ty.ret().llvm_type(ctx) {
                            let ft = llt.fn_type(&ps, false);
                            let fv = ctx.module.add_function(
                                std::str::from_utf8(comp_val).map_err(|_| {
                                    D::Error::invalid_value(
                                        de::Unexpected::Bytes(comp_val),
                                        &"a string",
                                    )
                                })?,
                                ft,
                                None,
                            );
                            if let Some(InterData::Function(FnData { cconv, .. })) = var.inter_val {
                                fv.set_call_conventions(cconv)
                            }
                            let gv = fv.as_global_value();
                            gv.set_linkage(DLLImport);
                            var.comp_val = Some(gv.as_pointer_value().into());
                        } else if ty.ret().size() == SizeType::Static(0) {
                            let ft = ctx.context.void_type().fn_type(&ps, false);
                            let fv = ctx.module.add_function(
                                std::str::from_utf8(comp_val).map_err(|_| {
                                    D::Error::invalid_value(
                                        de::Unexpected::Bytes(comp_val),
                                        &"a string",
                                    )
                                })?,
                                ft,
                                None,
                            );
                            if let Some(InterData::Function(FnData { cconv, .. })) = var.inter_val {
                                fv.set_call_conventions(cconv)
                            }
                            let gv = fv.as_global_value();
                            gv.set_linkage(DLLImport);
                            var.comp_val = Some(gv.as_pointer_value().into());
                        }
                        return Ok(var);
                    }
                } else if let Some(t) = var
                    .data_type
                    .downcast::<types::Reference>()
                    .and_then(|t| t.llvm_type(ctx))
                {
                    let gv = ctx.module.add_global(
                        t,
                        None,
                        std::str::from_utf8(comp_val).map_err(|_| {
                            D::Error::invalid_value(de::Unexpected::Bytes(comp_val), &"a string")
                        })?,
                    );
                    gv.set_linkage(DLLImport);
                    var.comp_val = Some(gv.as_pointer_value().into());
                }
                Ok(var)
            },
        )
    }
}
