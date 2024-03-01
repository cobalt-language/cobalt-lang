use super::*;
use once_cell::sync::Lazy;
use serde::de::DeserializeSeed;
use std::marker::PhantomData;
use std::{cell::Ref, collections::HashMap};

static CUSTOM_INTERN: Interner<Box<str>> = Interner::new();
static CUSTOM_DATA: Lazy<
    flurry::HashMap<&'static str, (TypeRef, bool, flurry::HashMap<Box<str>, usize>, usize)>,
> = Lazy::new(flurry::HashMap::new);

pub type ValueRef<'a, 'src, 'ctx> = Ref<'a, Value<'src, 'ctx>>;

#[derive(Debug, ConstIdentify, Display, RefCastCustom)]
#[repr(transparent)]
pub struct Custom(Box<str>);
impl Custom {
    #[ref_cast_custom]
    #[allow(clippy::borrowed_box)]
    fn from_ref(name: &Box<str>) -> &Self;
    pub fn new(name: impl Into<Box<str>>) -> &'static Self {
        let this = CUSTOM_INTERN.intern(name.into());
        assert!(CUSTOM_DATA.pin().contains_key(&**this));
        Self::from_ref(this)
    }
    pub fn new_ref(name: &str) -> &'static Self {
        let this = CUSTOM_INTERN.intern_ref(name);
        assert!(CUSTOM_DATA.pin().contains_key(&**this));
        Self::from_ref(this)
    }
    pub fn create(name: Box<str>, ctx: &CompCtx) -> &'static Self {
        let this = CUSTOM_INTERN.intern(name);
        let guard = CUSTOM_DATA.guard();
        if !CUSTOM_DATA.contains_key(&**this, &guard) {
            CUSTOM_DATA.insert(
                &**this,
                (
                    types::Null::new(),
                    true,
                    flurry::HashMap::new(),
                    ctx.nom_info.borrow_mut().insert(Default::default()),
                ),
                &guard,
            );
        }
        Self::from_ref(this)
    }
    pub fn create_ref(name: &str, ctx: &CompCtx) -> &'static Self {
        let this = CUSTOM_INTERN.intern_ref(name);
        let guard = CUSTOM_DATA.guard();
        if !CUSTOM_DATA.contains_key(&**this, &guard) {
            CUSTOM_DATA.insert(
                &**this,
                (
                    types::Null::new(),
                    true,
                    flurry::HashMap::new(),
                    ctx.nom_info.borrow_mut().insert(Default::default()),
                ),
                &guard,
            );
        }
        Self::from_ref(this)
    }
    pub fn name(&'static self) -> &'static str {
        &self.0
    }
    pub fn nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> NominalInfo<'ctx> {
        ctx.nom_info.borrow()[CUSTOM_DATA.pin().get(&*self.0).unwrap().3].clone()
    }
    pub fn set_nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>, info: NominalInfo<'ctx>) {
        ctx.nom_info.borrow_mut()[CUSTOM_DATA.pin().get(&*self.0).unwrap().3] = info;
    }
    pub fn base(&self) -> TypeRef {
        CUSTOM_DATA.pin().get(&*self.0).unwrap().0
    }
    pub fn set_base(&self, ty: TypeRef) {
        let guard = CUSTOM_DATA.guard();

        CUSTOM_DATA
            .pin()
            .compute_if_present(&*self.0, |_, (_, e, m, i)| Some((ty, *e, m.clone(), *i)));
    }
    pub fn methods<'a, 'src, 'ctx>(
        &'static self,
        ctx: &'a CompCtx<'src, 'ctx>,
    ) -> HashMap<&'static str, ValueRef<'a, 'src, 'ctx>> {
        let guard = CUSTOM_DATA.guard();
        let methods = &CUSTOM_DATA.get(&*self.0, &guard).unwrap().2;
        let guard = methods.guard();
        methods
            .iter(&guard)
            .map(move |(k, v)| {
                (
                    unsafe { new_lifetime(&**k) },
                    Ref::map(ctx.values.borrow(), |vals| &vals[*v]),
                )
            })
            .collect()
    }
    pub fn set_methods<
        'src,
        'ctx,
        I: IntoIterator<Item = V>,
        V: Into<(K, Value<'src, 'ctx>)>,
        K: Into<Box<str>>,
    >(
        &self,
        metds: I,
        ctx: &CompCtx<'src, 'ctx>,
    ) {
        let mut mb = ctx.values.borrow_mut();
        CUSTOM_DATA
            .pin()
            .compute_if_present(&*self.0, |k, (t, e, m, i)| {
                {
                    let guard = m.guard();
                    for v in m.values(&guard) {
                        mb.remove(*v);
                    }
                }
                Some((
                    *t,
                    *e,
                    metds
                        .into_iter()
                        .map(Into::into)
                        .map(|(k, v)| (k.into(), mb.insert(v)))
                        .collect(),
                    *i,
                ))
            });
    }
}

#[derive(DeserializeState)]
#[serde(de_parameters = "'a")]
#[serde(deserialize_state = "&'a CompCtx<'src, 'ctx>")]
struct CIProxy<'src, 'ctx> {
    base: TypeRef,
    #[serde(deserialize_state)]
    info: NominalInfo<'ctx>,
    #[serde(deserialize_state)]
    fields: std::collections::HashMap<String, Value<'src, 'ctx>>,
}
struct CIProxySeed<'a, 's, 'c>(&'a CompCtx<'s, 'c>);
struct SerCusInfo<'b, 'a, 's, 'c>(
    &'b (TypeRef, bool, flurry::HashMap<Box<str>, usize>, usize),
    &'a CompCtx<'s, 'c>,
);
struct DeCusInfo(TypeRef, bool, flurry::HashMap<Box<str>, usize>, usize);
impl Serialize for SerCusInfo<'_, '_, '_, '_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ser::*;
        let mut map = serializer.serialize_struct("cus_info", 3)?;
        map.serialize_field("base", &self.0 .0)?;
        map.serialize_field("info", self.1.nom_info.borrow().get(self.0 .3).unwrap())?;
        map.serialize_field(
            "fields",
            &self
                .0
                 .2
                .pin()
                .iter()
                .map(|(k, v)| (k, unsafe { new_lifetime(&self.1.values.borrow()[*v]) }))
                .collect::<hashbrown::HashMap<_, _>>(),
        )?;
        map.end()
    }
}
impl<'de> DeserializeSeed<'de> for CIProxySeed<'_, '_, '_> {
    type Value = DeCusInfo;
    fn deserialize<D>(mut self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        CIProxy::deserialize_state(&mut self.0, deserializer).map(
            |CIProxy { base, info, fields }| {
                DeCusInfo(
                    base,
                    false,
                    fields
                        .into_iter()
                        .map(|(k, v)| (k.into_boxed_str(), self.0.values.borrow_mut().insert(v)))
                        .collect(),
                    self.0.nom_info.borrow_mut().insert(info),
                )
            },
        )
    }
}
#[doc(hidden)]
pub struct CustomHeader;
impl Serialize for CustomHeader {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ser::*;
        let ctx = SERIALIZATION_CONTEXT.with(|c| unsafe { get_ctx_ptr(c) });
        let cd = CUSTOM_DATA.pin();
        let mut map = serializer.serialize_map(Some(cd.len()))?;
        cd.iter()
            .filter(|x| x.1 .1)
            .try_for_each(|(k, v)| map.serialize_entry(k, &SerCusInfo(v, ctx)))?;
        map.end()
    }
}
impl<'de> Deserialize<'de> for CustomHeader {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use de::*;
        let ctx = SERIALIZATION_CONTEXT.with(|c| unsafe { get_ctx_ptr(c) });
        struct CHVisitor<'de, 'b, 'a, 's, 'c>(
            &'a CompCtx<'s, 'c>,
            flurry::Guard<'b>,
            PhantomData<&'de ()>,
        );
        impl<'de> Visitor<'de> for CHVisitor<'de, '_, '_, '_, '_> {
            type Value = CustomHeader;
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("the custom types' header")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                while let Some(k) = map.next_key()? {
                    let v = map.next_value_seed(CIProxySeed(self.0))?;
                    assert!(
                        CUSTOM_DATA
                            .insert(CUSTOM_INTERN.intern(k), (v.0, v.1, v.2, v.3), &self.1)
                            .is_none(),
                        "duplicate custom header key"
                    );
                }
                Ok(CustomHeader)
            }
        }
        deserializer.deserialize_map(CHVisitor(ctx, CUSTOM_DATA.guard(), PhantomData))
    }
}
impl TypeSerde for Custom {
    type Header = CustomHeader;
    fn has_header() -> bool {
        !CUSTOM_DATA.is_empty() // may return a false positive but much faster than iterating
    }
    fn get_header() -> Self::Header {
        CustomHeader
    }
    fn set_header(header: Self::Header) {}
    impl_type_proxy!(
        Cow<'static, str>,
        |this: &'static Custom| this.name().into(),
        Self::new
    );
}
impl Type for Custom {
    fn size(&self) -> SizeType {
        self.base().size()
    }
    fn align(&self) -> u16 {
        self.base().align()
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().llvm_type(ctx)
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        let guard = CUSTOM_DATA.guard();
        let keys = CUSTOM_DATA.get(&*self.0, &guard).unwrap();
        let borrow = ctx.nom_info.borrow();
        let info = &borrow[keys.3];
        info.dtor.is_some() || (!info.no_auto_drop && keys.0.has_dtor(ctx))
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        let guard = CUSTOM_DATA.guard();
        let keys = CUSTOM_DATA.get(&*self.0, &guard).unwrap();
        let borrow = ctx.nom_info.borrow();
        let info = &borrow[keys.3];
        if let Some(pv) = val.addr(ctx) {
            if let Some(fv) = info.dtor {
                ctx.builder.build_call(fv, &[pv.into()], "").unwrap();
            } else if !info.no_auto_drop {
                Value {
                    data_type: self.base(),
                    ..val.clone()
                }
                .ins_dtor(ctx)
            }
        }
    }
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        let info = self.nom_info(ctx);
        info.is_linear_type || (!info.no_auto_drop && self.base().is_linear(ctx))
    }
    fn static_attr<'src, 'ctx>(
        &'static self,
        name: &str,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<Value<'src, 'ctx>> {
        let res = CUSTOM_DATA
            .pin()
            .get(&*self.0)
            .and_then(|v| v.2.pin().get(name).map(|x| ctx.values.borrow()[*x].clone()));
        res.or_else(|| {
            self.nom_info(ctx)
                .transparent
                .then(|| self.base().static_attr(name, ctx))
                .flatten()
        })
    }
    fn attr<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base(),
                    loc: remove_unreachable(attr.1)
                        .map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else if self.nom_info(ctx).transparent {
                val.data_type = self.base();
                self.base().attr(val, attr, ctx)
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            };
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1));
        };
        assert!(field
            .data_type
            .is_and::<types::Reference>(|r| r.base().is::<types::Function>()));
        let fty = field
            .data_type
            .downcast::<types::Reference>()
            .unwrap()
            .base()
            .downcast::<types::Function>()
            .unwrap();
        match mt {
            MethodType::Getter => field.call(None, vec![val], ctx),
            MethodType::Method => {
                let (self_t, sic) = *fty
                    .params()
                    .first()
                    .ok_or_else(|| invalid_attr(&val, attr.0, attr.1))?;
                let this = val.impl_convert((self_t, Some(attr.1)), ctx)?;
                Ok(Value::new(
                    self_t
                        .llvm_type(ctx)
                        .map(|llt| this.value(ctx).unwrap_or_else(|| llt.const_zero())),
                    Some(InterData::Array(vec![
                        this.inter_val.unwrap_or(InterData::Null),
                        field.inter_val.unwrap(),
                    ])),
                    types::BoundMethod::new(fty.ret(), fty.params()),
                ))
            }
            MethodType::Static => Err(invalid_attr(&val, attr.0, attr.1)),
        }
    }
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        let Some(field) = self.static_attr(attr, ctx) else {
            return attr == "__base"
                || (self.nom_info(ctx).transparent && self.base()._has_mut_attr(attr, ctx));
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return false;
        };
        assert!(field
            .data_type
            .is_and::<types::Reference>(|r| r.base().is::<types::Function>()));
        let fty = field
            .data_type
            .downcast::<types::Reference>()
            .unwrap()
            .base()
            .downcast::<types::Function>()
            .unwrap();
        mt != MethodType::Static
            && !fty.params().is_empty()
            && self.add_ref(false).impl_convertible(fty.params()[0].0, ctx)
    }
    fn _has_refmut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        let Some(field) = self.static_attr(attr, ctx) else {
            return attr == "__base"
                || (self.nom_info(ctx).transparent && self.base()._has_mut_attr(attr, ctx));
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return false;
        };
        assert!(field
            .data_type
            .is_and::<types::Reference>(|r| r.base().is::<types::Function>()));
        let fty = field
            .data_type
            .downcast::<types::Reference>()
            .unwrap()
            .base()
            .downcast::<types::Function>()
            .unwrap();
        mt != MethodType::Static
            && !fty.params().is_empty()
            && self.add_ref(true).impl_convertible(fty.params()[0].0, ctx)
    }
    fn _has_mut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        attr == "__base" || (self.nom_info(ctx).transparent && self.base()._has_mut_attr(attr, ctx))
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base().add_ref(false),
                    loc: remove_unreachable(attr.1)
                        .map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else if self.nom_info(ctx).transparent {
                val.data_type = self.base();
                self.base().attr(val, attr, ctx)
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            };
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1));
        };
        assert!(field
            .data_type
            .is_and::<types::Reference>(|r| r.base().is::<types::Function>()));
        let fty = field
            .data_type
            .downcast::<types::Reference>()
            .unwrap()
            .base()
            .downcast::<types::Function>()
            .unwrap();
        match mt {
            MethodType::Getter => field.call(None, vec![val], ctx),
            MethodType::Method => {
                let (self_t, sic) = *fty
                    .params()
                    .first()
                    .ok_or_else(|| invalid_attr(&val, attr.0, attr.1))?;
                let this = val.impl_convert((self_t, Some(attr.1)), ctx)?;
                Ok(Value::new(
                    self_t
                        .llvm_type(ctx)
                        .map(|llt| this.value(ctx).unwrap_or_else(|| llt.const_zero())),
                    Some(InterData::Array(vec![
                        this.inter_val.unwrap_or(InterData::Null),
                        field.inter_val.unwrap(),
                    ])),
                    types::BoundMethod::new(fty.ret(), fty.params()),
                ))
            }
            MethodType::Static => Err(invalid_attr(&val, attr.0, attr.1)),
        }
    }
    fn _refmut_attr<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base().add_ref(true),
                    loc: remove_unreachable(attr.1)
                        .map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else if self.nom_info(ctx).transparent {
                val.data_type = self.base();
                self.base().attr(val, attr, ctx)
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            };
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1));
        };
        assert!(field
            .data_type
            .is_and::<types::Reference>(|r| r.base().is::<types::Function>()));
        let fty = field
            .data_type
            .downcast::<types::Reference>()
            .unwrap()
            .base()
            .downcast::<types::Function>()
            .unwrap();
        match mt {
            MethodType::Getter => field.call(None, vec![val], ctx),
            MethodType::Method => {
                let (self_t, sic) = *fty
                    .params()
                    .first()
                    .ok_or_else(|| invalid_attr(&val, attr.0, attr.1))?;
                let this = val.impl_convert((self_t, Some(attr.1)), ctx)?;
                Ok(Value::new(
                    self_t
                        .llvm_type(ctx)
                        .map(|llt| this.value(ctx).unwrap_or_else(|| llt.const_zero())),
                    Some(InterData::Array(vec![
                        this.inter_val.unwrap_or(InterData::Null),
                        field.inter_val.unwrap(),
                    ])),
                    types::BoundMethod::new(fty.ret(), fty.params()),
                ))
            }
            MethodType::Static => Err(invalid_attr(&val, attr.0, attr.1)),
        }
    }
    fn _mut_attr<'src, 'ctx>(
        &'static self,
        mut val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if attr.0 == "__base" {
            val.data_type = types::Mut::new(self.base());
            Ok(val)
        } else {
            self.attr(val, attr, ctx)
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct NominalInfo<'ctx> {
    pub dtor: Option<FunctionValue<'ctx>>,
    pub no_auto_drop: bool,
    pub is_linear_type: bool,
    pub transparent: bool,
}

#[derive(Debug, Clone, Default, SerializeState, DeserializeState)]
#[serde(serialize_state = "()")]
#[serde(
    de_parameters = "'a, 'b, 'c",
    deserialize_state = "&'a CompCtx<'b, 'c>"
)]
pub struct NominalInfoShim<'d> {
    #[serde(borrow)]
    pub dtor: Option<&'d bstr::BStr>,
    pub no_auto_drop: bool,
    pub is_linear_type: bool,
    pub transparent: bool,
}
impl Serialize for NominalInfo<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let NominalInfo {
            dtor,
            no_auto_drop,
            is_linear_type,
            transparent,
        } = *self;
        NominalInfoShim {
            no_auto_drop,
            is_linear_type,
            transparent,
            dtor: dtor.map(|d| unsafe { new_lifetime(d.get_name().to_bytes().into()) }),
        }
        .serialize_state(serializer, &())
    }
}
impl<'de, 'ctx> DeserializeState<'de, &CompCtx<'_, 'ctx>> for NominalInfo<'ctx> {
    fn deserialize_state<D>(
        seed: &mut &CompCtx<'_, 'ctx>,
        deserializer: D,
    ) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use de::*;
        NominalInfoShim::deserialize_state(seed, deserializer).and_then(
            |NominalInfoShim {
                 dtor,
                 no_auto_drop,
                 is_linear_type,
                 transparent,
             }| {
                Ok(NominalInfo {
                    no_auto_drop,
                    is_linear_type,
                    transparent,
                    dtor: dtor
                        .map(|n| {
                            std::str::from_utf8(n)
                                .map_err(|_| {
                                    D::Error::invalid_value(
                                        Unexpected::Bytes(n),
                                        &"a valid UTF-8 string",
                                    )
                                })
                                .map(|name| {
                                    seed.module.add_function(
                                        name,
                                        seed.context.void_type().fn_type(
                                            &[seed
                                                .context
                                                .i8_type()
                                                .ptr_type(Default::default())
                                                .into()],
                                            false,
                                        ),
                                        Some(inkwell::module::Linkage::DLLImport),
                                    )
                                })
                        })
                        .transpose()?,
                })
            },
        )
    }
}

submit_types!(Custom);
