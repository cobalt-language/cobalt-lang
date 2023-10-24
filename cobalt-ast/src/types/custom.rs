use super::*;
use once_cell::sync::Lazy;
use std::{cell::Ref, collections::HashMap};
static CUSTOM_INTERN: Interner<Box<str>> = Interner::new();
static CUSTOM_DATA: Lazy<
    flurry::HashMap<&'static str, (TypeRef, bool, flurry::HashMap<Box<str>, usize>, usize)>,
> = Lazy::new(flurry::HashMap::new);
pub type ValueRef<'a, 'src, 'ctx> = Ref<'a, Value<'src, 'ctx>>;
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
pub struct Custom(Box<str>);
impl Custom {
    #[ref_cast_custom]
    #[allow(clippy::borrowed_box)]
    fn from_ref(name: &Box<str>) -> &Self;
    pub fn new(name: Box<str>) -> &'static Self {
        let this = CUSTOM_INTERN.intern(name);
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
impl ConcreteType for Custom {
    const KIND: NonZeroU64 = make_id(b"custom");
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
                ctx.builder.build_call(fv, &[pv.into()], "");
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
                    .get(0)
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
                    .get(0)
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
                    .get(0)
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
    fn save_header(out: &mut dyn Write) -> io::Result<()> {
        let guard = CUSTOM_DATA.guard();
        for (key, (ty, _export, methods, info)) in CUSTOM_DATA.iter(&guard) {
            out.write_all(key.as_bytes())?;
            out.write_all(&[0])?;
            save_type(out, *ty)?;
            let guard = methods.guard();
            for (k, v) in methods.iter(&guard) {
                serial_utils::save_str(out, k)?;
                out.write_all(&v.to_be_bytes())?;
            }
            out.write_all(&[0])?;
            out.write_all(&info.to_be_bytes())?;
        }
        out.write_all(&[0])?;
        Ok(())
    }
    fn load_header(buf: &mut dyn BufRead) -> io::Result<()> {
        let guard = CUSTOM_DATA.guard();
        loop {
            let key = serial_utils::load_str(buf)?;
            if key.is_empty() {
                break;
            }
            let ty = load_type(buf)?;
            let mut arr = [0; std::mem::size_of::<usize>()];
            let methods = {
                let methods = flurry::HashMap::new();
                let guard = methods.guard();
                loop {
                    let metd = serial_utils::load_str(buf)?;
                    if metd.is_empty() {
                        break;
                    }
                    buf.read_exact(&mut arr)?;
                    methods.insert(metd.into(), usize::from_be_bytes(arr), &guard);
                }
                std::mem::drop(guard);
                methods
            };
            buf.read_exact(&mut arr)?;
            CUSTOM_DATA.insert(
                CUSTOM_INTERN.intern(key.into()),
                (ty, false, methods, usize::from_be_bytes(arr)),
                &guard,
            );
        }
        Ok(())
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(self.0.as_bytes())?;
        out.write_all(&[0])
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let mut vec = vec![];
        buf.read_until(0, &mut vec)?;
        if vec.last() == Some(&0) {
            vec.pop();
        }
        Ok(Self::new(
            String::from_utf8(vec)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
                .into(),
        ))
    }
}
submit_types!(Custom);
