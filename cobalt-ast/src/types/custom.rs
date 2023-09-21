use super::*;
use dashmap::DashMap;
use once_cell::sync::Lazy;
use std::{cell::Ref, collections::HashMap};
static CUSTOM_INTERN: Interner<Box<str>> = Interner::new();
static CUSTOM_DATA: Lazy<DashMap<&'static str, (TypeRef, bool, DashMap<Box<str>, usize>, usize)>> =
    Lazy::new(DashMap::new);
pub type ValueRef<'a, 'src, 'ctx> = Ref<'a, Value<'src, 'ctx>>;
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
pub struct Custom(Box<str>);
impl Custom {
    pub const KIND: NonZeroU64 = make_id(b"custom");
    #[ref_cast_custom]
    #[allow(clippy::borrowed_box)]
    fn from_ref(name: &Box<str>) -> &Self;
    pub fn new(name: Box<str>) -> &'static Self {
        let this = CUSTOM_INTERN.intern(name);
        assert!(CUSTOM_DATA.contains_key(&**this));

        Self::from_ref(this)
    }
    pub fn new_ref(name: &str) -> &'static Self {
        let this = CUSTOM_INTERN.intern_ref(name);
        assert!(CUSTOM_DATA.contains_key(&**this));
        Self::from_ref(this)
    }
    pub fn create(name: Box<str>, ctx: &CompCtx) -> &'static Self {
        let this = CUSTOM_INTERN.intern(name);
        if !CUSTOM_DATA.contains_key(&**this) {
            CUSTOM_DATA.insert(
                &**this,
                (
                    types::Null::new(),
                    true,
                    DashMap::new(),
                    ctx.nom_info.borrow_mut().insert(Default::default()),
                ),
            );
        }
        Self::from_ref(this)
    }
    pub fn create_ref(name: &str, ctx: &CompCtx) -> &'static Self {
        let this = CUSTOM_INTERN.intern_ref(name);
        if !CUSTOM_DATA.contains_key(&**this) {
            CUSTOM_DATA.insert(
                &**this,
                (
                    types::Null::new(),
                    true,
                    DashMap::new(),
                    ctx.nom_info.borrow_mut().insert(Default::default()),
                ),
            );
        }
        Self::from_ref(this)
    }
    pub fn base(&self) -> TypeRef {
        CUSTOM_DATA.get(&*self.0).unwrap().0
    }
    pub fn set_base(&self, ty: TypeRef) {
        CUSTOM_DATA.get_mut(&*self.0).unwrap().0 = ty;
    }
    pub fn methods<'a, 'src, 'ctx>(
        &'static self,
        ctx: &'a CompCtx<'src, 'ctx>,
    ) -> HashMap<&'static str, ValueRef<'a, 'src, 'ctx>> {
        CUSTOM_DATA
            .get(&*self.0)
            .unwrap()
            .2
            .iter()
            .map(move |v| {
                (
                    unsafe { std::mem::transmute(&**v.key()) },
                    Ref::map(ctx.values.borrow(), |vals| &vals[*v.value()]),
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
        let mut this_data = CUSTOM_DATA.get_mut(&*self.0).unwrap();
        for kv in std::mem::take(&mut this_data.2) {
            mb.remove(kv.1);
        }
        metds
            .into_iter()
            .map(Into::into)
            .map(|(k, v)| (k.into(), v))
            .for_each(|(k, v)| {
                this_data.2.insert(k, mb.insert(v));
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
    fn nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<NominalInfo<'ctx>> {
        Some(ctx.nom_info.borrow()[CUSTOM_DATA.get(&*self.0).unwrap().3].clone())
    }
    fn set_nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>, info: NominalInfo<'ctx>) -> bool {
        ctx.nom_info.borrow_mut()[CUSTOM_DATA.get(&*self.0).unwrap().3] = info;
        true
    }
    fn llvm_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().llvm_type(ctx)
    }
    fn ptr_type<'ctx>(&self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        self.base().ptr_type(ctx)
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        let keys = CUSTOM_DATA.get(&*self.0).unwrap();
        let borrow = ctx.nom_info.borrow();
        let info = &borrow[keys.3];
        info.dtor.is_some() || (!info.no_auto_drop && keys.0.has_dtor(ctx))
    }
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {
        let keys = CUSTOM_DATA.get(&*self.0).unwrap();
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
    fn static_attr<'src, 'ctx>(
        &'static self,
        name: &str,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<Value<'src, 'ctx>> {
        println!(
            "{name:?} of {:#?}",
            CUSTOM_DATA.get(&*self.0).map_or_else(Vec::new, |v| v
                .2
                .iter()
                .map(|x| bstr::BString::from(x.key().as_ref()))
                .collect())
        ); // debug
        let res = CUSTOM_DATA
            .get(&*self.0)
            .and_then(|v| v.2.get(name).map(|x| ctx.values.borrow()[*x].clone()));
        res
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base(),
                    loc: remove_unreachable(attr.1).map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            }
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1))
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
            MethodType::Normal => {
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
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base().add_ref(false),
                    loc: remove_unreachable(attr.1).map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            }
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1))
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
            MethodType::Normal => {
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
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        let Some(field) = self.static_attr(&attr.0, ctx) else {
            return if attr.0 == "__base" {
                Ok(Value {
                    data_type: self.base().add_ref(true),
                    loc: remove_unreachable(attr.1).map_or(val.loc, |loc| merge_spans(val.loc, loc)),
                    ..val
                })
            } else {
                Err(invalid_attr(&val, attr.0, attr.1))
            }
        };
        let Some(InterData::Function(FnData { mt, .. })) = field.inter_val else {
            return Err(invalid_attr(&val, attr.0, attr.1))
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
            MethodType::Normal => {
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
    fn save_header(out: &mut dyn Write) -> io::Result<()> {
        for vals in CUSTOM_DATA.iter() {
            let (ty, _export, methods, info) = &*vals;
            out.write_all(vals.key().as_bytes())?;
            out.write_all(&[0])?;
            save_type(out, *ty)?;
            for kv in methods {
                serial_utils::save_str(out, kv.key())?;
                out.write_all(&info.to_be_bytes())?;
            }
            out.write_all(&[0])?;
            out.write_all(&info.to_be_bytes())?;
        }
        out.write_all(&[0])?;
        Ok(())
    }
    fn load_header(buf: &mut dyn BufRead) -> io::Result<()> {
        loop {
            let key = serial_utils::load_str(buf)?;
            if key.is_empty() {
                break;
            }
            let ty = load_type(buf)?;
            let methods = DashMap::new();
            let mut arr = [0; std::mem::size_of::<usize>()];
            loop {
                let metd = serial_utils::load_str(buf)?;
                if metd.is_empty() {
                    break;
                }
                buf.read_exact(&mut arr)?;
                methods.insert(metd.into(), usize::from_be_bytes(arr));
            }
            buf.read_exact(&mut arr)?;
            CUSTOM_DATA.insert(
                CUSTOM_INTERN.intern(key.into()),
                (ty, false, methods, usize::from_be_bytes(arr)),
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
