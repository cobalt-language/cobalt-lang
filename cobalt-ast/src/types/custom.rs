use super::*;
static CUSTOM_INTERN: Interner<Box<str>> = Interner::new();
static CUSTOM_DATA: dashmap::DashMap<
    Box<str>,
    (TypeRef, bool, dashmap::DashMap<Box<str>, usize>, usize),
> = Default::default();
#[derive(Debug, Display, RefCastCustom)]
#[repr(transparent)]
pub struct Custom(Box<str>);
impl Custom {
    #[ref_cast_custom]
    fn from_ref(types: &Box<str>) -> &Self;
    pub fn new(types: Box<str>) -> &'static Self {
        Self::from_ref(CUSTOM_INTERN.intern(types))
    }
    pub fn new_ref(types: &str) -> &'static Self {
        Self::from_ref(CUSTOM_INTERN.intern_ref(types))
    }
}
impl Type for Custom {
    fn kind() -> NonZeroU64
    where
        Self: Sized,
    {
        make_id("custom")
    }
    fn size(&self) -> SizeType {
        CUSTOM_DATA.get(&*self.0).unwrap().0.size()
    }
    fn align(&self) -> u16 {
        CUSTOM_DATA.get(&*self.0).unwrap().0.align()
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        let keys = CUSTOM_DATA.get(&*self.0).unwrap();
        let borrow = ctx.nom_info.borrow();
        let info = &borrow[keys.3];
        info.dtor.is_some() || (!info.is_linear_type && keys.0.has_dtor(ctx))
    }
    fn save_header(out: &mut dyn Write) -> io::Result<()>
    where
        Self: Sized,
    {
        for vals in &CUSTOM_DATA {
            let (ty, _export, methods, info) = &*vals;
            out.write_all(vals.key().as_bytes())?;
            out.write_all(&[0])?;
            save_type(out, *ty)?;
            for kv in methods {
                serial_utils::save_str(out, &kv.key())?;
                out.write_all(&info.to_be_bytes())?;
            }
            out.write_all(&[0])?;
            out.write_all(&info.to_be_bytes())?;
        }
        out.write_all(&[0])?;
        Ok(())
    }
    fn load_header(buf: &mut dyn BufRead) -> io::Result<()>
    where
        Self: Sized,
    {
        loop {
            let mut key = serial_utils::load_str(buf)?;
            if key.is_empty() {
                break;
            }
            let ty = load_type(buf)?;
            let mut methods = dashmap::DashMap::new();
            let mut arr = [0; std::mem::size_of::<usize>()];
            loop {
                let mut metd = serial_utils::load_str(buf)?;
                if metd.is_empty() {
                    break;
                }
                buf.read_exact(&mut arr)?;
                methods.insert(metd.into(), usize::from_be_bytes(arr));
            }
            buf.read_exact(&mut arr)?;
            CUSTOM_DATA.insert(key.into(), (ty, false, methods, usize::from_be_bytes(arr)));
        }
        Ok(())
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        out.write_all(self.0.as_bytes())?;
        out.write_all(&[0])
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef>
    where
        Self: Sized,
    {
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
