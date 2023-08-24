use super::*;
#[derive(Debug, RefCastCustom)]
#[repr(transparent)]
pub struct Function((TypeRef, Box<[(TypeRef, bool)]>));
impl Function {
    pub const KIND: NonZeroU64 = make_id(b"function");
    #[ref_cast_custom]
    #[inline(always)]
    fn from_ref(base: &(TypeRef, Box<[(TypeRef, bool)]>)) -> &Self;
    pub fn new(ret: TypeRef, params: impl Into<Box<[(TypeRef, bool)]>>) -> &'static Self {
        static INTERN: Interner<(TypeRef, Box<[(TypeRef, bool)]>)> = Interner::new();
        Self::from_ref(INTERN.intern((ret, params.into())))
    }

    pub fn ret(&self) -> TypeRef {
        self.0 .0
    }
    pub fn params(&self) -> &[(TypeRef, bool)] {
        &self.0 .1
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fn (")?;
        let mut rem = self.params().len();
        for &(ty, c) in self.params() {
            if c {
                f.write_str("const ")?;
            }
            Display::fmt(ty, f)?;
            rem -= 1;
            if rem > 0 {
                f.write_str(", ")?;
            }
        }
        write!(f, "): {}", self.ret())
    }
}
impl Type for Function {
    fn kind() -> NonZeroU64 {
        Self::KIND
    }
    fn size(&self) -> SizeType {
        SizeType::Meta
    }
    fn align(&self) -> u16 {
        0
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()> {
        save_type(out, self.ret())?;
        self.params().iter().try_for_each(|&(ty, c)| {
            save_type(out, ty)?;
            out.write_all(&[u8::from(c)])
        })
    }
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
        let ret = load_type(buf)?;
        let params = std::iter::from_fn(|| match load_type_opt(buf) {
            Ok(None) => None,
            Ok(Some(t)) => {
                let mut c = 0u8;
                if let Err(err) = buf.read_exact(std::slice::from_mut(&mut c)) {
                    return Some(Err(err));
                }
                Some(Ok((t, c != 0)))
            }
            Err(err) => Some(Err(err)),
        })
        .collect::<Result<Vec<_>, _>>()?;
        Ok(Self::new(ret, params))
    }
}
