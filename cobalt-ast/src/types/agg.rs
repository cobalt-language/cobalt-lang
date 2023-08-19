use super::*;
use std::collections::BTreeMap;

fn tuple_size(types: &[TypeRef]) -> SizeType {
    let mut out = 0;
    let mut overall_align = 1;
    for ty in types {
        let size = ty.size();
        if let SizeType::Static(size) = size {
            let align = ty.align() as u32;
            out += align - 1;
            out /= align;
            out += size;
            if align > overall_align {
                overall_align = align
            }
        } else {
            return size;
        }
    }
    out += overall_align - 1;
    out /= overall_align;
    SizeType::Static(out)
}

static TUPLE_INTERN: Interner<Box<[TypeRef]>> = Interner::new();
#[derive(Debug, Display, RefCastCustom)]
#[display(
    fmt = "({})",
    r#"_0.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")"#
)]
#[repr(transparent)]
pub struct Tuple(Box<[TypeRef]>);
impl Tuple {
    #[ref_cast_custom]
    fn from_ref(types: &Box<[TypeRef]>) -> &Self;
    pub fn new(types: Box<[TypeRef]>) -> &'static Self {
        Self::from_ref(TUPLE_INTERN.intern(types))
    }
    pub fn new_ref(types: &[TypeRef]) -> &'static Self {
        Self::from_ref(TUPLE_INTERN.intern_ref(types))
    }
}
impl Type for Tuple {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("tuple")
    }
    fn align(&self) -> u16 {
        self.0.iter().map(|v| v.align()).max().unwrap_or(1)
    }
    fn size(&self) -> SizeType {
        tuple_size(&self.0)
    }
}

#[derive(Debug, RefCastCustom)]
#[repr(transparent)]
pub struct Struct((Box<[TypeRef]>, BTreeMap<Box<str>, usize>));
impl Struct {
    #[ref_cast_custom]
    fn from_ref(inner: &(Box<[TypeRef]>, BTreeMap<Box<str>, usize>)) -> &Self;
    pub fn new(fields: impl IntoIterator<Item = (impl Into<Box<str>>, TypeRef)>) -> &'static Self {
        let mut vec = fields
            .into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect::<Vec<_>>();
        use std::cmp::Ordering;
        vec.sort_by(|(ln, lt), (rn, rt)| match lt.align().cmp(&rt.align()) {
            Ordering::Less => Ordering::Greater,
            Ordering::Greater => Ordering::Less,
            Ordering::Equal => match (lt.size(), rt.size()) {
                (SizeType::Meta, _)
                | (_, SizeType::Meta)
                | (SizeType::Dynamic, SizeType::Dynamic) => ln.cmp(rn),
                (SizeType::Static(ls), SizeType::Static(rs)) => ls.cmp(&rs),
                (SizeType::Static(_), SizeType::Dynamic) => Ordering::Less,
                (SizeType::Dynamic, SizeType::Static(_)) => Ordering::Greater,
            },
        });
        let (types, fields): (Vec<_>, _) = vec
            .into_iter()
            .enumerate()
            .map(|(n, (name, ty))| (ty, (name, n)))
            .unzip();
        static INTERN: Interner<(Box<[TypeRef]>, BTreeMap<Box<str>, usize>)> = Interner::new();
        Self::from_ref(INTERN.intern((types.into(), fields)))
    }
}
impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut rev_lookup = ["<error>"].repeat(self.0 .0.len());
        for (name, idx) in self.0 .1 {
            rev_lookup[idx] = &name;
        }
        f.write_str("{")?;
        let mut rem = self.0 .0.len();
        for (n, ty) in self.0 .0.iter().enumerate() {
            write!(f, "{}: {ty}", rev_lookup[n])?;
            rem -= 1;
            if rem > 0 {
                f.write_str(", ")?;
            }
        }
        f.write_str("}")
    }
}
impl Type for Struct {
    fn kind() -> usize
    where
        Self: Sized,
    {
        make_id("struct")
    }
    fn align(&self) -> u16 {
        self.0 .0.iter().map(|v| v.align()).max().unwrap_or(1)
    }
    fn size(&self) -> SizeType {
        tuple_size(&self.0 .0)
    }
}
