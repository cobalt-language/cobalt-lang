use crate::*;
use inkwell::values::FunctionValue;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;
use std::io::{self, BufRead, Read, Write};
use std::num::NonZeroU64;
use std::ptr::addr_of;
use SizeType::*;
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum SizeType {
    Static(u32),
    Dynamic,
    Meta,
}
impl SizeType {
    pub fn is_static(self) -> bool {
        matches!(self, Static(_))
    }
    pub fn is_dynamic(self) -> bool {
        self == Dynamic
    }
    pub fn is_meta(self) -> bool {
        self == Meta
    }
    pub fn as_static(self) -> Option<u32> {
        if let Static(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn map_static<F: FnOnce(u32) -> u32>(self, f: F) -> SizeType {
        if let Static(x) = self {
            Static(f(x))
        } else {
            self
        }
    }
}
impl Display for SizeType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            SizeType::Static(size) => write!(f, "{size}"),
            SizeType::Dynamic => write!(f, "dynamic"),
            SizeType::Meta => write!(f, "meta"),
        }
    }
}
impl std::ops::Add for SizeType {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (SizeType::Meta, _) | (_, SizeType::Meta) => SizeType::Meta,
            (SizeType::Dynamic, _) | (_, SizeType::Dynamic) => SizeType::Dynamic,
            (SizeType::Static(l), SizeType::Static(r)) => SizeType::Static(l + r),
        }
    }
}
pub trait Type: Debug + Display + Send + Sync {
    fn kind() -> NonZeroU64
    where
        Self: Sized;
    fn self_kind(&self) -> NonZeroU64 {
        Self::self_kind(&self)
    }
    fn size(&self) -> SizeType;
    fn align(&self) -> u16;
    fn nom_info(&self) -> Option<NominalInfo> {
        None
    }
    fn has_dtor(&self, ctx: &CompCtx) -> bool {
        #![allow(unused_variables)]
        false
    }
    fn save_header(out: &mut dyn Write) -> io::Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }
    fn load_header(buf: &mut dyn BufRead) -> io::Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }
    fn save(&self, out: &mut dyn Write) -> io::Result<()>;
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef>
    where
        Self: Sized;
    unsafe fn downcast_impl(val: &dyn Type) -> &Self
    where
        Self: Sized,
    {
        *std::mem::transmute::<&&dyn Type, &&Self>(&val)
    }
}
impl dyn Type {
    fn downcast<T: Type>(&self) -> Option<&T> {
        if self.self_kind() == T::kind() {
            Some(unsafe { T::downcast_impl(self) })
        } else {
            None
        }
    }
    unsafe fn downcast_unchecked<T: Type>(&self) -> &T {
        T::downcast_impl(self)
    }
}
impl PartialEq for dyn Type {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
impl Eq for dyn Type {}
impl Hash for dyn Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(addr_of!(self) as _)
    }
}
pub type TypeRef = &'static dyn Type;
pub fn save_type(buf: &mut dyn Write, ty: TypeRef) -> io::Result<()> {
    buf.write_all(&ty.self_kind().get().to_be_bytes())?;
    ty.save(buf)
}
pub fn load_type(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
    todo!()
}
#[derive(Debug, Clone, Default)]
pub struct NominalInfo<'ctx> {
    pub dtor: Option<FunctionValue<'ctx>>,
    pub no_auto_drop: bool,
    pub is_linear_type: bool,
    pub transparent: bool,
}

impl<'ctx> NominalInfo<'ctx> {
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        if let Some(fv) = self.dtor {
            out.write_all(fv.get_name().to_bytes())?;
        }
        out.write_all(&[0])?;
        out.write_all(&[u8::from(self.no_auto_drop) << 1 | u8::from(self.transparent)])
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'_, 'ctx>) -> io::Result<Self> {
        let mut vec = vec![];
        buf.read_until(0, &mut vec)?;
        if vec.last() == Some(&0) {
            vec.pop();
        }
        let dtor = if vec.is_empty() {
            None
        } else {
            let name = String::from_utf8(vec).expect("value should be valid UTF-8!");
            let fv = ctx.module.get_function(&name);
            use inkwell::types::BasicType;
            Some(fv.unwrap_or_else(|| {
                ctx.module.add_function(
                    &name,
                    ctx.context
                        .void_type()
                        .fn_type(&[ctx.null_type.ptr_type(Default::default()).into()], false),
                    None,
                )
            }))
        };
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        let no_auto_drop = c & 1 != 0;
        let transparent = c & 2 != 0;
        Ok(Self {
            dtor,
            no_auto_drop,
            transparent,
            is_linear_type: false,
        })
    }
}

#[inline(always)]
fn pad_bytes<const N: usize>(val: &[u8]) -> [u8; N] {
    let len = std::cmp::min(val.len(), N);
    let mut out = [0u8; N];
    out[..len].copy_from_slice(&val[..len]);
    out
}
/// generate a type id from a byte string
/// truncate or zero pad to 8 bytes
#[inline(always)]
pub fn make_id(id: impl AsRef<[u8]>) -> NonZeroU64 {
    NonZeroU64::new(u64::from_be_bytes(pad_bytes(id.as_ref())))
        .expect("ID string should not be empty")
}

pub mod agg;
pub mod custom;
pub mod float;
pub mod int;
pub mod mem;
pub mod meta;
pub use agg::*;
pub use custom::*;
pub use float::*;
pub use int::*;
pub use mem::*;
pub use meta::*;

use ref_cast::*;
