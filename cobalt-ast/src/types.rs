#![allow(unused_variables)]
use crate::*;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
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
pub trait AsTypeRef {
    fn as_type_ref(&'static self) -> TypeRef;
}
impl<T: Type + Sized> AsTypeRef for T {
    fn as_type_ref(&'static self) -> TypeRef {
        self
    }
}
pub trait Type: AsTypeRef + Debug + Display + Send + Sync {
    fn kind() -> NonZeroU64
    where
        Self: Sized;
    fn self_kind(&self) -> NonZeroU64 {
        todo!()
    }
    fn size(&'static self) -> SizeType;
    fn align(&'static self) -> u16;
    fn llvm_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        None
    }
    fn ptr_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        (self.size() != SizeType::Meta).then(|| ctx.null_type.ptr_type(Default::default()).into())
    }

    fn nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<NominalInfo<'ctx>> {
        None
    }
    fn set_nom_info<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>, info: NominalInfo<'ctx>) -> bool {
        false
    }
    fn has_dtor(&'static self, ctx: &CompCtx) -> bool {
        false
    }
    fn ins_dtor<'ctx>(
        &'static self,
        comp_val: Option<BasicValueEnum<'ctx>>,
        ctx: &CompCtx<'_, 'ctx>,
    ) {
    }
    fn decay(&'static self) -> TypeRef {
        self.as_type_ref()
    }

    fn static_attr<'src, 'ctx>(
        &'static self,
        name: &str,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<Value<'src, 'ctx>> {
        None
    }
    fn attr<'src, 'ctx>(
        &'static self,
        val: &Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(CobaltError::AttrNotDefined {
            val: self.to_string(),
            attr: attr.0,
            vloc: val.loc,
            aloc: attr.1,
        })
    }
    fn impl_convertible(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self._can_iconv_to(other, ctx) || other._can_iconv_from(self.as_type_ref(), ctx)
    }
    fn expl_convertible(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self._can_econv_to(other, ctx)
            || other._can_econv_from(self.as_type_ref(), ctx)
            || self.impl_convertible(other, ctx)
    }
    fn common(&'static self, other: TypeRef, ctx: &CompCtx) -> Option<TypeRef> {
        let this = self.as_type_ref();
        if this.impl_convertible(other, ctx) {
            Some(other)
        } else if other.impl_convertible(this, ctx) {
            Some(this)
        } else {
            let ty = types::TypeData::new();
            (this.impl_convertible(ty, ctx) && other.impl_convertible(ty, ctx)).then_some(ty)
        }
    }
    fn pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_preop(&val, op, oloc))
    }
    fn post_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_postop(&val, op, oloc))
    }
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_call(&val, cparen, args.iter()))
    }
    fn subscript<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        idx: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        idx.data_type
            .is::<types::Error>()
            .then(Value::error)
            .ok_or_else(|| invalid_sub(&val, &idx))
    }

    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_iconv(&val, target.0, target.1))
    }
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, self.as_type_ref(), target))
    }
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, target.0, target.1))
    }
    fn _econv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, self.as_type_ref(), target))
    }
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_econv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_bin_lhs(rhs.data_type.as_type_ref(), op.0, ctx));
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_bin_rhs(lhs.data_type.as_type_ref(), op.0, ctx));
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    fn _has_bin_lhs(&self, other: TypeRef, op: &'static str, ctx: &CompCtx) -> bool {
        false
    }
    fn _has_bin_rhs(&self, other: TypeRef, op: &'static str, ctx: &CompCtx) -> bool {
        false
    }

    fn compiled<'src, 'ctx>(
        &'static self,
        inter_val: &InterData<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        None
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
    fn save(&'static self, out: &mut dyn Write) -> io::Result<()>;
    fn load(buf: &mut dyn BufRead) -> io::Result<TypeRef>
    where
        Self: Sized;
    /// Downcast from a type
    /// # Safety
    /// The casted from type must be the same as Self
    unsafe fn downcast_impl(val: &dyn Type) -> &Self
    where
        Self: Sized,
    {
        std::mem::transmute::<&&dyn Type, &&Self>(&val)
    }

    fn add_ref(&'static self, is_mut: bool) -> TypeRef {
        types::Reference::new(if is_mut {
            types::Mut::new(self.as_type_ref())
        } else {
            self.as_type_ref()
        })
    }
}
impl dyn Type {
    pub fn is<T: Type>(&self) -> bool {
        self.self_kind() == T::kind()
    }
    pub fn downcast<T: Type>(&self) -> Option<&T> {
        if self.is::<T>() {
            Some(unsafe { T::downcast_impl(self) })
        } else {
            None
        }
    }
    /// Downcast to a concrete type
    /// # Safety
    /// The target type must be correct
    pub unsafe fn downcast_unchecked<T: Type>(&self) -> &T {
        T::downcast_impl(self)
    }
}
impl PartialEq for dyn Type {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_of!(self) as usize == std::ptr::addr_of!(other) as usize
    }
}
impl<T: Type> PartialEq<T> for dyn Type {
    fn eq(&self, other: &T) -> bool {
        std::ptr::addr_of!(self) as usize == std::ptr::addr_of!(other) as usize
    }
}
impl Eq for dyn Type {}
impl Hash for dyn Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(addr_of!(self) as _)
    }
}
pub type TypeRef = &'static dyn Type;
/// save a type to the output buffer
pub fn save_type(buf: &mut dyn Write, ty: TypeRef) -> io::Result<()> {
    buf.write_all(&ty.self_kind().get().to_be_bytes())?;
    ty.save(buf)
}
/// load a type from the buffer
pub fn load_type(buf: &mut dyn BufRead) -> io::Result<TypeRef> {
    load_type_opt(buf).and_then(|v| {
        v.ok_or(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "unexpected end of input",
        ))
    })
}
/// type ids are NonZeroU64, so if it reads a 0 it returns None
pub fn load_type_opt(buf: &mut dyn BufRead) -> io::Result<Option<TypeRef>> {
    #![allow(unused_variables)]
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
const fn pad_bytes<const N: usize>(val: &[u8]) -> [u8; N] {
    // I wish more stuff was const
    let len = if val.len() < N { val.len() } else { N };
    let mut out = [0u8; N];
    let mut i = 0;
    while i < len {
        out[i] = val[i];
        i += 1;
    }
    out
}
/// generate a type id from a byte string
/// truncate or zero pad to 8 bytes
#[inline(always)]
pub const fn make_id(id: &[u8]) -> NonZeroU64 {
    if let Some(id) = NonZeroU64::new(u64::from_be_bytes(pad_bytes(id))) {
        id
    } else {
        panic!("ID string should not be empty (or all nulls)")
    }
}

pub mod agg;
pub mod custom;
pub mod float;
pub mod func;
pub mod int;
pub mod mem;
pub mod meta;

pub use agg::*;
pub use custom::*;
pub use float::*;
pub use func::*;
pub use int::*;
pub use mem::*;
pub use meta::*;

use ref_cast::*;
