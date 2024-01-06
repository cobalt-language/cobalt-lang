#![allow(unused_variables, ambiguous_glob_reexports)]
use crate::*;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
use once_cell::sync::Lazy;
use serde_state::de::DeserializeOwned;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;
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
/// AsTypeRef was moved to a separate trait so concrete and trait objects can be used interchangeably
pub trait AsTypeRef {
    fn as_type_ref(&'static self) -> TypeRef;
}
impl<T: Type + Sized> AsTypeRef for T {
    fn as_type_ref(&'static self) -> TypeRef {
        self
    }
}
/// A ConcreteType is not object-safe, and only holds the Self::KIND associated constant
pub trait ConcreteType: Type + Sized {
    const KIND: u64;
}
impl<T: Type + Sized + ConstIdentify> ConcreteType for T {
    const KIND: u64 = Self::CONST_ID.raw_value();
}
/// TypeKind contains `self.kind()`, but it's moved to a separate trait so it can be auto-implemented
pub trait TypeKind {
    /// get the kind ID of this type
    fn kind(&self) -> u64;
}
impl<T: ConcreteType> TypeKind for T {
    fn kind(&self) -> u64 {
        Self::KIND
    }
}

pub trait TypeSerde: Sized {
    type Header: Serialize + DeserializeOwned;
    type Proxy: Serialize + DeserializeOwned;
    fn get_header() -> Self::Header;
    fn set_header(header: Self::Header);
    fn get_proxy(&'static self) -> Self::Proxy;
    fn from_proxy(body: Self::Proxy) -> &'static Self;
}
pub trait TypeSerdeFns {
    fn erased_header() -> Box<dyn erased_serde::Serialize>
    where
        Self: Sized;
    fn load_header(
        deserializer: &mut dyn erased_serde::Deserializer,
    ) -> Result<(), erased_serde::Error>
    where
        Self: Sized;
    fn erased_proxy(&'static self) -> Box<dyn erased_serde::Serialize>;
    fn load(
        deserializer: &mut dyn erased_serde::Deserializer,
    ) -> Result<TypeRef, erased_serde::Error>
    where
        Self: Sized;
}
impl<T: Type + TypeSerde> TypeSerdeFns for T {
    fn erased_header() -> Box<dyn erased_serde::Serialize> {
        Box::new(Self::get_header())
    }
    fn load_header(
        deserializer: &mut dyn erased_serde::Deserializer,
    ) -> Result<(), erased_serde::Error> {
        erased_serde::deserialize(deserializer).map(Self::set_header)
    }
    fn erased_proxy(&'static self) -> Box<dyn erased_serde::Serialize> {
        Box::new(self.get_proxy())
    }
    fn load(
        deserializer: &mut dyn erased_serde::Deserializer,
    ) -> Result<TypeRef, erased_serde::Error>
    where
        Self: Sized,
    {
        Ok(Self::from_proxy(erased_serde::deserialize(deserializer)?) as _)
    }
}

pub trait NullType: Type {
    fn create_self() -> &'static Self;
}
impl<T: NullType> TypeSerde for T {
    type Header = ();
    type Proxy = ();
    fn get_header() -> Self::Header {}
    fn set_header(header: Self::Header) {}
    fn get_proxy(&'static self) -> Self::Proxy {}
    fn from_proxy(body: Self::Proxy) -> &'static Self {
        Self::create_self()
    }
}
macro_rules! no_type_header {
    () => {
        type Header = ();
        fn get_header() -> Self::Header {}
        fn set_header(header: Self::Header) {}
    };
}
macro_rules! impl_null_type_with_new {
    ($ty:ty) => {
        impl NullType for $ty {
            fn create_self() -> &'static Self {
                Self::new()
            }
        }
    };
}
macro_rules! impl_type_proxy {
    ($proxy:ty, $gp:pat => $ge:expr, $sp:pat => $se:expr) => {
        type Proxy = $proxy;
        #[inline(always)]
        fn get_proxy(&'static self) -> $proxy {
            #[allow(irrefutable_let_patterns)]
            let $gp = self
            else {
                panic!("{self} is in an invalid state!")
            };
            $ge
        }
        #[inline(always)]
        fn from_proxy(proxy: $proxy) -> &'static Self {
            #[allow(irrefutable_let_patterns)]
            let $sp = proxy
            else {
                panic!();
            };
            $se
        }
    };
    ($proxy:ty, $get:expr, $set:expr) => {
        type Proxy = $proxy;
        #[inline(always)]
        #[allow(clippy::redundant_closure_call)]
        fn get_proxy(&'static self) -> $proxy {
            ($get)(self)
        }
        #[inline(always)]
        #[allow(clippy::redundant_closure_call)]
        fn from_proxy(proxy: $proxy) -> &'static Self {
            ($set)(proxy)
        }
    };
}

/// All values have types, and each kind of type implements `Type`.
/// Methods with a leading underscore are customization points, and are not meant to be called directly.
pub trait Type:
    AsTypeRef + TypeKind + TypeSerdeFns + Debug + Display + Send + Sync + 'static
{
    /// Get the size of the current type.
    fn size(&'static self) -> SizeType;
    /// Get the alignment of the current type. An alignment of 0 is used for non-runtime data.
    fn align(&'static self) -> u16;
    /// Get the LLVM type that this type lowers to.
    fn llvm_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        None
    }
    /// Get the LLVM type of `*<self>`. This is useful for fat pointer types.
    fn ptr_type<'ctx>(&'static self, ctx: &CompCtx<'_, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        (self.size() != SizeType::Meta).then(|| ctx.null_type.ptr_type(Default::default()).into())
    }

    /// Check if a type is linear - can only be used once
    fn is_linear(&'static self, ctx: &CompCtx) -> bool {
        false
    }
    /// Check if a type can be copied without tracking
    fn copyable(&'static self, ctx: &CompCtx) -> bool {
        !(self.has_dtor(ctx) || self.is_linear(ctx))
    }
    /// Check whether or not this type has a destructor.
    fn has_dtor(&'static self, ctx: &CompCtx) -> bool {
        false
    }
    /// Insert a destructor for a value at the current insertion position.
    fn ins_dtor<'src, 'ctx>(&'static self, val: &Value<'src, 'ctx>, ctx: &CompCtx<'src, 'ctx>) {}
    /// Get the "decayed" type - essentially what the type of a variable would be with this assigned to it.
    fn decay(&'static self) -> TypeRef {
        self.as_type_ref()
    }

    /// Find a field on the type. Currently only used for custom types, but could be expanded
    fn static_attr<'src, 'ctx>(
        &'static self,
        name: &str,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<Value<'src, 'ctx>> {
        None
    }
    /// Get an attribute on the value
    fn attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
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
    fn _has_ref_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        false
    }
    fn _has_mut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        false
    }
    fn _has_refmut_attr(&'static self, attr: &str, ctx: &CompCtx) -> bool {
        false
    }
    fn _ref_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_ref_attr(&attr.0, ctx));
        Err(CobaltError::AttrNotDefined {
            val: self.to_string(),
            attr: attr.0,
            vloc: val.loc,
            aloc: attr.1,
        })
    }
    fn _mut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_ref_attr(&attr.0, ctx));
        Err(CobaltError::AttrNotDefined {
            val: self.to_string(),
            attr: attr.0,
            vloc: val.loc,
            aloc: attr.1,
        })
    }
    fn _refmut_attr<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_refmut_attr(&attr.0, ctx));
        Err(CobaltError::AttrNotDefined {
            val: self.to_string(),
            attr: attr.0,
            vloc: val.loc,
            aloc: attr.1,
        })
    }
    /// Check if this type is implicitly convertible to another type.
    fn impl_convertible(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self.as_type_ref() == other
            || self._can_iconv_to(other, ctx)
            || other._can_iconv_from(self.as_type_ref(), ctx)
    }
    /// Check if this type is explicitly convertible to another type.
    fn expl_convertible(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        self._can_econv_to(other, ctx)
            || other._can_econv_from(self.as_type_ref(), ctx)
            || self.impl_convertible(other, ctx)
    }
    /// Find a common type between two types.
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
    /// Apply a prefix operator to a value of the current type.
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
    /// Apply a postfix operator to a value of the current type.
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
    /// Call a value of the current type.
    fn call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_call(&val, cparen, args.iter()))
    }
    /// Subscript a value of the current type.
    /// If the index type is the error type, `Ok(Value::error())` should be returned.
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

    /// Implicitly convert a value of the current type to another
    fn _iconv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_iconv(&val, target.0, target.1))
    }
    /// Implicitly convert a value to the current type
    fn _iconv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_iconv(&val, self.as_type_ref(), target))
    }
    /// Explicitly convert a value of the current type to another
    fn _econv_to<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, target.0, target.1))
    }
    /// Explicitly convert a value to the current type
    fn _econv_from<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: Option<SourceSpan>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, self.as_type_ref(), target))
    }
    /// Return true iff `_iconv_to` returns an `Ok` value
    fn _can_iconv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    /// Return true iff `_iconv_from` returns an `Ok` value
    fn _can_iconv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    /// Return true iff `_econv_to` returns an `Ok` value
    fn _can_econv_to(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    /// Return true iff `_econv_from` returns an `Ok` value
    fn _can_econv_from(&'static self, other: TypeRef, ctx: &CompCtx) -> bool {
        false
    }

    /// Override this for binary operators where this type appears on the left side.
    /// `move_left` and `move_right` track whether or not moving from the left and right operands is permitted.
    fn _bin_lhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_bin_lhs(
            rhs.data_type.as_type_ref(),
            op.0,
            ctx,
            move_left,
            move_right
        ));
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    /// See [`_bin_lhs`].
    fn _bin_rhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_bin_rhs(
            lhs.data_type.as_type_ref(),
            op.0,
            ctx,
            move_left,
            move_right
        ));
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    /// Returns true iff `_bin_lhs` returns an `Ok` value
    fn _has_bin_lhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        false
    }
    /// Returns true iff `_bin_rhs` returns an `Ok` value
    fn _has_bin_rhs(
        &'static self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        false
    }
    /// Override this to implement mutating prefix operators
    fn _mut_pre_op<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        op: &'static str,
        oloc: SourceSpan,
        ctx: &CompCtx<'src, 'ctx>,
        can_move: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_preop(&val, op, oloc))
    }
    /// Returns true iff `_mut_pre_op` returns an `Ok` value
    fn _has_mut_pre_op(&'static self, op: &'static str, ctx: &CompCtx) -> bool {
        false
    }
    /// Override this for types that have special behavior when their reference is called
    fn _ref_call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_call(&val, cparen, args.iter()))
    }
    /// Override this for types that have special behavior when their mutable reference is called
    fn _refmut_call<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(invalid_call(&val, cparen, args.iter()))
    }
    fn _has_ref_call(&'static self, args: &[TypeRef], ctx: &CompCtx) -> bool {
        false
    }
    fn _has_refmut_call(&'static self, args: &[TypeRef], ctx: &CompCtx) -> bool {
        false
    }
    fn _mut_subscript<'src, 'ctx>(
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
    fn _has_mut_subscript(&'static self, idx: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    /// Override this for types that have in-place operators or similar.
    /// This does **not** need to be overridden for assignment.
    fn _mut_bin_lhs<'src, 'ctx>(
        &'static self,
        lhs: Value<'src, 'ctx>,
        rhs: Value<'src, 'ctx>,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
        move_left: bool,
        move_right: bool,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        assert!(!self._has_mut_bin_lhs(
            rhs.data_type.as_type_ref(),
            op.0,
            ctx,
            move_left,
            move_right
        ));
        Err(invalid_binop(&lhs, &rhs, op.0, op.1))
    }
    fn _has_mut_bin_lhs(
        &self,
        other: TypeRef,
        op: &'static str,
        ctx: &CompCtx,
        move_left: bool,
        move_right: bool,
    ) -> bool {
        false
    }
    fn _can_ref_iconv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_ref_econv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_refmut_iconv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    fn _can_refmut_econv(&'static self, target: TypeRef, ctx: &CompCtx) -> bool {
        false
    }
    /// Override implicit conversion from a reference to the current type
    fn _ref_iconv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_iconv(&val, target.0, target.1))
    }
    /// Override explicit conversion from a reference to the current type
    fn _ref_econv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, target.0, target.1))
    }
    /// Override implicit conversion from a mutable reference to the current type
    fn _refmut_iconv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_iconv(&val, target.0, target.1))
    }
    /// Override explicit conversion from a nutable reference to the current type
    fn _refmut_econv<'src, 'ctx>(
        &'static self,
        val: Value<'src, 'ctx>,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        Err(cant_econv(&val, target.0, target.1))
    }

    /// Get the compiled form of an interpreted value, if possible.
    fn compiled<'src, 'ctx>(
        &'static self,
        inter_val: &InterData<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        None
    }
    /// Downcast from a type
    /// # Safety
    /// The casted from type must be the same as Self
    unsafe fn downcast_impl(val: &dyn Type) -> &Self
    where
        Self: ConcreteType,
    {
        std::mem::transmute::<&dyn Type, (&Self, *const u8)>(val).0
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
    pub fn is<T: ConcreteType>(&self) -> bool {
        self.kind() == T::KIND
    }
    pub fn is_and<'a, T: ConcreteType + 'a>(&'a self, f: impl FnOnce(&'a T) -> bool) -> bool {
        self.downcast::<T>().map_or(false, f)
    }
    pub fn downcast<T: ConcreteType>(&self) -> Option<&T> {
        if self.is::<T>() {
            Some(unsafe { T::downcast_impl(self) })
        } else {
            None
        }
    }
    /// Downcast to a concrete type
    /// # Safety
    /// The target type must be correct
    pub unsafe fn downcast_unchecked<T: ConcreteType>(&self) -> &T {
        T::downcast_impl(self)
    }
}
/// Since all Type implementors intern their variables, &dyn Type can be checked for pointer equality
impl PartialEq for dyn Type {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            // extract thin pointers
            std::mem::transmute::<&dyn Type, (*const u8, *const u8)>(self).0
                == std::mem::transmute::<&dyn Type, (*const u8, *const u8)>(other).0
        }
    }
}
impl<T: Type + Sized> PartialEq<T> for dyn Type {
    fn eq(&self, other: &T) -> bool {
        std::ptr::eq(
            unsafe { std::mem::transmute::<&dyn Type, (*const T, *const u8)>(self).0 },
            other,
        )
    }
}
impl Eq for dyn Type {}
impl Hash for dyn Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(unsafe {
            std::mem::transmute::<&dyn Type, (*const u8, *const u8)>(self).0
        } as _)
    }
}
pub type TypeRef = &'static dyn Type;

impl Serialize for TypeRef {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use ser::*;
        let mut map = serializer.serialize_struct("Type", 3)?;
        map.serialize_field("kind", &hex::encode(self.kind().to_le_bytes()))?;
        map.serialize_field("type", &self.erased_proxy())?;
        map.end()
    }
}
impl<'de> Deserialize<'de> for TypeRef {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use de::Error;
        #[derive(Deserialize)]
        #[serde(bound = "'a: 'de")]
        struct Proxy<'a> {
            #[serde(with = "hex::serde")]
            kind: [u8; 8],
            #[serde(borrow, rename = "type")]
            ty: serde::__private::de::Content<'a>,
        }
        let p = Proxy::deserialize(deserializer)?;
        (TYPE_SERIAL_REGISTRY
            .pin()
            .get(&u64::from_le_bytes(p.kind))
            .ok_or_else(|| D::Error::custom(format!("unknown type id {}", hex::encode(p.kind))))?
            .load)(&mut <dyn erased_serde::Deserializer>::erase(
            serde::__private::de::ContentDeserializer::<'de, D::Error>::new(p.ty),
        ))
        .map_err(D::Error::custom)
    }
}
pub struct TypeLoader {
    pub kind: u64,
    pub erased_header: fn() -> Box<dyn erased_serde::Serialize>,
    pub load_header: fn(&mut dyn erased_serde::Deserializer) -> Result<(), erased_serde::Error>,
    pub load: fn(&mut dyn erased_serde::Deserializer) -> Result<TypeRef, erased_serde::Error>,
}
#[macro_export]
macro_rules! type_loader {
    ($T:ty) => {
        $crate::types::TypeLoader {
            kind: <$T as $crate::types::ConcreteType>::KIND,
            erased_header: <$T as $crate::types::TypeSerdeFns>::erased_header,
            load_header: <$T as $crate::types::TypeSerdeFns>::load_header,
            load: <$T as $crate::types::TypeSerdeFns>::load,
        }
    };
}
macro_rules! submit_types {
    ($T:ty) => (
        inventory::submit! {type_loader!($T)}
    );
    ($T:ty, $($Ts:ty),+) => {
        submit_types!($T);
        submit_types!($($Ts),+);
    }
}
inventory::collect!(TypeLoader);
pub struct LoadInfo {
    pub erased_header: fn() -> Box<dyn erased_serde::Serialize>,
    pub load_header: fn(&mut dyn erased_serde::Deserializer) -> Result<(), erased_serde::Error>,
    pub load: fn(&mut dyn erased_serde::Deserializer) -> Result<TypeRef, erased_serde::Error>,
}
impl LoadInfo {
    pub fn new(
        &TypeLoader {
            kind,
            erased_header,
            load_header,
            load,
            ..
        }: &'static TypeLoader,
    ) -> (u64, Self) {
        (
            kind,
            Self {
                erased_header,
                load_header,
                load,
            },
        )
    }
}
pub static TYPE_SERIAL_REGISTRY: Lazy<flurry::HashMap<u64, LoadInfo>> =
    Lazy::new(|| inventory::iter::<TypeLoader>().map(LoadInfo::new).collect());

pub mod agg;
pub mod custom;
pub mod float;
pub mod func;
pub mod int;
pub mod intrinsic;
pub mod mem;
pub mod meta;

pub use agg::*;
pub use custom::*;
pub use float::*;
pub use func::*;
pub use int::*;
pub use intrinsic::*;
pub use mem::*;
pub use meta::{Symbol, *};
use ref_cast::*;
