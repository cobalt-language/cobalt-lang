//! Sometimes, you need a type to live for 'static, even though you can guarantee you won't use it past a lifetime.
//! This is where `StaticRef` and `StaticRefGuard` come in. `StaticRefGuard` wraps a reference for a given lifetime,
//! and any `StaticRef`s it creates are checked at runtime to not live past its lifetime.
use std::marker::PhantomPinned;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
/// `StaticRefGuard` wraps a reference. It ensures that `StaticRef`s can't extend past its lifetime.
#[derive(Debug, Clone)]
pub struct StaticRefGuard<'a, T: ?Sized> {
    reference: &'a T,
    valid: Arc<AtomicBool>,
}
impl<'a, T: ?Sized> StaticRefGuard<'a, T> {
    pub fn new(reference: &'a T) -> Self {
        Self {
            reference,
            valid: Arc::new(AtomicBool::new(true)),
        }
    }
    pub fn get_ref(&self) -> StaticRef<T> {
        StaticRef {
            ptr: self.reference as _,
            valid: self.valid.clone(),
        }
    }
}
impl<T: ?Sized> Drop for StaticRefGuard<'_, T> {
    fn drop(&mut self) {
        self.valid.store(false, Ordering::Release)
    }
}
#[derive(Debug, Clone)]
pub struct StaticValueGuard<T> {
    value: T,
    valid: Arc<AtomicBool>,
    _pin: PhantomPinned,
}
impl<T> StaticValueGuard<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            valid: Arc::new(AtomicBool::new(true)),
            _pin: PhantomPinned,
        }
    }
    pub fn get_ref(&self) -> StaticRef<T> {
        StaticRef {
            ptr: std::ptr::addr_of!(self.value),
            valid: self.valid.clone(),
        }
    }
}
impl<T> Drop for StaticValueGuard<T> {
    fn drop(&mut self) {
        self.valid.store(false, Ordering::Release)
    }
}
/// `StaticRef` acts like a reference, but has `'static` lifetime.
/// It panics if you try to access its contents after the lifetime has expired
pub struct StaticRef<T: ?Sized> {
    ptr: *const T,
    valid: Arc<AtomicBool>,
}
impl<T: ?Sized> StaticRef<T> {
    pub fn is_valid(this: &Self) -> bool {
        this.valid.load(Ordering::Acquire)
    }
    pub fn try_get(this: &Self) -> Option<&T> {
        Self::is_valid(this).then(|| unsafe { &*this.ptr })
    }
    pub fn get(this: &Self) -> &T {
        assert!(Self::is_valid(this), "lifetime has expired");
        unsafe { &*this.ptr }
    }
}
impl<T: ?Sized> std::ops::Deref for StaticRef<T> {
    type Target = T;
    fn deref(&self) -> &T {
        Self::get(self)
    }
}
unsafe impl<T: ?Sized + Sync> Send for StaticRef<T> {}
unsafe impl<T: ?Sized + Sync> Sync for StaticRef<T> {}
