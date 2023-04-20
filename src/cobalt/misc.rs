use std::mem::MaybeUninit;
use std::cell::Cell;
use std::pin::Pin;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u16,
    pub bounds_checks: bool,
    pub prepass: bool,
    pub(crate) up: bool
}
impl Default for Flags {
    fn default() -> Self {
        Flags {
            word_size: std::mem::size_of::<isize>() as u16,
            bounds_checks: true,
            prepass: true,
            up: true,
        }
    }
}
#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Counter(Cell<i32>);
impl Counter {
    pub fn min() -> Self {Counter(Cell::new(0))}
    pub fn max() -> Self {Counter(Cell::new(std::i32::MAX as i32))}
    pub fn get(&self) -> i32 {self.0.get()}
    pub fn incr(&self) -> &Self {self.0.set(self.0.get() + 1); self}
    pub fn decr(&self) -> &Self {self.0.set(self.0.get() - 1); self}
}
pub struct CellExt<T>(Cell<MaybeUninit<T>>);
impl<T> CellExt<T> {
    pub fn new(val: T) -> Self {CellExt(Cell::new(MaybeUninit::new(val)))}
    pub fn map<F: FnOnce(T) -> T>(&self, f: F) -> &Self {
        let val = self.0.replace(MaybeUninit::uninit());
        self.0.set(MaybeUninit::new(unsafe {f(val.assume_init())}));
        self
    }
    pub fn map_split<U, F: FnOnce(T) -> (T, U)>(&self, f: F) -> U {
        let val = self.0.replace(MaybeUninit::uninit());
        let (val, ret) = unsafe {f(val.assume_init())};
        self.0.set(MaybeUninit::new(val));
        ret
    }
    pub fn try_map<E, F: FnOnce(T) -> Result<T, E>>(&self, f: F) -> Result<(), E> {
        let val = self.0.replace(MaybeUninit::uninit());
        self.0.set(MaybeUninit::new(unsafe {f(val.assume_init())}?));
        Ok(())
    }
    pub fn with<R, F: FnOnce(&mut T) -> R>(&self, f: F) -> R {
        let mut val = unsafe {self.0.replace(MaybeUninit::uninit()).assume_init()};
        let out = f(&mut val);
        self.0.set(MaybeUninit::new(val));
        out
    }
    pub fn replace(&self, val: T) -> T {unsafe {self.0.replace(MaybeUninit::new(val)).assume_init()}}
    pub fn set(&self, val: T) {unsafe {self.0.replace(MaybeUninit::new(val)).assume_init_drop()}}
}
impl<T> CellExt<Pin<Box<T>>> {
    pub fn with_self<'this, R, F: FnOnce(&mut Pin<Box<T>>) -> R>(&'this self, f: F) -> R {
        let mut val = unsafe {self.0.replace(MaybeUninit::uninit()).assume_init()};
        let out = f(unsafe {std::mem::transmute::<&mut _, &'this mut _>(&mut val)});
        self.0.set(MaybeUninit::new(val));
        out
    }
}
impl<T> Drop for CellExt<T> {
    fn drop(&mut self) {
        unsafe {
            self.0.replace(MaybeUninit::uninit()).assume_init_drop();
        }
    }
}
impl<T: Clone> Clone for CellExt<T> {
    fn clone(&self) -> Self {
        Self::new(self.with(|v| v.clone()))
    }
}
impl<T: Default> Default for CellExt<T> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}
impl<T: std::fmt::Debug> std::fmt::Debug for CellExt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.with(|v| write!(f, "{v:?}"))
    }
}
impl<T: std::fmt::Display> std::fmt::Display for CellExt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.with(|v| write!(f, "{v}"))
    }
}
