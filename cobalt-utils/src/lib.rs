use std::cell::Cell;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u16,
    pub bounds_checks: bool,
    pub prepass: bool,
    pub dbg_mangle: bool,
    pub up: bool
}
impl Default for Flags {
    fn default() -> Self {
        Flags {
            word_size: std::mem::size_of::<isize>() as u16,
            bounds_checks: true,
            prepass: true,
            dbg_mangle: false,
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
pub struct CellExt<T>(Cell<Option<T>>);
impl<T> CellExt<T> {
    pub fn new(val: T) -> Self {CellExt(Cell::new(Some(val)))}
    pub fn map<F: FnOnce(T) -> T>(&self, f: F) -> &Self {
        let val = self.0.take();
        self.0.set(Some(f(val.unwrap())));
        self
    }
    pub fn map_split<U, F: FnOnce(T) -> (T, U)>(&self, f: F) -> U {
        let val = self.0.take();
        let (val, ret) = f(val.unwrap());
        self.0.set(Some(val));
        ret
    }
    pub fn try_map<E, F: FnOnce(T) -> Result<T, E>>(&self, f: F) -> Result<(), E> {
        let val = self.0.take();
        self.0.set(Some(f(val.unwrap())?));
        Ok(())
    }
    pub fn with<R, F: FnOnce(&mut T) -> R>(&self, f: F) -> R {
        let mut val = self.0.take().unwrap();
        let out = f(&mut val);
        self.0.set(Some(val));
        out
    }
    pub fn replace(&self, val: T) -> T {self.0.replace(Some(val)).unwrap()}
    pub fn set(&self, val: T) {self.0.set(Some(val))}
    pub fn into_inner(self) -> T {self.0.into_inner().unwrap()}
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
