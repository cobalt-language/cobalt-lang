use std::fmt::*;
use std::cell::Cell;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u16,
    pub(crate) up: bool
}
impl Default for Flags {
    fn default()->Self {
        Flags {
            word_size: std::mem::size_of::<isize>() as u16,
            up: true
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
