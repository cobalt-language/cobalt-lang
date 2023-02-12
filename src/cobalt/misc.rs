use std::fmt::*;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub word_size: u64,
    pub(crate) up: bool
}
impl Default for Flags {
    fn default()->Self {
        Flags {
            word_size: std::mem::size_of::<isize>() as u64,
            up: true
        }
    }
}
