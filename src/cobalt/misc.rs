use std::fmt::*;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub(crate) up: bool
}
impl Default for Flags {
    fn default()->Self {
        Flags {
            up: true
        }
    }
}
