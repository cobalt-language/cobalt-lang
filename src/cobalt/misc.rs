use std::fmt::*;
use colored::Colorize;
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Location {
    pub file: &'static str,
    pub line: u64,
    pub col: u64,
    pub offset: u64
}
impl Location {
    pub fn null() -> Self {Location::new("<anonymous>", 0, 0, 0)}
    pub fn new(file: &'static str, line: u64, col: u64, offset: u64) -> Self {Location{file, line, col, offset}}
    pub fn from_name(file: &'static str) -> Self {Location{file, line: 1, col: 1, offset: 0}}
}
impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if f.alternate() {write!(f, "{}", format!("{}", self).blue().bold())}
        else {write!(f, "{}:{}:{}", self.file, self.line, self.col)}
    }
}
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
