#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Location<'a> {
    pub file: &'a str,
    pub line: u64,
    pub col: u64,
    pub offset: u64
}
impl<'a> Location<'a> {
    pub fn new(file: &'a str, line: u64, col: u64, offset: u64) -> Self {Location{file, line, col, offset}}
    pub fn from_name(file: &'a str) -> Self {Location{file, line: 1, col: 1, offset: 0}}
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
