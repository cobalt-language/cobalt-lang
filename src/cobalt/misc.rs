#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Location<'a> {
    pub file: &'a str,
    pub line: u64,
    pub col: u64,
    pub offset: u64
}
impl<'a> Location<'a> {
    fn new(file: &'a str, line: u64, col: u64, offset: u64)->Self {Location{file, line, col, offset}}
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Flags {
    pub(crate) update_location: bool
}
impl Default for Flags {
    fn default()->Self {
        Flags {
            update_location: true
        }
    }
}
