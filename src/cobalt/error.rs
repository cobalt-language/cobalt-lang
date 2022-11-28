use crate::Location;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Note<'a> {
    pub loc: Location<'a>,
    pub message: String
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error<'a> {
    pub loc: Location<'a>,
    pub code: u64,
    pub message: String,
    pub notes: Vec<Note<'a>>
}
impl<'a> Error<'a> {
    pub fn new(loc: Location<'a>, code: u64, message: String) -> Self {Error{loc, code, message, notes: vec![]}}
    pub fn note(mut self, note: Note<'a>) -> Self {
        self.notes.push(note);
        self
    }
    pub fn add_note(&mut self, note: Note<'a>) -> &mut Self {
        self.notes.push(note);
        self
    }
}
