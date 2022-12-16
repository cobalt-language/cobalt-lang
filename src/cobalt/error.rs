use crate::Location;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Note {
    pub loc: Location,
    pub message: String
}
impl Note {
    pub fn new(loc: Location, message: String) -> Self {Note{loc, message}}
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pub loc: Location,
    pub code: u64,
    pub message: String,
    pub notes: Vec<Note>
}
impl Error {
    pub fn new(loc: Location, code: u64, message: String) -> Self {Error{loc, code, message, notes: vec![]}}
    pub fn note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }
    pub fn add_note(&mut self, note: Note) -> &mut Self {
        self.notes.push(note);
        self
    }
}
