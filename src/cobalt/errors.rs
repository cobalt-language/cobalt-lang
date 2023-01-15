use codespan_reporting::{diagnostic::{self, *}, files::*};
use std::ops::Range;
use std::cell::Cell;
pub mod files {
    pub static FILES: Cell<SimpleFiles<String, String>> = Cell::new(SimpleFiles::new());
    pub fn files() -> &SimpleFiles<String, String> {
        &*FILES.get_mut()
    }
    pub fn get_file(id: FileId) -> Option<&SimpleFile<String, String>> {
        FILES.get_mut().get(id).ok()
    }
    pub fn add_file(name: String, source: String) -> FileId {
        FILES.get_mut().add(name, source)
    }
}
pub type FileId = SimpleFiles::FileId;
pub type Location = (FileId, Range<usize>);
pub struct Diagnostic(pub diagnostic::Diagnostic<FileId>, pub u64);
impl Diagnostic {
    pub fn error(loc: Location, code: u64) -> Self {
        Diagnostic(diagnostic::Diagnostic::error().with_code(format!("E{code}")).with_message(info::lookup(code).map_or("<unknown error>", |i| i.message)), code)
    }
    pub fn warning(loc: Location, code: u64) -> Self {
        Diagnostic(diagnostic::Diagnostic::warning().with_code(format!("W{code}")).with_message(info::lookup(code).map_or("<unknown error>", |i| i.message)), code)
    }
    pub fn primary(self, loc: Location, message: String) -> Self {
        Diagnostic(self.0.with_labels(vec![Label::primary(loc.0, loc.1).with_message(message)]), self.1)
    }
    pub fn note(self, loc: Location, message: String) -> Self {
        Diagnostic(self.0.with_labels(vec![Label::secondary(loc.0, loc.1).with_message(message)]), self.1)
    }
    pub fn info(self, message: String) -> Self {
        Diagnostic(self.0.with_notes(vec![message]), self.1)
    }
}
pub mod info;
