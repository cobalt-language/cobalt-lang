use codespan_reporting::{diagnostic::{self, *}, files::*};
use std::ops::Range;
use std::sync::RwLock;
pub mod files {
    use super::*;
    lazy_static::lazy_static! {
        pub static ref FILES: RwLock<SimpleFiles<String, String>> = RwLock::new(SimpleFiles::new());
    }
    pub fn add_file(name: String, source: String) -> FileId {
        FILES.write().expect("FILES should not be poisoned").add(name, source)
    }
}
pub type FileId = usize;
pub type Location = (FileId, Range<usize>);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic(pub diagnostic::Diagnostic<FileId>, pub u64);
impl Diagnostic {
    pub fn code(&self) -> u64 {self.1}
    pub fn is_err(&self) -> bool {self.1 >= 100}
    pub fn is_warn(&self) -> bool {self.1 < 100}
    pub fn error(loc: Location, code: u64, message: Option<String>) -> Self {
        assert!(code >= 100, "errors must have codes greater than or equal to 100");
        Diagnostic(diagnostic::Diagnostic::error().with_code(format!("E{code:0>4}")).with_message(info::lookup(code).map_or("<unknown error>", |i| i.message)).with_labels(vec![if let Some(message) = message {Label::primary(loc.0, loc.1).with_message(message)} else {Label::primary(loc.0, loc.1)}]), code)
    }
    pub fn warning(loc: Location, code: u64, message: Option<String>) -> Self {
        assert!(code < 100, "warnings must have codes less than 100");
        Diagnostic(diagnostic::Diagnostic::warning().with_code(format!("W{code:0>4}")).with_message(info::lookup(code).map_or("<unknown error>", |i| i.message)).with_labels(vec![if let Some(message) = message {Label::primary(loc.0, loc.1).with_message(message)} else {Label::primary(loc.0, loc.1)}]), code)
    }
    pub fn note(self, loc: Location, message: String) -> Self {
        Diagnostic(self.0.with_labels(vec![Label::secondary(loc.0, loc.1).with_message(message)]), self.1)
    }
    pub fn info(self, message: String) -> Self {
        Diagnostic(self.0.with_notes(vec![message]), self.1)
    }
}
pub mod info;
