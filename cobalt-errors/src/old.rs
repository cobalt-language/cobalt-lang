use miette::{SourceSpan};
use thiserror::Error;
use miette::Diagnostic as MDiagnostic;
/// Dummy diagnostic so stuff still compiled. It will be removed
#[derive(Clone, Debug, PartialEq, Eq, Error, MDiagnostic)]
#[deprecated]
#[error("code: {0}")]
pub struct Diagnostic(pub u64);
impl Diagnostic {
    pub fn code(&self) -> u64 {self.0}
    pub fn is_err(&self) -> bool {self.0 >= 100}
    pub fn is_warn(&self) -> bool {self.0 < 100}
    pub fn error(_loc: SourceSpan, _code: u64, _message: Option<String>) -> Self {panic!("errors have been removed!")}
    pub fn warning(_loc: SourceSpan, _code: u64, _message: Option<String>) -> Self {panic!("errors have been removed!")}
    pub fn note(self, _loc: SourceSpan, _message: String) -> Self {self}
    pub fn info(self, _message: String) -> Self {self}
    pub fn add_note(&mut self, _loc: SourceSpan, _message: String) -> &mut Self {self}
    pub fn add_info(&mut self, _message: String) -> &mut Self {self}
}
