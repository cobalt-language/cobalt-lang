/// Convenience re-exports so miette doesn't have to be a dependency
pub use miette::{Report, SourceSpan};
/// File registry
pub mod files;
pub use files::{CobaltFile, FILES};
/// `warning!` and `error!`
pub mod color;
/// CobaltError and SourcedError
pub mod error;
pub use error::*;
pub use miette;

pub fn unreachable_span() -> SourceSpan {
    (usize::MAX, usize::MAX).into()
}
pub fn remove_unreachable(span: SourceSpan) -> Option<SourceSpan> {
    (span != unreachable_span()).then_some(span)
}
pub fn merge_spans(a: SourceSpan, b: SourceSpan) -> SourceSpan {
    use std::cmp::{max, min};
    let start = min(a.offset(), b.offset());
    let end = max(a.offset() + a.len(), b.offset() + b.len());
    (start, end - start).into()
}
