pub use miette::SourceSpan;
#[allow(deprecated)]
pub mod old;
pub use old::Diagnostic;
pub mod files;
pub use files::{FILES, CobaltFile};
pub mod color;

pub type FileId = (usize, usize);
pub fn unreachable_span() -> SourceSpan {(usize::MAX, usize::MAX).into()}
pub fn merge_spans(a: SourceSpan, b: SourceSpan) -> SourceSpan {
    use std::cmp::{min, max};
    let start = min(a.offset(), b.offset());
    let end = max(a.offset() + a.len(), b.offset() + b.len());
    (start, end - start).into()
}
