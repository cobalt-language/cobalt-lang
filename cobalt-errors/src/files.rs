use miette::{SourceCode, SpanContents, SourceSpan, MietteError};
use parking_lot::{RwLock, RwLockReadGuard, MappedRwLockReadGuard};
use once_cell::sync::Lazy;
static UNIT_REF: () = ();
#[allow(clippy::type_complexity)]
pub struct FileRegistry(Lazy<RwLock<Vec<(String, Vec<InnerFile>)>>>);
impl FileRegistry {
    pub(self) const fn new() -> Self {Self(Lazy::new(|| RwLock::new(vec![(String::new(), vec![])])))}
    pub fn add_module(&self, name: String) -> usize {
        let mut lock = self.0.write();
        lock.push((name, vec![]));
        lock.len() - 1
    }
    pub fn add_file(&self, module: usize, mut name: String, contents: String) -> CobaltFile {
        let mut lock = self.0.write();
        let (mname, vec) = &mut lock[module];
        if !mname.is_empty() {name = format!("{mname}//{name}")} // format files in other modules as module//path/to/file
        vec.push(InnerFile(name, contents));
        CobaltFile(module, vec.len() - 1)
    }
}
/// Wrapper around a span to optionally add a name
struct MaybeNamed<'a>(Box<dyn SpanContents<'a> + 'a>, Option<&'a str>);
impl<'a> SpanContents<'a> for MaybeNamed<'a> {
    fn data(&self) -> &'a [u8] {self.0.data()}
    fn span(&self) -> &SourceSpan {self.0.span()}
    fn line(&self) -> usize {self.0.line()}
    fn column(&self) -> usize {self.0.column()}
    fn line_count(&self) -> usize {self.0.line_count()}
    fn name(&self) -> Option<&str> {self.1}
}
/// Span carrying a lock guard for thread safety
/// This has to be implemented like this because the guard requires a reference, and the Box is
/// owned
struct LockedSpan<'a>(Box<dyn SpanContents<'a> + 'a>, MappedRwLockReadGuard<'a, ()>);
impl<'a> SpanContents<'a> for LockedSpan<'a> {
    fn data(&self) -> &'a [u8] {self.0.data()}
    fn span(&self) -> &SourceSpan {self.0.span()}
    fn line(&self) -> usize {self.0.line()}
    fn column(&self) -> usize {self.0.column()}
    fn line_count(&self) -> usize {self.0.line_count()}
    fn name(&self) -> Option<&str> {self.0.name()}
}
/// File implementation. It's like `miette::NamedSource`, but returns a `MaybeNamed`, so it is
/// unnamed if the file name is an empty string
struct InnerFile(String, String);
impl SourceCode for InnerFile {
    fn read_span<'a>(&'a self, span: &SourceSpan, clb: usize, cla: usize) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        Ok(Box::new(MaybeNamed(self.1.read_span(span, clb, cla)?, (!self.0.is_empty()).then_some(&self.0))))
    }
}
/// Lightweight, copyable 
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CobaltFile(usize, usize);
impl CobaltFile {
    /// Get the name of the file that this points to.
    pub fn name(self) -> MappedRwLockReadGuard<'static, str> {RwLockReadGuard::map(FILES.0.read(), |lock| lock[self.0].1[self.1].0.as_str())}
    /// Get the contents of the file.
    pub fn contents(self) -> MappedRwLockReadGuard<'static, str> {RwLockReadGuard::map(FILES.0.read(), |lock| lock[self.0].1[self.1].1.as_str())}
    /// Extract the module identifier.
    pub fn module_id(self) -> usize {self.0}
    /// Return the source location of a point
    pub fn source_loc(self, loc: usize) -> Result<(usize, usize), MietteError> {
        let lock = FILES.0.read();
        let file = &lock[self.0].1[self.1].1;
        let span = file.read_span(&loc.into(), 0, 0)?;
        Ok((span.line(), span.column()))
    }
}
impl SourceCode for CobaltFile {
    fn read_span<'a>(&'a self, span: &SourceSpan, clb: usize, cla: usize) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let mut res: Option<Result<Box<dyn SpanContents<'a> + 'a>, MietteError>> = None;
        let lock = RwLockReadGuard::map(FILES.0.read(), |lock| {
            res = Some(unsafe {std::mem::transmute(lock[self.0].1[self.1].read_span(span, clb, cla))}); // transmute to avoid borrowing
            &UNIT_REF // we need to return a reference to something
        });
        Ok(Box::new(LockedSpan(res.unwrap()?, lock)))
    }
}
/// The actual registry. This is where all of the files are stored
pub static FILES: FileRegistry = FileRegistry::new();
