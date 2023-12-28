use aovec::Aovec;
use miette::{MietteError, SourceCode, SourceSpan, SpanContents};
use once_cell::sync::Lazy;
pub struct FileRegistry(Lazy<Aovec<(Box<str>, Aovec<InnerFile>)>>);
impl FileRegistry {
    pub(self) const fn new() -> Self {
        Self(Lazy::new(|| {
            let vec = Aovec::new(16);
            vec.push((Box::default(), Aovec::new(16)));
            vec
        }))
    }
    pub fn add_module(&self, name: Box<str>) -> usize {
        self.0.push((name, Aovec::new(16)));
        self.0.len() - 1
    }
    pub fn add_file(&self, module: usize, mut name: String, contents: Box<str>) -> CobaltFile {
        let (mname, vec) = &self.0[module];
        if !mname.is_empty() {
            name = format!("{mname}//{name}")
        } // format files in other modules as module//path/to/file
        vec.push(InnerFile(name.into_boxed_str(), contents));
        CobaltFile(module, vec.len() - 1)
    }
    pub fn lookup(&self, name: &str) -> Option<CobaltFile> {
        let mod_idx = name.find("//").map_or(Some(0), |idx| {
            (0..self.0.len()).find(|&i| &*self.0[i].0 == &name[..idx])
        })?;
        let vec = &self.0[mod_idx].1;
        let file_idx = (0..self.0.len()).find(|&i| &*vec[i].1 == name)?;
        Some(CobaltFile(mod_idx, file_idx))
    }
}
/// Wrapper around a span to optionally add a name
struct MaybeNamed<'a>(Box<dyn SpanContents<'a> + 'a>, Option<&'a str>);
impl<'a> SpanContents<'a> for MaybeNamed<'a> {
    fn data(&self) -> &'a [u8] {
        self.0.data()
    }
    fn span(&self) -> &SourceSpan {
        self.0.span()
    }
    fn line(&self) -> usize {
        self.0.line()
    }
    fn column(&self) -> usize {
        self.0.column()
    }
    fn line_count(&self) -> usize {
        self.0.line_count()
    }
    fn name(&self) -> Option<&str> {
        self.1
    }
}
/// File implementation. It's like `miette::NamedSource`, but returns a `MaybeNamed`, so it is
/// unnamed if the file name is an empty string
struct InnerFile(Box<str>, Box<str>);
impl SourceCode for InnerFile {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        clb: usize,
        cla: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        Ok(Box::new(MaybeNamed(
            self.1.read_span(span, clb, cla)?,
            (!self.0.is_empty()).then_some(&self.0),
        )))
    }
}
/// Lightweight, copyable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CobaltFile(usize, usize);
impl CobaltFile {
    /// Get the name of the file that this points to.
    pub fn name(self) -> &'static str {
        FILES.0[self.0].1[self.1].0.as_ref()
    }
    /// Get the contents of the file.
    pub fn contents(self) -> &'static str {
        FILES.0[self.0].1[self.1].1.as_ref()
    }
    /// Extract the module identifier.
    pub fn module_id(self) -> usize {
        self.0
    }
    /// Return the source location of a point
    pub fn source_loc(self, loc: usize) -> Result<(usize, usize), MietteError> {
        let file = FILES.0[self.0].1[self.1].1.as_ref();
        let span = file.read_span(&loc.into(), 0, 0)?;
        Ok((span.line(), span.column()))
    }
}
impl SourceCode for CobaltFile {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        clb: usize,
        cla: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        FILES.0[self.0].1[self.1].read_span(span, clb, cla)
    }
}
#[cfg(feature = "serde")]
impl serde::Serialize for CobaltFile {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.name())
    }
}
#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for CobaltFile {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = CobaltFile;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a module path")
            }
            fn visit_str<E: serde::de::Error>(self, name: &str) -> Result<CobaltFile, E> {
                FILES.lookup(name).ok_or_else(|| {
                    E::invalid_value(
                        serde::de::Unexpected::Str(name),
                        &"an already-defined module",
                    )
                })
            }
        }
        deserializer.deserialize_str(Visitor)
    }
}
/// The actual registry. This is where all of the files are stored
pub static FILES: FileRegistry = FileRegistry::new();
