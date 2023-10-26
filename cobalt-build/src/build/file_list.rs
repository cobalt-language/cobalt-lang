use crate::serde_utils::CowPath;

use super::*;
use wax::{BuildError, Glob, Walk, WalkEntry, WalkError};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "FileListShim")]
#[serde(into = "FileListShim")]
pub enum FileList<'a> {
    #[serde(borrow)]
    Glob(Glob<'a>),
    List(Vec<Cow<'a, Path>>),
}
impl<'a> FileList<'a> {
    pub fn into_owned(self) -> FileList<'static> {
        match self {
            FileList::Glob(pat) => FileList::Glob(pat.into_owned()),
            FileList::List(files) => {
                FileList::List(files.into_iter().map(|f| f.into_owned().into()).collect())
            }
        }
    }
    pub fn iter<'this: 'a, P: AsRef<Path>>(&'this self, base: P) -> Iter<'a, 'this> {
        match self {
            FileList::Glob(pat) => Iter(Either::Left(pat.walk(base))),
            FileList::List(pat) => Iter(Either::Right((pat.iter(), base.as_ref().into()))),
        }
    }
    pub fn into_iter<P: AsRef<Path>>(self, base: P) -> IntoIter<'a> {
        match self {
            FileList::Glob(pat) => IntoIter(Either::Left(pat.walk(base).into_owned())),
            FileList::List(pat) => IntoIter(Either::Right((pat.into_iter(), base.as_ref().into()))),
        }
    }
    pub fn is_glob(&self) -> bool {
        matches!(self, FileList::Glob(_))
    }
    pub fn is_list(&self) -> bool {
        matches!(self, FileList::List(_))
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum FileListShim<'a> {
    #[serde(borrow)]
    Glob(Cow<'a, str>),
    List(Vec<CowPath<'a>>),
}
impl<'a> From<FileList<'a>> for FileListShim<'a> {
    fn from(value: FileList<'a>) -> Self {
        match value {
            FileList::Glob(pat) => FileListShim::Glob(pat.to_string().into()),
            FileList::List(files) => FileListShim::List(unsafe { std::mem::transmute(files) }),
        }
    }
}
impl<'a> TryFrom<FileListShim<'a>> for FileList<'a> {
    type Error = BuildError;
    fn try_from(value: FileListShim<'a>) -> Result<Self, Self::Error> {
        match value {
            FileListShim::Glob(pat) => match pat {
                Cow::Borrowed(pat) => Glob::new(pat).map(FileList::Glob),
                Cow::Owned(pat) => Glob::new(&pat).map(Glob::into_owned).map(FileList::Glob),
            },
            FileListShim::List(files) => Ok(FileList::List(unsafe { std::mem::transmute(files) })),
        }
    }
}
#[derive(Debug)]
pub struct Iter<'a, 'this>(Either<Walk<'a>, (std::slice::Iter<'this, Cow<'a, Path>>, Box<Path>)>);
impl Iterator for Iter<'_, '_> {
    type Item = Result<PathBuf, WalkError>;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            Either::Left(it) => Some(it.next()?.map(WalkEntry::into_path)),
            Either::Right((it, base)) => Some(Ok(base.join(it.next()?))),
        }
    }
}

#[derive(Debug)]
pub struct IntoIter<'a>(Either<Walk<'a>, (std::vec::IntoIter<Cow<'a, Path>>, Box<Path>)>);
impl Iterator for IntoIter<'_> {
    type Item = Result<PathBuf, WalkError>;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            Either::Left(it) => Some(it.next()?.map(WalkEntry::into_path)),
            Either::Right((it, base)) => Some(Ok(base.join(it.next()?))),
        }
    }
}
