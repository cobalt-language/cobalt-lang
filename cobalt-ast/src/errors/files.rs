use codespan_reporting::files::{Files, SimpleFile, Error};
use std::sync::RwLock;
use std::fmt::{self, Display, Formatter};
pub struct ModuleFile<'a> {
    module: &'a str,
    file: &'a str
}
impl<'a> ModuleFile<'a> {
    pub fn new(module: &'a str, file: &'a str) -> Self {Self {module, file}}
}
impl Display for ModuleFile<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.module.is_empty() {write!(f, "{}", self.file)}
        else {write!(f, "{}//{}", self.module, self.file)}
    }
}
pub struct CobaltFiles(Vec<(String, Vec<SimpleFile<String, String>>)>);
impl CobaltFiles {
    pub fn new() -> Self {Self(vec![(String::new(), vec![])])}
    pub fn add_module(&mut self, name: String) -> usize {
        self.0.push((name, vec![]));
        self.0.len() - 1
    }
    pub fn add_file(&mut self, module: usize, name: String, body: String) -> (usize, usize) {
        let m = &mut self.0[module].1;
        m.push(SimpleFile::new(name, body));
        (module, m.len() - 1)
    }
    pub fn get_file(&self, (module, file): (usize, usize)) -> Option<&SimpleFile<String, String>> {self.0.get(module)?.1.get(file)}
    pub fn get_file_mut(&mut self, (module, file): (usize, usize)) -> Option<&mut SimpleFile<String, String>> {self.0.get_mut(module)?.1.get_mut(file)}
}
impl<'a> Files<'a> for CobaltFiles {
    type FileId = (usize, usize);
    type Name = ModuleFile<'a>;
    type Source = &'a str;
    fn name(&'a self, (module, file): (usize, usize)) -> Result<ModuleFile<'a>, Error> {
        let (m, v) = self.0.get(module).ok_or(Error::FileMissing)?;
        Ok(ModuleFile::new(&m, &v.get(file).ok_or(Error::FileMissing)?.name()))
    }
    fn source(&'a self, id: (usize, usize)) -> Result<&'a str, Error> {self.get_file(id).map_or(Err(Error::FileMissing), |f| Ok(f.source().as_str()))}
    fn line_index(&'a self, id: (usize, usize), byte_index: usize) -> Result<usize, Error> {self.get_file(id).map_or(Err(Error::FileMissing), |f| f.line_index((), byte_index))}
    fn line_range(&'a self, id: (usize, usize), byte_index: usize) -> Result<std::ops::Range<usize>, Error> {self.get_file(id).map_or(Err(Error::FileMissing), |f| f.line_range((), byte_index))}
}
lazy_static::lazy_static! {
    pub static ref FILES: RwLock<CobaltFiles> = CobaltFiles::new().into();
}
