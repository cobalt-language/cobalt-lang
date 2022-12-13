use std::fmt::*;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DottedName {
    pub ids: Vec<String>,
    pub global: bool
}
impl DottedName {
    pub fn new(ids: Vec<String>, global: bool) -> Self {Self {ids, global}}
    pub fn absolute(ids: Vec<String>) -> Self {Self::new(ids, true)}
    pub fn relative(ids: Vec<String>) -> Self {Self::new(ids, false)}
    pub fn local(id: String) -> Self {Self::new(vec![id], false)}
}
impl Display for DottedName {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.global {write!(f, ".")?}
        let mut count = 0;
        for val in self.ids.iter() {
            write!(f, "{}", val)?;
            count += 1;
            if count != self.ids.len() {write!(f, ".")?;}
        }
        Ok(())
    }
}
#[derive(Clone, Debug)]
pub enum CompoundDottedNameSegment {
    Identifier(String),
    Glob(String),
    Group(Vec<Vec<CompoundDottedNameSegment>>)
}
impl Display for CompoundDottedNameSegment {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Identifier(x) | Self::Glob(x) => write!(f, "{x}"),
            Self::Group(x) => {
                write!(f, "{{")?;
                let mut count = x.len();
                for val in x.iter() {
                    let mut c2 = val.len();
                    for v in val.iter() {
                        write!(f, "{}", v)?;
                        if c2 != 1 {write!(f, ".")?}
                        c2 -= 1;
                    }
                    if count != 1 {write!(f, ", ")?}
                    count -= 1;
                }
                write!(f, "}}")
            }
        }
    }
}
#[derive(Clone, Debug)]
pub struct CompoundDottedName {
    pub ids: Vec<CompoundDottedNameSegment>,
    pub global: bool
}
impl CompoundDottedName {
    pub fn new(ids: Vec<CompoundDottedNameSegment>, global: bool) -> Self {Self {ids, global}}
    pub fn absolute(ids: Vec<CompoundDottedNameSegment>) -> Self {Self::new(ids, true)}
    pub fn relative(ids: Vec<CompoundDottedNameSegment>) -> Self {Self::new(ids, false)}
    pub fn local(id: CompoundDottedNameSegment) -> Self {Self::new(vec![id], false)}
}
impl From<DottedName> for CompoundDottedName {
    fn from(other: DottedName) -> Self {Self::new(other.ids.into_iter().map(|x| CompoundDottedNameSegment::Identifier(x)).collect(), other.global)}
}
impl Display for CompoundDottedName {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.global {write!(f, ".")?}
        let mut count = 0;
        for val in self.ids.iter() {
            write!(f, "{}", val)?;
            count += 1;
            if count == self.ids.len() {write!(f, ".")?;}
        }
        Ok(())
    }
}