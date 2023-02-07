use crate::Location;
use std::fmt::*;
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DottedName {
    pub ids: Vec<(String, Location)>,
    pub global: bool
}
impl DottedName {
    pub fn new(ids: Vec<(String, Location)>, global: bool) -> Self {DottedName {ids, global}}
    pub fn absolute(ids: Vec<(String, Location)>) -> Self {Self::new(ids, true)}
    pub fn relative(ids: Vec<(String, Location)>) -> Self {Self::new(ids, false)}
    pub fn local(id: (String, Location)) -> Self {Self::new(vec![id], false)}
    pub fn start(&self, len: usize) -> Self {DottedName {global: self.global, ids: self.ids[..(len + 1)].to_vec()}}
    pub fn end(&self, len: usize) -> Self {DottedName {global: self.global, ids: self.ids[len..].to_vec()}}
}
impl Display for DottedName {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.global {write!(f, ".")?}
        let mut count = 0;
        for (val, _) in self.ids.iter() {
            write!(f, "{}", val)?;
            count += 1;
            if count != self.ids.len() {write!(f, ".")?;}
        }
        Ok(())
    }
}
#[derive(Clone, Debug)]
pub enum CompoundDottedNameSegment {
    Identifier(String, Location),
    Glob(String, Location),
    Group(Vec<Vec<CompoundDottedNameSegment>>)
}
impl Display for CompoundDottedNameSegment {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Identifier(x, _) | Self::Glob(x, _) => write!(f, "{x}"),
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
    pub fn matches(name: &DottedName) -> bool {todo!("Matching of a DottedName against a CompoundDottedName can wait")}
}
impl From<DottedName> for CompoundDottedName {
    fn from(other: DottedName) -> Self {Self::new(other.ids.into_iter().map(|(id, loc)| CompoundDottedNameSegment::Identifier(id, loc)).collect(), other.global)}
}
impl Display for CompoundDottedName {
    fn fmt(&self, f: &mut Formatter) -> Result {
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
