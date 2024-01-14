use crate::*;
use std::fmt::*;
use std::io::{self, BufRead, Read, Write};
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(crate = "serde_state")]
pub struct DottedName<'src> {
    pub ids: Vec<(Cow<'src, str>, SourceSpan)>,
    pub global: bool,
}
impl<'src> DottedName<'src> {
    pub fn new(ids: Vec<(Cow<'src, str>, SourceSpan)>, global: bool) -> Self {
        DottedName { ids, global }
    }
    pub fn absolute(ids: Vec<(Cow<'src, str>, SourceSpan)>) -> Self {
        Self::new(ids, true)
    }
    pub fn relative(ids: Vec<(Cow<'src, str>, SourceSpan)>) -> Self {
        Self::new(ids, false)
    }
    pub fn local(id: (Cow<'src, str>, SourceSpan)) -> Self {
        Self::new(vec![id], false)
    }
    pub fn start(&self, len: usize) -> Self {
        DottedName {
            global: self.global,
            ids: self.ids[..(len + 1)].to_vec(),
        }
    }
    pub fn end(&self, len: usize) -> Self {
        DottedName {
            global: self.global,
            ids: self.ids[len..].to_vec(),
        }
    }
}
impl Display for DottedName<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.global {
            write!(f, ".")?
        }
        let mut count = 0;
        for (val, _) in self.ids.iter() {
            write!(f, "{}", val)?;
            count += 1;
            if count != self.ids.len() {
                write!(f, ".")?;
            }
        }
        Ok(())
    }
}
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(crate = "serde_state")]
pub enum CompoundDottedNameSegment<'src> {
    Identifier(Cow<'src, str>, SourceSpan),
    Glob(SourceSpan),
    Group(Vec<Vec<Self>>),
}
impl<'src> CompoundDottedNameSegment<'src> {
    pub fn ends_with(&self, name: &str) -> bool {
        match self {
            Self::Identifier(n, _) => n == name,
            Self::Glob(_) => true,
            Self::Group(v) => v.iter().any(|v| {
                if let Some(s) = v.last() {
                    s.ends_with(name)
                } else {
                    false
                }
            }),
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        use CompoundDottedNameSegment::*;
        match self {
            Identifier(id, _) => {
                out.write_all(&[1])?;
                out.write_all(id.as_bytes())?;
                out.write_all(&[0])
            }
            Glob(_) => out.write_all(&[2]),
            Group(groups) => {
                out.write_all(&[3])?;
                for group in groups.iter() {
                    group.iter().try_for_each(|id| id.save(out))?;
                    out.write_all(&[0])?;
                }
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(buf: &mut R) -> io::Result<Option<Self>> {
        use CompoundDottedNameSegment::*;
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        match c {
            0 => Ok(None),
            1 => {
                let mut name = vec![];
                buf.read_until(0, &mut name)?;
                if name.last() == Some(&0) {
                    name.pop();
                }
                Ok(Some(Identifier(
                    std::str::from_utf8(&name)
                        .expect("Cobalt symbols should be valid UTF-8")
                        .to_string()
                        .into(),
                    unreachable_span(),
                )))
            }
            2 => Ok(Some(Glob(unreachable_span()))),
            3 => {
                let mut out = vec![];
                loop {
                    let mut group = vec![];
                    while let Some(val) = Self::load(buf)? {
                        group.push(val);
                    }
                    if group.is_empty() {
                        break;
                    } else {
                        out.push(group);
                    }
                }
                Ok(Some(Group(out)))
            }
            x => panic!("read CDN segment expecting 0, 1, 2, or 3, got {x}"),
        }
    }
}
impl Display for CompoundDottedNameSegment<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Identifier(x, _) => write!(f, "{x}"),
            Self::Glob(_) => write!(f, "*"),
            Self::Group(x) => {
                write!(f, "{{")?;
                let mut count = x.len();
                for val in x.iter() {
                    let mut c2 = val.len();
                    for v in val.iter() {
                        write!(f, "{}", v)?;
                        if c2 != 1 {
                            write!(f, ".")?
                        }
                        c2 -= 1;
                    }
                    if count != 1 {
                        write!(f, ", ")?
                    }
                    count -= 1;
                }
                write!(f, "}}")
            }
        }
    }
}
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(crate = "serde_state")]
pub struct CompoundDottedName<'src> {
    pub ids: Vec<CompoundDottedNameSegment<'src>>,
    pub global: bool,
}
impl<'src> CompoundDottedName<'src> {
    pub fn new(ids: Vec<CompoundDottedNameSegment<'src>>, global: bool) -> Self {
        Self { ids, global }
    }
    pub fn absolute(ids: Vec<CompoundDottedNameSegment<'src>>) -> Self {
        Self::new(ids, true)
    }
    pub fn relative(ids: Vec<CompoundDottedNameSegment<'src>>) -> Self {
        Self::new(ids, false)
    }
    pub fn local(id: CompoundDottedNameSegment<'src>) -> Self {
        Self::new(vec![id], false)
    }
    pub fn ends_with(&self, name: &str) -> bool {
        if let Some(s) = self.ids.last() {
            s.ends_with(name)
        } else {
            false
        }
    }
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        out.write_all(&[if self.global { 2 } else { 1 }])?;
        self.ids.iter().try_for_each(|i| i.save(out))?;
        out.write_all(&[0])
    }
    pub fn load<R: Read + BufRead>(buf: &mut R) -> io::Result<Option<Self>> {
        let mut c = 0u8;
        if buf.read_exact(std::slice::from_mut(&mut c)).is_err() {
            return Ok(None);
        };
        match c {
            0 => Ok(None),
            1 | 2 => {
                let mut ids = vec![];
                while let Some(id) = CompoundDottedNameSegment::load(buf)? {
                    ids.push(id)
                }
                Ok(Some(Self::new(ids, c == 2)))
            }
            x => panic!("read CDN expecting 0, 1, or 2, got {x}"),
        }
    }
}
impl<'src> From<DottedName<'src>> for CompoundDottedName<'src> {
    fn from(other: DottedName<'src>) -> Self {
        Self::new(
            other
                .ids
                .into_iter()
                .map(|(id, loc)| CompoundDottedNameSegment::Identifier(id, loc))
                .collect(),
            other.global,
        )
    }
}
impl Display for CompoundDottedName<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.global {
            write!(f, ".")?
        }
        let mut count = 0;
        for val in self.ids.iter() {
            write!(f, "{}", val)?;
            count += 1;
            if count != self.ids.len() {
                write!(f, ".")?;
            }
        }
        Ok(())
    }
}
