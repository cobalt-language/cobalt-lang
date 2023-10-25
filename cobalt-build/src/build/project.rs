use super::*;
use anyhow::Context;
use hashbrown::HashSet;
use std::fmt::{self, Display, Formatter};

macro_rules! impl_project {
    ($name:ident) => {
        impl<'a> $name<'a> {
            #[cfg(feature = "config-toml")]
            pub fn from_toml(project: &str) -> Result<Self, toml::de::Error> {
                Self::deserialize(toml::Deserializer::new(project))
            }
            #[cfg(feature = "config-toml")]
            pub fn from_toml_borrowed(project: &'a str) -> Result<Self, toml::de::Error> {
                Self::deserialize(toml::Deserializer::new(project))
            }
            #[cfg(feature = "config-toml")]
            pub fn from_toml_static(project: &str) -> Result<$name<'static>, toml::de::Error> {
                $name::<'static>::deserialize(toml::Deserializer::new(project))
            }
            #[cfg(feature = "config-json")]
            pub fn from_json<'b>(project: &'b str) -> serde_json::Result<Self> {
                Ok(serde_json::from_str::<$name<'b>>(project)?.into_owned())
            }
            #[cfg(feature = "config-json")]
            pub fn from_json_borrowed(project: &'a str) -> serde_json::Result<Self> {
                serde_json::from_str(project)
            }
            #[cfg(feature = "config-json")]
            pub fn from_json_static<'b>(project: &'b str) -> serde_json::Result<$name<'static>> {
                Ok(serde_json::from_str::<$name<'b>>(project)?.into_owned())
            }

            fn load_exact_(
                path: &mut PathBuf,
                set_src: bool,
                set_build: bool,
                frag: ProjectFragment<'a>,
            ) -> anyhow::Result<Result<Self, ProjectFragment<'a>>> {
                if path.is_dir() {
                    #[allow(unused_variables)]
                    let succ = false;
                    path.push("cobalt");
                    #[cfg(feature = "config-toml")]
                    let (frag, succ) = {
                        path.set_extension("toml");
                        if path.exists() {
                            let mut cfg =
                                ProjectFragment::<'a>::from_toml(&path.read_to_string_anyhow()?)
                                    .context("failed to parse project file")?;
                            if succ {
                                cfg.merge(frag)?;
                            } else {
                                cfg.fill_from(frag);
                            }
                            if set_src {
                                cfg.source_dir = Some(if let Some(src) = cfg.source_dir {
                                    if src.is_absolute() {
                                        src
                                    } else {
                                        path.join(src).into()
                                    }
                                } else {
                                    path.clone().into()
                                });
                            }
                            if set_build {
                                cfg.build_dir = Some(if let Some(build) = cfg.build_dir {
                                    if build.is_absolute() {
                                        build
                                    } else {
                                        path.join(build).into()
                                    }
                                } else {
                                    path.join("build").into()
                                });
                            }
                            (cfg, true)
                        } else {
                            (frag, false)
                        }
                    };
                    #[cfg(feature = "config-json")]
                    let (frag, succ) = {
                        path.set_extension("json");
                        if path.exists() {
                            let mut cfg =
                                ProjectFragment::<'a>::from_json(&path.read_to_string_anyhow()?)
                                    .context("failed to parse project file")?;
                            if succ {
                                cfg.merge(frag)?;
                            } else {
                                cfg.fill_from(frag);
                            }
                            if set_src {
                                cfg.source_dir = Some(if let Some(src) = cfg.source_dir {
                                    if src.is_absolute() {
                                        src
                                    } else {
                                        path.join(src).into()
                                    }
                                } else {
                                    path.clone().into()
                                });
                            }
                            if set_build {
                                cfg.build_dir = Some(if let Some(build) = cfg.build_dir {
                                    if build.is_absolute() {
                                        build
                                    } else {
                                        path.join(build).into()
                                    }
                                } else {
                                    path.join("build").into()
                                });
                            }
                            (cfg, true)
                        } else {
                            (frag, succ)
                        }
                    };
                    path.pop();
                    Ok(if succ {
                        Ok(frag.try_into()?)
                    } else {
                        Err(frag)
                    })
                } else {
                    let cfg = match path.extension().and_then(|e| e.to_str()) {
                        #[cfg(feature = "config-toml")]
                        Some("toml") => Ok({
                            let mut cfg =
                                ProjectFragment::<'a>::from_toml(&path.read_to_string_anyhow()?)
                                    .context("failed to parse project file")?;
                            cfg.fill_from(frag);
                            if set_src {
                                cfg.source_dir = Some(if let Some(src) = cfg.source_dir {
                                    if src.is_absolute() {
                                        src
                                    } else {
                                        path.join(src).into()
                                    }
                                } else {
                                    path.clone().into()
                                });
                            }
                            if set_build {
                                cfg.build_dir = Some(if let Some(build) = cfg.build_dir {
                                    if build.is_absolute() {
                                        build
                                    } else {
                                        path.join(build).into()
                                    }
                                } else {
                                    path.join("build").into()
                                });
                            }
                            cfg.try_into()?
                        }),
                        #[cfg(feature = "config-json")]
                        Some("json") => Ok({
                            let mut cfg =
                                ProjectFragment::<'a>::from_json(&path.read_to_string_anyhow()?)
                                    .context("failed to parse project file")?;
                            cfg.fill_from(frag);
                            if set_src {
                                cfg.source_dir = Some(if let Some(src) = cfg.source_dir {
                                    if src.is_absolute() {
                                        src
                                    } else {
                                        path.join(src).into()
                                    }
                                } else {
                                    path.clone().into()
                                });
                            }
                            if set_build {
                                cfg.build_dir = Some(if let Some(build) = cfg.build_dir {
                                    if build.is_absolute() {
                                        build
                                    } else {
                                        path.join(build).into()
                                    }
                                } else {
                                    path.join("build").into()
                                });
                            }
                            cfg.try_into()?
                        }),
                        _ => anyhow::bail!("unknown config type for {}", path.display()),
                    };
                    path.pop();
                    Ok(cfg)
                }
            }
            pub fn load<P: Into<PathBuf>>(
                path: P,
                set_src: bool,
                set_build: bool,
                mut frag: ProjectFragment<'a>,
            ) -> anyhow::Result<(Self, PathBuf)> {
                let original = path.into();
                let mut path = original.canonicalize_anyhow()?;
                match Self::load_exact_(&mut path, set_src, set_build, frag)? {
                    Ok(c) => return Ok((c, path)),
                    Err(f) => frag = f,
                }
                while path.pop() {
                    match Self::load_exact_(&mut path, set_src, set_build, frag)? {
                        Ok(c) => return Ok((c, path)),
                        Err(f) => frag = f,
                    }
                }
                anyhow::bail!(
                    "couldn't find cobalt configuration in {} or any parent directories",
                    original.display()
                )
            }
            pub fn load_exact<P: Into<PathBuf>>(
                path: P,
                set_src: bool,
                set_build: bool,
                frag: ProjectFragment<'a>,
            ) -> anyhow::Result<Self> {
                let mut path = path.into();
                Self::load_exact_(&mut path, set_src, set_build, frag).and_then(|r| {
                    r.map_err(|_| {
                        anyhow::anyhow!("couldn't find cobalt configuration in {}", path.display())
                    })
                })
            }
        }
    };
}

#[derive(Debug, Default, Clone, Copy, Error)]
pub struct MissingFields {
    pub name: bool,
    pub version: bool,
    pub source_dir: bool,
    pub build_dir: bool,
}
impl Display for MissingFields {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match (self.name, self.version) {
            (true, false) => f.write_str("missing name in project fragment"),
            (false, true) => f.write_str("missing version in project fragment"),
            (true, true) => f.write_str("missing name and version in project fragment"),
            (false, false) => {
                panic!("MissingFields should not be an error if all fields are true!")
            }
        }
    }
}
#[derive(Debug, Default, Clone, Error)]
pub struct FragmentMergeError {
    pub name: bool,
    pub version: bool,
    pub desc: bool,
    pub source_dir: bool,
    pub build_dir: bool,
    pub targets: Box<[Box<str>]>,
}
impl Display for FragmentMergeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("duplicate fields when merging project fragments")?;
        if self.name {
            f.write_str("\n- name")?;
        }
        if self.version {
            f.write_str("\n- version")?;
        }
        if self.desc {
            f.write_str("\n- desc")?;
        }
        if self.source_dir {
            f.write_str("\n- source_dir")?;
        }
        if self.build_dir {
            f.write_str("\n- build_dir")?;
        }
        for target in self.targets.iter() {
            write!(f, "\n- targets.{target}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Project<'a> {
    pub name: Cow<'a, str>,
    pub version: Version,
    pub author: Option<Cow<'a, str>>,
    pub co_version: Option<VersionReq>,
    pub desc: Option<Cow<'a, str>>,
    pub source_dir: Cow<'a, Path>,
    pub build_dir: Cow<'a, Path>,
    pub targets: HashMap<Cow<'a, str>, Target<'a>>,
}
impl<'a> Project<'a> {
    pub fn into_owned(self) -> Project<'static> {
        Project {
            name: self.name.into_owned().into(),
            author: self.author.map(|a| a.into_owned().into()),
            desc: self.desc.map(|d| d.into_owned().into()),
            source_dir: self.source_dir.into_owned().into(),
            build_dir: self.build_dir.into_owned().into(),
            targets: self
                .targets
                .into_iter()
                .map(|(k, v)| (k.into_owned().into(), v.into_owned()))
                .collect(),
            ..self
        }
    }
}
impl<'a, 'de: 'a> Deserialize<'de> for Project<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = ProjectFragment::deserialize(deserializer)?;
        Self::try_from(value).map_err(|e| {
            use serde::de::Error;
            D::Error::missing_field(if e.name {
                "name"
            } else if e.version {
                "version"
            } else if e.source_dir {
                "source_dir"
            } else if e.build_dir {
                "build_dir"
            } else {
                unreachable!()
            })
        })
    }
}
impl<'a> TryFrom<ProjectFragment<'a>> for Project<'a> {
    type Error = MissingFields;
    fn try_from(value: ProjectFragment<'a>) -> Result<Self, Self::Error> {
        let err = MissingFields {
            name: value.name.is_none(),
            version: value.version.is_none(),
            source_dir: value.source_dir.is_none(),
            build_dir: value.build_dir.is_none(),
        };
        Ok(Project {
            name: value.name.ok_or(err)?,
            version: value.version.ok_or(err)?,
            author: value.author,
            co_version: value.co_version,
            desc: value.desc,
            source_dir: value.source_dir.ok_or(err)?,
            build_dir: value.build_dir.ok_or(err)?,
            targets: value.targets,
        })
    }
}
impl<'a> From<Project<'a>> for ProjectFragment<'a> {
    fn from(value: Project<'a>) -> Self {
        ProjectFragment {
            name: Some(value.name),
            version: Some(value.version),
            author: value.author,
            co_version: value.co_version,
            desc: value.desc,
            source_dir: Some(value.source_dir),
            build_dir: Some(value.build_dir),
            targets: value.targets,
        }
    }
}
impl_project!(Project);

#[derive(Debug, Default, Clone, Serialize)]
pub struct ProjectFragment<'a> {
    pub name: Option<Cow<'a, str>>,
    pub version: Option<Version>,
    pub author: Option<Cow<'a, str>>,
    pub co_version: Option<VersionReq>,
    pub desc: Option<Cow<'a, str>>,
    pub source_dir: Option<Cow<'a, Path>>,
    pub build_dir: Option<Cow<'a, Path>>,
    pub targets: HashMap<Cow<'a, str>, Target<'a>>,
}
impl<'a> ProjectFragment<'a> {
    pub fn into_owned(self) -> ProjectFragment<'static> {
        ProjectFragment {
            name: self.name.map(|n| n.into_owned().into()),
            author: self.author.map(|a| a.into_owned().into()),
            desc: self.desc.map(|d| d.into_owned().into()),
            source_dir: self.source_dir.map(|s| s.into_owned().into()),
            build_dir: self.build_dir.map(|b| b.into_owned().into()),
            targets: self
                .targets
                .into_iter()
                .map(|(k, v)| (k.into_owned().into(), v.into_owned()))
                .collect(),
            ..self
        }
    }
    pub fn merge(&mut self, other: Self) -> Result<(), FragmentMergeError> {
        let mut err = FragmentMergeError::default();
        let mut is_err = false;
        if self.name.is_some() {
            if other.name.is_some() && self.name != other.name {
                err.name = true;
                is_err = true;
            }
        } else {
            self.name = other.name;
        }
        if self.version.is_some() {
            if other.version.is_some() && self.version != other.version {
                err.version = true;
                is_err = true;
            }
        } else {
            self.version = other.version;
        }
        self.author = match (std::mem::take(&mut self.author), other.author) {
            (Some(l), Some(r)) => Some(l + ", " + r),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        };
        self.co_version = match (std::mem::take(&mut self.co_version), other.co_version) {
            (Some(mut l), Some(mut r)) => {
                l.comparators.append(&mut r.comparators);
                Some(l)
            }
            (Some(c), None) | (None, Some(c)) => Some(c),
            (None, None) => None,
        };
        if self.desc.is_some() {
            if other.desc.is_some() && self.desc != other.desc {
                err.desc = true;
                is_err = true;
            }
        } else {
            self.desc = other.desc;
        }
        if self.source_dir.is_some() {
            if other.source_dir.is_some() {
                err.source_dir = true;
                is_err = true;
            }
        } else {
            self.source_dir = other.source_dir;
        }
        if self.build_dir.is_some() {
            if other.build_dir.is_some() {
                err.build_dir = true;
                is_err = true;
            }
        } else {
            self.build_dir = other.build_dir;
        }
        let t1 = self.targets.keys().map(|k| &**k).collect::<HashSet<&str>>();
        let t2 = other
            .targets
            .keys()
            .map(|k| &**k)
            .collect::<HashSet<&str>>();
        err.targets = t1.intersection(&t2).copied().map(Box::from).collect();
        is_err |= !err.targets.is_empty();
        is_err.then_some(()).ok_or(err)
    }
    pub fn into_merge(mut self, other: Self) -> Result<Self, FragmentMergeError> {
        self.merge(other)?;
        Ok(self)
    }
    pub fn overwrite_from(&mut self, other: Self) {
        self.name = self.name.take().or(other.name);
        self.version = self.version.take().or(other.version);
        self.author = match (std::mem::take(&mut self.author), other.author) {
            (Some(l), Some(r)) => Some(l + ", " + r),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        };
        self.co_version = match (std::mem::take(&mut self.co_version), other.co_version) {
            (Some(mut l), Some(mut r)) => {
                l.comparators.append(&mut r.comparators);
                Some(l)
            }
            (Some(c), None) | (None, Some(c)) => Some(c),
            (None, None) => None,
        };
        self.source_dir = self.source_dir.take().or(other.source_dir);
        self.build_dir = self.build_dir.take().or(other.build_dir);
        self.targets.extend(other.targets);
    }
    pub fn fill_from(&mut self, other: Self) {
        self.name = other.name.or(self.name.take());
        self.version = other.version.or(self.version.take());
        self.author = match (std::mem::take(&mut self.author), other.author) {
            (Some(l), Some(r)) => Some(l + ", " + r),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        };
        self.co_version = match (std::mem::take(&mut self.co_version), other.co_version) {
            (Some(mut l), Some(mut r)) => {
                l.comparators.append(&mut r.comparators);
                Some(l)
            }
            (Some(c), None) | (None, Some(c)) => Some(c),
            (None, None) => None,
        };
        self.source_dir = other.source_dir.or(self.source_dir.take());
        self.build_dir = other.build_dir.or(self.build_dir.take());
        let mut targets = other.targets;
        targets.extend(self.targets.drain());
        self.targets = targets;
    }
    pub fn set_dirs<P: Into<PathBuf>>(&mut self, path: P) {
        match (self.source_dir.is_none(), self.build_dir.is_none()) {
            (false, false) => {}
            (false, true) => {
                let mut p = path.into();
                p.push("build");
                self.build_dir = Some(Cow::Owned(p))
            }
            (true, false) => self.source_dir = Some(Cow::Owned(path.into())),
            (true, true) => {
                let path = path.into();
                self.build_dir = Some(path.join("build").into());
                self.source_dir = Some(path.into());
            }
        }
    }
    pub fn get_name(&self) -> Result<&str, MissingFields> {
        self.name.as_deref().ok_or(build::MissingFields {
            name: true,
            ..Default::default()
        })
    }
}
impl<'a, 'de: 'a> Deserialize<'de> for ProjectFragment<'a> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::*;
        struct ProjectVisitor<'a>(PhantomData<&'a ()>);
        impl<'a, 'de: 'a> Visitor<'de> for ProjectVisitor<'a> {
            type Value = ProjectFragment<'a>;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str("struct Project")
            }
            fn visit_map<V: MapAccess<'de>>(
                self,
                mut map: V,
            ) -> Result<ProjectFragment<'a>, V::Error> {
                let mut name = None;
                let mut version = None;
                let mut author = None;
                let mut co_version = None;
                let mut desc = None;
                let mut source_dir = None;
                let mut build_dir = None;
                let mut targets = HashMap::new();
                enum Field {
                    Name,
                    Version,
                    Author,
                    CoVersion,
                    Desc,
                    SourceDir,
                    BuildDir,
                    Targets,
                    Bin,
                    Lib,
                    Meta,
                    Ignore,
                }
                impl<'de> Deserialize<'de> for Field {
                    fn deserialize<D: Deserializer<'de>>(
                        deserializer: D,
                    ) -> Result<Field, D::Error> {
                        struct FieldVisitor;
                        impl<'de> Visitor<'de> for FieldVisitor {
                            type Value = Field;
                            fn expecting(&self, f: &mut Formatter<'_>) -> fmt::Result {
                                f.write_str("field identifier")
                            }
                            fn visit_str<E: Error>(self, v: &str) -> Result<Field, E> {
                                Ok(match v {
                                    "name" => Field::Name,
                                    "version" => Field::Version,
                                    "author" => Field::Author,
                                    "co_version" | "co-version" | "cobalt" => Field::CoVersion,
                                    "desc" | "description" => Field::Desc,
                                    "src" | "source" | "src_dir" | "src-dir" | "source_dir"
                                    | "source-dir" => Field::SourceDir,
                                    "build" | "build_dir" | "build-dir" => Field::BuildDir,
                                    "target" | "targets" => Field::Targets,
                                    "bin" | "binary" | "exe" | "executable" => Field::Bin,
                                    "lib" | "library" => Field::Lib,
                                    "meta" => Field::Meta,
                                    _ => Field::Ignore,
                                })
                            }
                        }
                        deserializer.deserialize_identifier(FieldVisitor)
                    }
                }
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Name => {
                            if name.is_some() {
                                return Err(Error::duplicate_field("name"));
                            } else {
                                name = Some(map.next_value()?)
                            }
                        }
                        Field::Version => {
                            if version.is_some() {
                                return Err(Error::duplicate_field("version"));
                            } else {
                                version = Some(map.next_value()?)
                            }
                        }
                        Field::Author => {
                            if author.is_some() {
                                return Err(Error::duplicate_field("author"));
                            } else {
                                author = Some(map.next_value()?)
                            }
                        }
                        Field::CoVersion => {
                            if co_version.is_some() {
                                return Err(Error::duplicate_field("co_version"));
                            } else {
                                co_version = Some(map.next_value()?)
                            }
                        }
                        Field::Desc => {
                            if desc.is_some() {
                                return Err(Error::duplicate_field("desc"));
                            } else {
                                desc = Some(map.next_value()?)
                            }
                        }
                        Field::SourceDir => {
                            if source_dir.is_some() {
                                return Err(Error::duplicate_field("source_dir"));
                            } else {
                                source_dir = Some(map.next_value()?)
                            }
                        }
                        Field::BuildDir => {
                            if build_dir.is_some() {
                                return Err(Error::duplicate_field("build_dir"));
                            } else {
                                build_dir = Some(map.next_value()?)
                            }
                        }
                        Field::Targets => targets.extend(
                            &mut map.next_value::<Vec<TargetShim>>()?.into_iter().map(
                                |TargetShim {
                                     name,
                                     target_type,
                                     deps,
                                     files,
                                 }| {
                                    (
                                        name,
                                        Target {
                                            target_type,
                                            deps,
                                            files,
                                        },
                                    )
                                },
                            ),
                        ),
                        Field::Bin => targets.extend(
                            &mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(
                                |KnownTargetShim { name, deps, files }| {
                                    (
                                        name,
                                        Target {
                                            target_type: TargetType::Executable,
                                            deps,
                                            files,
                                        },
                                    )
                                },
                            ),
                        ),
                        Field::Lib => targets.extend(
                            &mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(
                                |KnownTargetShim { name, deps, files }| {
                                    (
                                        name,
                                        Target {
                                            target_type: TargetType::Library,
                                            deps,
                                            files,
                                        },
                                    )
                                },
                            ),
                        ),
                        Field::Meta => targets.extend(
                            &mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(
                                |KnownTargetShim { name, deps, files }| {
                                    (
                                        name,
                                        Target {
                                            target_type: TargetType::Meta,
                                            deps,
                                            files,
                                        },
                                    )
                                },
                            ),
                        ),
                        Field::Ignore => {}
                    }
                }
                Ok(ProjectFragment {
                    name,
                    version,
                    author,
                    co_version,
                    source_dir,
                    build_dir,
                    desc,
                    targets,
                })
            }
        }
        deserializer.deserialize_struct(
            "Project",
            &[
                "name",
                "version",
                "author",
                "co_version",
                "desc",
                "source_dir",
                "target_dir",
                "targets",
            ],
            ProjectVisitor(PhantomData),
        )
    }
}
impl_project!(ProjectFragment);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TargetType {
    #[serde(rename = "exe", alias = "executable", alias = "bin", alias = "binary")]
    Executable,
    #[serde(rename = "lib", alias = "library")]
    Library,
    #[serde(rename = "archive", alias = "static", alias = "static-lib")]
    Archive,
    #[serde(
        rename = "obj",
        alias = "object",
        alias = "objects",
        alias = "artifacts"
    )]
    Objects,
    #[serde(rename = "meta")]
    Meta,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PkgDepSpec<'a> {
    #[serde(default)]
    pub version: semver::VersionReq,
    pub targets: Option<Vec<Cow<'a, str>>>,
}
impl PkgDepSpec<'_> {
    pub fn into_owned(self) -> PkgDepSpec<'static> {
        PkgDepSpec {
            version: self.version,
            targets: self
                .targets
                .map(|t| t.into_iter().map(|v| v.into_owned().into()).collect()),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum Dependency<'a> {
    Project,
    System,
    Package(PkgDepSpec<'a>),
}
impl Dependency<'_> {
    pub fn into_owned(self) -> Dependency<'static> {
        match self {
            Dependency::Project => Dependency::Project,
            Dependency::System => Dependency::System,
            Dependency::Package(spec) => Dependency::Package(spec.into_owned()),
        }
    }
}
impl<'a, 'de: 'a> Deserialize<'de> for Dependency<'a> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::*;
        struct DepVisitor<'a>(PhantomData<&'a ()>);
        impl<'a, 'de: 'a> Visitor<'de> for DepVisitor<'a> {
            type Value = Dependency<'a>;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str(r#""project", "version", or a version statement"#)
            }
            fn visit_str<E: Error>(self, v: &str) -> Result<Dependency<'a>, E> {
                Ok(match v.trim() {
                    "project" => Dependency::Project,
                    "system" => Dependency::System,
                    x => Dependency::Package({
                        let mut it = v.match_indices(['@', ':']).peekable();
                        if let Some(&(next, _)) = it.peek() {
                            let mut targets = None;
                            let mut version =
                                VersionReq::parse(&v[..next]).map_err(Error::custom)?;
                            while let Some((idx, ch)) = it.next() {
                                let blk = if let Some(&(next, _)) = it.peek() {
                                    v[idx..next].trim()
                                } else {
                                    v[idx..].trim()
                                };
                                match ch {
                                    "@" => version.comparators.extend(
                                        VersionReq::parse(blk).map_err(Error::custom)?.comparators,
                                    ),
                                    ":" => targets.get_or_insert_with(Vec::new).extend(
                                        blk.split(',')
                                            .map(str::trim)
                                            .map(String::from)
                                            .map(Cow::Owned),
                                    ),
                                    x => unreachable!("expected '@' or ':', got {x:?}"),
                                }
                            }
                            PkgDepSpec { targets, version }
                        } else {
                            PkgDepSpec {
                                targets: None,
                                version: VersionReq::parse(x).map_err(Error::custom)?,
                            }
                        }
                    }),
                })
            }
            fn visit_map<M: MapAccess<'de>>(self, mut v: M) -> Result<Dependency<'a>, M::Error> {
                enum Field {
                    Version,
                    Targets,
                }
                impl<'de> Deserialize<'de> for Field {
                    fn deserialize<D: Deserializer<'de>>(
                        deserializer: D,
                    ) -> Result<Field, D::Error> {
                        struct FieldVisitor;
                        impl<'de> Visitor<'de> for FieldVisitor {
                            type Value = Field;
                            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                                f.write_str("`version` or `targets`")
                            }
                            fn visit_str<E: Error>(self, v: &str) -> Result<Field, E> {
                                match v {
                                    "version" => Ok(Field::Version),
                                    "targets" => Ok(Field::Targets),
                                    _ => Err(Error::unknown_field(v, &["version", "targets"])),
                                }
                            }
                        }
                        deserializer.deserialize_identifier(FieldVisitor)
                    }
                }
                let mut targets = None;
                let mut version = None;
                while let Some(key) = v.next_key()? {
                    match key {
                        Field::Version => {
                            if version.is_some() {
                                return Err(Error::duplicate_field("version"));
                            } else {
                                version = Some(v.next_value()?)
                            }
                        }
                        Field::Targets => {
                            if targets.is_some() {
                                return Err(Error::duplicate_field("targets"));
                            } else {
                                targets = Some(v.next_value()?)
                            }
                        }
                    }
                }
                let version = version.ok_or_else(|| Error::missing_field("version"))?;
                Ok(Dependency::Package(PkgDepSpec { targets, version }))
            }
        }
        deserializer.deserialize_any(DepVisitor(PhantomData))
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Target<'a> {
    pub target_type: TargetType,
    pub files: Option<FileList<'a>>,
    pub deps: HashMap<Cow<'a, str>, Dependency<'a>>,
}
impl Target<'_> {
    pub fn into_owned(self) -> Target<'static> {
        Target {
            target_type: self.target_type,
            files: self.files.map(FileList::into_owned),
            deps: self
                .deps
                .into_iter()
                .map(|(k, v)| (k.into_owned().into(), v.into_owned()))
                .collect(),
        }
    }
}

#[derive(Deserialize)]
#[serde(rename = "target", bound = "'de: 'a")]
struct TargetShim<'a> {
    name: Cow<'a, str>,
    #[serde(rename = "type")]
    target_type: TargetType,
    files: Option<FileList<'a>>,
    #[serde(default)]
    deps: HashMap<Cow<'a, str>, Dependency<'a>>,
}

#[derive(Deserialize)]
#[serde(rename = "target", bound = "'de: 'a")]
struct KnownTargetShim<'a> {
    name: Cow<'a, str>,
    files: Option<FileList<'a>>,
    #[serde(default)]
    deps: HashMap<Cow<'a, str>, Dependency<'a>>,
}
