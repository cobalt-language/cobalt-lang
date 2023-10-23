use crate::*;
use anyhow::Context;
use anyhow_std::*;
use cobalt_ast::ast::*;
use cobalt_errors::miette::Severity;
use cobalt_errors::*;
use cobalt_parser::prelude::*;
use cobalt_utils::CellExt;
use either::Either;
#[cfg(feature = "gluon-build")]
use gluon_codegen::*;
use hashbrown::HashMap;
use indexmap::IndexMap;
use inkwell::targets::{FileType, Target as InkwellTarget, TargetTriple};
use os_str_bytes::OsStrBytes;
use path_calculate::*;
use semver::{Version, VersionReq};
use serde::*;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize)]
pub struct Project {
    pub name: String,
    pub version: Version,
    pub author: Option<String>,
    pub co_version: Option<VersionReq>,
    pub desc: Option<String>,
    pub targets: BTreeMap<String, Target>,
}
impl Project {
    #[cfg(feature = "gluon-build")]
    fn load_gluon<'a>(
        path: &Path,
        proj: Option<Self>,
        options: BuildOptions<'a>,
    ) -> anyhow::Result<(Option<Self>, BuildOptions<'a>)> {
        use gluon::import::add_extern_module;
        use gluon::vm::ExternModule;
        use gluon::{record, ThreadExt};
        use gluon_build::BuildConfig;
        use std::sync::Arc;
        let project = proj.unwrap_or_default();
        let file = path.read_to_string_anyhow()?;
        let vm = gluon::new_vm();
        vm.load_file("std/map.glu")?;
        // this is probably safe
        // nothing else escapes this scope
        let module = Arc::new(unsafe {
            std::mem::transmute::<_, BuildConfig<'static>>(BuildConfig { project, options })
        });
        vm.register_type::<BuildOptions<'static>>("cobalt.Options", &[])?;
        macro_rules! register_types {
            ($name:ident) => {
                vm.register_type::<$name>(concat!("cobalt.", stringify!($name)), &[])?;
            };
            ($name:ident, $($rest:ident),+) => {
                register_types!($name);
                register_types!($($rest),+);
            };
        }
        register_types!(Project, FileList, Target, PkgDepSpec, Dependency);
        add_extern_module(&vm, "cobalt", move |vm| {
            ExternModule::new(
                vm,
                record! {
                    type Project => Project,
                    type Options => BuildOptions<'static>,
                    type FileList => FileList,
                    type Target => Target,
                    type Dependency => Dependency,
                    type PkgDepSpec => PkgDepSpec,
                    values => &*module
                },
            )
        });
        vm.run_io(true);
        let BuildConfig { project, options } =
            unsafe { std::mem::transmute(vm.run_expr::<BuildConfig<'static>>("cobalt", &file)?.0) };
        Ok((Some(project), options))
    }
    pub fn load_exact<'a>(
        path: &mut PathBuf,
        mut opts: BuildOptions<'a>,
        set_src: bool,
        set_build: bool,
    ) -> anyhow::Result<(Option<Self>, BuildOptions<'a>)> {
        if path.metadata_anyhow()?.file_type().is_dir() {
            path.push("cobalt.toml");
            let mut cfg = path
                .exists()
                .then(|| -> anyhow::Result<Self> {
                    let cfg = path.read_to_string_anyhow()?;
                    toml::from_str::<Self>(&cfg).context("failed to parse project file")
                })
                .transpose()?;
            path.set_extension("glu");
            if cfg.is_some() || (path.exists() && cfg!(feature = "gluon-build")) {
                if set_src {
                    opts.source_dir = path.clone().into();
                }
                if set_build {
                    opts.source_dir = path.join("build").into();
                }
            }
            #[cfg(feature = "gluon-build")]
            if path.exists() {
                let (c, o) = Self::load_gluon(path, cfg, opts)?;
                cfg = c;
                opts = o;
            }
            path.pop();
            Ok((cfg, opts))
        } else {
            if set_src {
                opts.source_dir = path.clone().into();
            }
            if set_build {
                opts.source_dir = path.join("build").into();
            }
            let cfg = match path.extension().and_then(|e| e.to_str()) {
                Some("toml") => {
                    let cfg = path.read_to_string_anyhow()?;
                    Some(toml::from_str::<Project>(&cfg).context("failed to parse project file")?)
                }
                #[cfg(feature = "gluon-build")]
                Some("glu") => {
                    let (c, o) = Self::load_gluon(path, None, opts)?;
                    opts = o;
                    c
                }
                _ => anyhow::bail!("unknown config type for {}", path.display()),
            };
            path.pop();
            Ok((cfg, opts))
        }
    }
    pub fn load<P: Into<PathBuf>>(
        path: P,
        mut opts: BuildOptions,
        set_src: bool,
        set_build: bool,
    ) -> anyhow::Result<(Self, BuildOptions, PathBuf)> {
        let original: PathBuf = path.into();
        let mut path = original.canonicalize_anyhow()?;
        let (c, o) = Self::load_exact(&mut path, opts, set_src, set_build)?;
        if let Some(c) = c {
            return Ok((c, o, path));
        } else {
            opts = o
        }
        while path.pop() {
            let (c, o) = Self::load_exact(&mut path, opts, set_src, set_build)?;
            if let Some(c) = c {
                return Ok((c, o, path));
            } else {
                opts = o
            }
        }
        anyhow::bail!(
            "couldn't find cobalt configuration in {} or any parent directories",
            original.display()
        )
    }
}
impl Default for Project {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            version: Version::new(0, 0, 0),
            author: None,
            co_version: None,
            desc: None,
            targets: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "gluon-build", derive(VmType, Getable, Pushable))]
#[serde(untagged)]
pub enum FileList {
    Glob(String),
    List(Vec<PathBuf>),
}
impl FileList {
    pub fn validate(&self) -> Result<(), glob::PatternError> {
        if let FileList::Glob(pattern) = self {
            glob::Pattern::new(pattern).map(|_| ())
        } else {
            Ok(())
        }
    }
    pub fn has_files(&self) -> bool {
        match self {
            FileList::Glob(pattern) => {
                glob::glob(pattern).map_or(false, |mut p| p.next().is_some())
            }
            FileList::List(files) => !files.is_empty(),
        }
    }
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }
    pub fn relative_to<P: AsRef<Path>>(&mut self, root: P) {
        match self {
            FileList::Glob(pat) => *pat = root.as_ref().join(&pat).to_string_lossy().into(),
            FileList::List(files) => files.iter_mut().for_each(|p| {
                if p.is_relative() {
                    *p = root.as_ref().join(&p)
                }
            }),
        }
    }
}
impl From<String> for FileList {
    fn from(value: String) -> Self {
        Self::Glob(value)
    }
}
impl From<Vec<PathBuf>> for FileList {
    fn from(value: Vec<PathBuf>) -> Self {
        Self::List(value)
    }
}
impl IntoIterator for FileList {
    type IntoIter = Either<
        glob::Paths,
        std::iter::Map<std::vec::IntoIter<PathBuf>, fn(PathBuf) -> glob::GlobResult>,
    >;
    type Item = glob::GlobResult;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            FileList::Glob(pattern) => Either::Left(glob::glob(&pattern).unwrap()),
            FileList::List(list) => Either::Right(list.into_iter().map(Ok)),
        }
    }
}
impl<'a> IntoIterator for &'a FileList {
    type IntoIter = Either<
        std::iter::Map<glob::Paths, fn(glob::GlobResult) -> Self::Item>,
        std::iter::Map<std::slice::Iter<'a, PathBuf>, fn(&'a PathBuf) -> Self::Item>,
    >;
    type Item = Result<Cow<'a, Path>, glob::GlobError>;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            FileList::Glob(pattern) => {
                Either::Left(glob::glob(pattern).unwrap().map(|p| p.map(Cow::Owned)))
            }
            FileList::List(list) => Either::Right(list.iter().map(|p| Ok(Cow::Borrowed(p)))),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "gluon-build", derive(VmType, Getable, Pushable))]
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

#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "gluon-build", derive(VmType, Getable, Pushable))]
pub struct Target {
    pub target_type: TargetType,
    pub files: Option<FileList>,
    pub deps: BTreeMap<String, Dependency>,
}

#[derive(Deserialize)]
#[serde(rename = "target")]
struct TargetShim {
    name: String,
    #[serde(rename = "type")]
    target_type: TargetType,
    #[serde(with = "either::serde_untagged_optional")]
    files: Option<Either<String, Vec<PathBuf>>>,
    #[serde(default)]
    deps: BTreeMap<String, Dependency>,
}

#[derive(Deserialize)]
#[serde(rename = "target")]
struct KnownTargetShim {
    name: String,
    #[serde(with = "either::serde_untagged_optional")]
    files: Option<Either<String, Vec<PathBuf>>>,
    #[serde(default)]
    deps: BTreeMap<String, Dependency>,
}

impl<'de> Deserialize<'de> for Project {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::*;
        use std::fmt::{self, Formatter};
        struct ProjectVisitor;
        impl<'de> Visitor<'de> for ProjectVisitor {
            type Value = Project;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str("struct Project")
            }
            fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<Project, V::Error> {
                let mut name = None;
                let mut version = None;
                let mut author = None;
                let mut co_version = None;
                let mut desc = None;
                let mut targets = BTreeMap::new();
                enum Field {
                    Name,
                    Version,
                    Author,
                    CoVersion,
                    Desc,
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
                                            files: files.map(|e| e.either_into()),
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
                                            files: files.map(|e| e.either_into()),
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
                                            files: files.map(|e| e.either_into()),
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
                                            files: files.map(|e| e.either_into()),
                                        },
                                    )
                                },
                            ),
                        ),
                        Field::Ignore => {}
                    }
                }
                let name = name.ok_or_else(|| Error::missing_field("name"))?;
                let version = version.ok_or_else(|| Error::missing_field("version"))?;
                Ok(Project {
                    name,
                    version,
                    author,
                    co_version,
                    desc,
                    targets,
                })
            }
        }
        deserializer.deserialize_struct(
            "Project",
            &["name", "version", "author", "co_version", "desc", "targets"],
            ProjectVisitor,
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildOptions<'a> {
    pub source_dir: Cow<'a, Path>,
    pub build_dir: Cow<'a, Path>,
    pub continue_build: bool,
    pub continue_comp: bool,
    pub rebuild: bool,
    pub no_default_link: bool,
    pub triple: Cow<'a, str>,
    pub profile: Cow<'a, str>,
    pub link_dirs: Vec<Cow<'a, Path>>,
}
impl<'a> BuildOptions<'a> {
    pub fn to_static(&self) -> BuildOptions<'static> {
        BuildOptions {
            source_dir: self.source_dir.to_path_buf().into(),
            build_dir: self.build_dir.to_path_buf().into(),
            continue_build: self.continue_build,
            continue_comp: self.continue_comp,
            rebuild: self.rebuild,
            no_default_link: self.no_default_link,
            triple: self.triple.to_string().into(),
            profile: self.profile.to_string().into(),
            link_dirs: self
                .link_dirs
                .iter()
                .map(|p| p.to_path_buf().into())
                .collect(),
        }
    }
    pub fn into_static(self) -> BuildOptions<'static> {
        BuildOptions {
            source_dir: self.source_dir.into_owned().into(),
            build_dir: self.build_dir.into_owned().into(),
            continue_build: self.continue_build,
            continue_comp: self.continue_comp,
            rebuild: self.rebuild,
            no_default_link: self.no_default_link,
            triple: self.triple.into_owned().into(),
            profile: self.profile.into_owned().into(),
            link_dirs: self
                .link_dirs
                .into_iter()
                .map(|p| p.into_owned().into())
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PkgDepSpec {
    #[serde(default)]
    pub version: semver::VersionReq,
    pub targets: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "gluon-build", derive(VmType, Getable, Pushable))]
pub enum Dependency {
    Project,
    System,
    Package(PkgDepSpec),
}
impl<'de> Deserialize<'de> for Dependency {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::*;
        use std::fmt::{self, Formatter};
        struct DepVisitor;
        impl<'de> Visitor<'de> for DepVisitor {
            type Value = Dependency;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str(r#""project", "version", or a version statement"#)
            }
            fn visit_str<E: Error>(self, v: &str) -> Result<Dependency, E> {
                Ok(match v.trim() {
                    "project" => Dependency::Project,
                    "system" => Dependency::System,
                    x => Dependency::Package({
                        let mut it = v.match_indices(['@', ':']).peekable();
                        if let Some(&(next, _)) = it.peek() {
                            let mut targets: Option<Vec<String>> = None;
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
                                    ":" => targets
                                        .get_or_insert_with(Vec::new)
                                        .extend(blk.split(',').map(str::trim).map(String::from)),
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
            fn visit_map<M: MapAccess<'de>>(self, mut v: M) -> Result<Dependency, M::Error> {
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
                let mut targets: Option<Vec<String>> = None;
                let mut version: Option<VersionReq> = None;
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
        deserializer.deserialize_any(DepVisitor)
    }
}

pub fn clear_mod(this: &mut HashMap<std::borrow::Cow<str>, Symbol>) {
    for sym in this.values_mut() {
        sym.1.export = false;
        if let Some((m, ..)) = sym.0.as_mod_mut() {
            clear_mod(m);
        } else {
            let Symbol(v, d) = sym;
            if let Some(inkwell::values::BasicValueEnum::PointerValue(pv)) = v.comp_val {
                if !d.fwd && d.init {
                    unsafe {
                        std::mem::transmute::<_, inkwell::values::GlobalValue>(pv)
                            .set_linkage(inkwell::module::Linkage::AvailableExternally);
                    }
                }
            }
        }
    }
}
/// Start building a file
/// This stops after running the prepasses to allow them to be run on all files before continuing
fn build_file_1(
    path: &Path,
    ctx: &CompCtx<'static, '_>,
    opts: &BuildOptions,
    force_build: bool,
    ec: &mut usize,
) -> anyhow::Result<(([PathBuf; 2], bool), Option<TopLevelAST<'static>>)> {
    let mut out_path = opts.build_dir.to_path_buf();
    out_path.push(".artifacts");
    out_path.push("objects");
    out_path.push(path.strip_prefix(&opts.source_dir).unwrap_or(path));
    out_path.set_extension("o");
    let mut head_path = opts.build_dir.to_path_buf();
    head_path.push(".artifacts");
    head_path.push("headers");
    head_path.push(path.strip_prefix(&opts.source_dir).unwrap_or(path));
    head_path.set_extension("coh.o");
    if !(force_build || opts.rebuild)
        && out_path.exists()
        && head_path.exists()
        && (|| {
            Ok::<bool, std::io::Error>(
                path.metadata()?.modified()? < out_path.metadata()?.modified()?,
            )
        })()
        .unwrap_or(false)
    {
        // lambda to propagate errors
        let conflicts = libs::load_lib(&head_path, ctx)?;
        if !conflicts.is_empty() {
            anyhow::bail!(libs::ConflictingDefs(conflicts))
        }
        return Ok((([out_path, head_path], false), None));
    }
    let mut fail = false;
    let name = path.to_str().expect("File name must be valid UTF-8");
    ctx.module.set_name(name);
    ctx.module.set_source_file_name(name);
    let code = path.as_absolute_path().unwrap().read_to_string_anyhow()?;
    let file = FILES.add_file(0, name.to_string(), code.into());
    let (ast, errs) = parse_tl().parse(file.contents()).into_output_errors();
    let mut ast = ast.unwrap_or_default();
    let errs = errs.into_iter().flat_map(cvt_err);
    for err in errs {
        let is_err = err.severity.map_or(true, |e| e > Severity::Warning);
        if is_err {
            *ec += 1;
            fail = true;
        }
        eprintln!("{:?}", Report::from(err).with_source_code(file));
    }
    ast.file = Some(file);
    if fail && !opts.continue_comp {
        anyhow::bail!(CompileErrors(*ec))
    }
    ast.run_passes(ctx);
    Ok((([out_path, head_path], fail), Some(ast)))
}
/// Finish building the file
/// This picks up where `build_file_1` left off
fn build_file_2(
    ast: TopLevelAST<'static>,
    ctx: &CompCtx<'static, '_>,
    opts: &BuildOptions,
    (out_path, head_path): (&Path, &Path),
    mut fail: bool,
    ec: &mut usize,
) -> anyhow::Result<bool> {
    let (_, errs) = ast.codegen(ctx);
    for err in errs {
        *ec += 1;
        fail = true;
        eprintln!(
            "{:?}",
            Report::from(err).with_source_code(ast.file.unwrap())
        );
    }
    if fail && !opts.continue_comp {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        anyhow::bail!(CompileErrors(*ec))
    }
    ctx.module.verify().map_err(|m| {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        LlvmVerifierError::from(m)
    })?;
    if fail {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        return Ok(false);
    }
    let pm = inkwell::passes::PassManager::create(());
    opt::load_profile(&opts.profile, &pm);
    pm.run_on(&ctx.module);
    let trip = TargetTriple::create(&opts.triple);
    let target_machine = InkwellTarget::from_triple(&trip)
        .unwrap()
        .create_target_machine(
            &trip,
            "",
            "",
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Small,
        )
        .expect("failed to create target machine");
    if !out_path.parent().unwrap().exists() {
        out_path.parent().unwrap().create_dir_all_anyhow()?
    }
    if !head_path.parent().unwrap().exists() {
        head_path.parent().unwrap().create_dir_all_anyhow()?
    }
    target_machine
        .write_to_file(&ctx.module, FileType::Object, out_path)
        .unwrap();
    let mut obj = libs::new_object(&opts.triple);
    libs::populate_header(&mut obj, ctx);
    let mut file = BufWriter::new(std::fs::File::create(head_path)?);
    file.write_all(&obj.write()?)?;
    file.flush()?;
    ctx.with_vars(|v| clear_mod(&mut v.symbols));
    Ok(true)
}
/// Resolve dependencies for `build_target_single`
/// This replaces project dependencies with packages, checks `plan` for installation versions, and
/// assumes that all dependencies are already built
fn resolve_deps_internal(
    ctx: &CompCtx,
    cmd: &mut cc::CompileCommand,
    t: &Target,
    pkg: &str,
    v: &Version,
    plan: &IndexMap<(&str, &str), Version>,
) -> anyhow::Result<()> {
    let mut libs = vec![];
    let mut conflicts = vec![];
    let mut installed_path = cobalt_dir()?;
    installed_path.push("installed");
    for (target, version) in t.deps.iter() {
        match version {
            Dependency::Project => {
                let mut artifact = installed_path.clone();
                artifact.push(pkg);
                artifact.push(v.to_string());
                artifact.push(target);
                conflicts.append(&mut libs::load_lib(&artifact, ctx)?);
                cmd.link_abs(artifact);
            }
            Dependency::System => libs.push(target.clone()),
            Dependency::Package(spec) => spec
                .targets
                .clone()
                .unwrap_or_else(|| vec!["default".to_string()])
                .into_iter()
                .try_for_each(|tar| {
                    let mut path = installed_path.clone();
                    path.push(target);
                    path.push(plan[&(target.as_str(), tar.as_str())].to_string());
                    path.push(tar);
                    conflicts.append(&mut libs::load_lib(&path, ctx)?);
                    cmd.link_abs(path);
                    anyhow::Ok(())
                })?,
        }
    }
    if !conflicts.is_empty() {
        anyhow::bail!(libs::ConflictingDefs(conflicts))
    }
    let notfound = cmd.search_libs(libs, Some(ctx), false)?;
    if !notfound.is_empty() {
        anyhow::bail!(LibsNotFound(notfound))
    }
    Ok(())
}
/// Build a single target, for use in package installation
pub fn build_target_single(
    t: &Target,
    pkg: &str,
    name: &str,
    v: &Version,
    plan: &IndexMap<(&str, &str), Version>,
    opts: &BuildOptions,
) -> anyhow::Result<PathBuf> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let mut cc = cc::CompileCommand::new();
    cc.link_dir("$ORIGIN");
    cc.link_dirs(opts.link_dirs.iter().cloned());
    cc.no_default_link = opts.no_default_link;
    resolve_deps_internal(&ctx, &mut cc, t, pkg, v, plan)?;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            let mut num_errs = 0;
            let mut files = t // TODO: remove clone
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            cc.objs(paths.into_iter().map(|([x, _], _)| x));
            cc.output(&output);
            cc.run()?;
            Ok(output)
        }
        TargetType::Library => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            cc.lib(true);
            cc.objs(paths.into_iter().flat_map(|x| x.0));
            cc.output(&output);
            cc.run()?;
            Ok(output)
        }
        TargetType::Archive => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                paths.push(path);
                asts.push(ast);
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            let mut builder = ar::Builder::new(output.create_file_anyhow()?);
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                    builder.append_path(p2)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            Ok(output)
        }
        TargetType::Objects => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            Ok(output)
        }
        TargetType::Meta => {
            let output = opts.build_dir.join(name);
            let mut vec = Vec::new();
            for lib in &cc.abss {
                vec.extend_from_slice(&lib.to_raw_bytes());
                vec.push(0);
            }
            vec.pop();
            output.write_anyhow(vec)?;
            Ok(output)
        }
    }
}
/// Resolve dependencies for a target
/// The header is loaded into the compilation context, and it returns a tuple containing if a
/// dependency has been rebuilt and a Vec containing the files that need to be linked
fn resolve_deps(
    ctx: &CompCtx,
    cmd: &mut cc::CompileCommand,
    t: &Target,
    targets: &BTreeMap<String, (Target, CellExt<Option<PathBuf>>)>,
    opts: &BuildOptions,
) -> anyhow::Result<bool> {
    let mut out = vec![];
    let mut libs = vec![];
    let mut conflicts = vec![];
    let mut to_install = vec![];
    let mut changed = false;
    for (target, version) in t.deps.iter() {
        match version {
            Dependency::Project => {
                let (t, d) = if let Some(t) = targets.get(target.as_str()) {
                    t
                } else {
                    anyhow::bail!("target {target:?} is not a target in this project")
                };
                if d.with(|d| {
                    if let Some(a) = d {
                        libs::load_lib(a, ctx)?;
                        anyhow::Ok(true)
                    } else {
                        anyhow::Ok(false)
                    }
                })? {
                    continue;
                }
                let (c, artifact) = build_target(t, target, d, targets, opts)?;
                changed |= c;
                libs::load_lib(&artifact, ctx)?;
                cmd.link_abs(artifact);
            }
            Dependency::System => libs.push(target.clone()),
            Dependency::Package(PkgDepSpec { version, targets }) => {
                to_install.push(pkg::InstallSpec {
                    name: target.clone(),
                    version: version.clone(),
                    targets: targets.clone(),
                })
            }
        }
    }
    if !to_install.is_empty() {
        let plan = pkg::install(
            to_install.iter().cloned(),
            &pkg::InstallOptions {
                target: opts.triple.to_string(),
                ..Default::default()
            },
        )?;
        let mut installed_path = cobalt_dir()?;
        installed_path.push("installed");
        to_install
            .into_iter()
            .try_for_each(|pkg::InstallSpec { name, targets, .. }| {
                targets
                    .unwrap_or_else(|| vec!["default".to_string()])
                    .into_iter()
                    .try_for_each(|target| {
                        let mut path = installed_path.clone();
                        path.push(&name);
                        path.push(plan[&(name.as_str(), target.as_str())].to_string());
                        path.push(&target);
                        conflicts.append(&mut libs::load_lib(&path, ctx)?);
                        out.push(path);
                        anyhow::Ok(())
                    })
            })?;
    }
    if !conflicts.is_empty() {
        anyhow::bail!(libs::ConflictingDefs(conflicts))
    }
    let notfound = cmd.search_libs(libs, Some(ctx), false)?;
    if !notfound.is_empty() {
        anyhow::bail!(LibsNotFound(notfound))
    }
    Ok(changed)
}
/// Build a single target. This is used with the `build` entry function
fn build_target(
    t: &Target,
    name: &str,
    data: &CellExt<Option<PathBuf>>,
    targets: &BTreeMap<String, (Target, CellExt<Option<PathBuf>>)>,
    opts: &BuildOptions,
) -> anyhow::Result<(bool, PathBuf)> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let mut cc = cc::CompileCommand::new();
    cc.no_default_link = opts.no_default_link;
    cc.link_dirs(opts.link_dirs.iter().cloned());
    let rebuild = resolve_deps(&ctx, &mut cc, t, targets, opts)?;
    let mut changed = rebuild;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            cc.objs(paths.into_iter().map(|([x, _], _)| x));
            cc.output(&output);
            cc.run()?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
        TargetType::Library => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(libs::format_lib(name, &opts.triple, true));
            cc.lib(true);
            cc.objs(paths.into_iter().flat_map(|x| x.0));
            cc.output(&output);
            cc.run()?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
        TargetType::Archive => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            let mut builder = ar::Builder::new(output.create_file_anyhow()?);
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                    builder.append_path(p2)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            Ok((changed, output))
        }
        TargetType::Objects => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let mut files = t
                .files
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("target must have files!"))?
                .clone();
            files.relative_to(&opts.source_dir);
            anyhow::ensure!(files.has_files(), "target must have files!");
            files.validate().context("error in file glob")?;
            for file in files.iter().filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if matches!(files, FileList::Glob(_)) {
                        continue;
                    } else {
                        anyhow::bail!("couldn't find file {}", file.display())
                    }
                }
                let (path, ast) = build_file_1(&file, &ctx, opts, true, &mut num_errs)?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(name);
            Ok((changed, output))
        }
        TargetType::Meta => {
            let output = opts.build_dir.join(name.to_string() + ".meta");
            let mut vec = Vec::new();
            for lib in &cc.abss {
                vec.extend_from_slice(&lib.to_raw_bytes());
                vec.push(0);
            }
            vec.pop();
            output.write_anyhow(vec)?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
    }
}
/// Build the project. If to_build is None, all targets are built
pub fn build(
    pkg: Project,
    to_build: Option<Vec<String>>,
    opts: &BuildOptions,
) -> anyhow::Result<()> {
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            anyhow::bail!(
                r#"project has Cobalt version requirement "{v}", but Cobalt version is {}"#,
                env!("CARGO_PKG_VERSION")
            );
        }
    }
    let targets = pkg
        .targets
        .into_iter()
        .map(|(n, t)| (n, (t, CellExt::default())))
        .collect::<BTreeMap<_, (Target, CellExt<Option<PathBuf>>)>>();
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&target_name) {
                t
            } else {
                anyhow::bail!("target {target_name:?} is not a target in this project");
            };
            if data.with(|x| x.is_some()) {
                continue;
            }
            build_target(target, &target_name, data, &targets, opts)?;
        }
    } else {
        for (name, (target, data)) in targets.iter() {
            if data.with(|x| x.is_some()) {
                continue;
            }
            build_target(target, name, data, &targets, opts)?;
        }
    }
    Ok(())
}
