#![allow(dead_code)]
use crate::*;
use anyhow_std::*;
use cobalt_errors::error;
use either::Either;
use indexmap::IndexMap;
use path_calculate::path_absolutize::Absolutize;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::path::{Path, PathBuf};
use thiserror::Error;
/// Information about a cloned repository
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitInfo {
    #[serde(alias = "path")]
    url: String, // URL/path of repository
    #[serde(flatten)]
    branch: Option<GitLocation>, // branch/commit/tag to use
    dir: Option<PathBuf>, // subdirectory
}
/// What to checkout to
/// Branches are updated, commits aren't
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitLocation {
    #[serde(rename = "branch")]
    Branch(String),
    #[serde(rename = "commit", alias = "tag")]
    Commit(String),
}
/// Location of a registry
/// This defines what happens when update() is called
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegistryType {
    #[serde(rename = "git", with = "either::serde_untagged")]
    Git(Either<String, GitInfo>),
    #[serde(rename = "tar", alias = "tarball", alias = "tgz")]
    Tar(String), // URL/path of .tar.gz archive
    #[serde(rename = "zip", alias = "zipball")]
    Zip(String), // URL/path of .zip archive
    #[serde(rename = "cmd", alias = "shell")]
    Cmd(String), // Command to run to update
    #[serde(rename = "manual", alias = "other")]
    Manual, // Something we don't have to worry about handles it
}
/// The actual registry: its type and path
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registry {
    #[serde(flatten)]
    pub reg_type: RegistryType,
    pub path: PathBuf,
}
/// Shim for serde
/// Registries are supposed to be defined as a list of tables in TOML, so this helps
/// deserialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryList {
    #[serde(default)]
    pub registry: Vec<Registry>,
}
/// A general structure describing how to install a project
/// While Projects describe how to build a project, Packages define how to install it, storing
/// releases and that contain the source and prebuilds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub author: String,
    #[serde(alias = "description")]
    pub desc: Option<String>,
    pub releases: BTreeMap<Version, Release>,
}
/// A single version of a project
/// Every release must have a source, but can also optionally have prebuilds and a separate
/// manifest download to speed up its usage.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Release {
    pub source: Source,
    /// A map of Arch => Target => URL
    /// The URL should be to download the uncompressed output
    pub prebuilds: HashMap<String, HashMap<String, String>>,
    /// An optional URL to download just the `cobalt.toml` file
    #[serde(alias = "proj_file", alias = "proj-file")]
    pub project: Option<String>,
}
impl Release {
    /// Get the manifest for this release
    pub fn project(
        &self,
        name: &str,
        version: &Version,
        frozen: bool,
    ) -> anyhow::Result<build::Project> {
        let mut path = cobalt_dir()?;
        path.push("packages");
        path.push(name);
        path.push(version.to_string());
        if !path.exists() {
            path.create_dir_all_anyhow()?
        }
        path.push("cobalt.toml");
        if path.exists() {
            return Ok(toml::from_str(&path.read_to_string_anyhow()?)?);
        }
        path.pop();
        path.push("src");
        path.push("cobalt.toml");
        if path.exists() {
            return Ok(toml::from_str(&path.read_to_string_anyhow()?)?);
        }
        path.pop();
        path.pop();
        path.push("cobalt.toml");
        if let Some(mut project) = self.project.as_deref() {
            if if project.starts_with("file://") {
                project = &project[7..];
                true
            } else {
                project.starts_with('/')
            } {
                Path::new(project).copy_anyhow(&path)?;
                return Ok(toml::from_str(&path.read_to_string_anyhow()?)?);
            } else if !frozen {
                let buf = ureq::get(project).call()?.into_string()?;
                path.write_anyhow(&buf)?;
                return Ok(toml::from_str(&buf)?);
            }
        }
        path.pop();
        path.push("src");
        if !(frozen || path.exists()) {
            self.source.install(&path)?;
            path.push("cobalt.toml");
            return Ok(toml::from_str(&path.read_to_string_anyhow()?)?);
        }
        Err(InstallError::Frozen(name.to_string()).into())
    }
}
/// How to download a project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Source {
    #[serde(rename = "git", with = "either::serde_untagged")]
    Git(Either<String, GitInfo>),
    #[serde(rename = "tar", alias = "tarball", alias = "tgz")]
    Tar(String),
    #[serde(rename = "zip", alias = "zipball")]
    Zip(String),
}
impl Source {
    /// Install the source to a given location
    /// The returned `PathBuf` is in case the target was a subdirectory of a Git repository
    pub fn install<P: AsRef<Path>>(&self, to: P) -> anyhow::Result<PathBuf> {
        let mut to = to.as_ref().to_path_buf();
        if to.exists() {
            to.remove_dir_all_anyhow()?
        }
        match self {
            Self::Git(
                Either::Left(url)
                | Either::Right(GitInfo {
                    url, branch: None, ..
                }),
            ) => {
                git2::Repository::clone_recurse(url, &to)?;
            }
            Self::Git(Either::Right(GitInfo {
                url,
                branch: Some(GitLocation::Branch(commit) | GitLocation::Commit(commit)),
                ..
            })) => {
                let repo = git2::Repository::clone_recurse(url, &to)?;
                let (object, reference) = repo.revparse_ext(commit)?;
                repo.checkout_tree(&object, None)?;
                if let Some(gref) = reference {
                    repo.set_head(std::str::from_utf8(gref.name_bytes())?)?
                } else {
                    repo.set_head_detached(object.id())?
                }
            }
            Self::Tar(url) => tar::Archive::new(flate2::read::GzDecoder::new(
                ureq::get(url).call()?.into_reader(),
            ))
            .unpack(&to)?,
            Self::Zip(url) => {
                let mut buf = vec![];
                ureq::get(url).call()?.into_reader().read_to_end(&mut buf)?;
                zip_extract::extract(std::io::Cursor::new(buf), &to, true)?;
            }
        }
        if let Self::Git(Either::Right(GitInfo { dir: Some(p), .. })) = self {
            to.push(p)
        }
        Ok(to)
    }
}
lazy_static::lazy_static! {
    pub static ref REGISTRY: Vec<Package> = get_packages().map_err(|e| {error!("{e:#}"); std::process::exit(101)}).unwrap();
}
/// Get all packages as a `Vec`
pub fn get_packages() -> anyhow::Result<Vec<Package>> {
    let cdir = cobalt_dir()?;
    let reg_path = cdir.join("registries.toml");
    if !reg_path.exists() {
        return Ok(vec![]);
    }
    let registries = toml::from_str::<RegistryList>(&reg_path.read_to_string_anyhow()?)?;
    let mut out = vec![];
    let reg_dir = cdir.join("registries");
    reg_dir.create_dir_all_anyhow()?;
    for reg in registries.registry {
        let mut path = reg.path.absolutize_from(&reg_dir)?.to_path_buf();
        if let RegistryType::Git(Either::Right(GitInfo {
            dir: Some(ref dir), ..
        })) = reg.reg_type
        {
            path.push(dir)
        }
        let it = path.read_dir_anyhow()?;
        let bounds = it.size_hint();
        if let Some(upper) = bounds.1 {
            out.reserve(upper);
        } else {
            out.reserve(bounds.0);
        }
        for entry in it {
            out.push(toml::from_str::<Package>(
                &entry?.path().read_to_string_anyhow()?,
            )?);
        }
    }
    Ok(out)
}
/// Update all registries
pub fn update_packages() -> anyhow::Result<()> {
    let cdir = cobalt_dir()?;
    let reg_path = cdir.join("registries.toml");
    if !reg_path.exists() {
        return Ok(());
    }
    let registries = toml::from_str::<RegistryList>(&reg_path.read_to_string_anyhow()?)?;
    let reg_dir = cdir.join("registries");
    reg_dir.create_dir_all_anyhow()?;
    for reg in registries.registry {
        let path = reg.path.absolutize_from(&reg_dir)?;
        use RegistryType::*;
        match reg.reg_type {
            Git(
                Either::Left(url)
                | Either::Right(GitInfo {
                    url, branch: None, ..
                }),
            ) => {
                if path.exists() {
                    let repo = git2::Repository::open(&path)?;
                    let remotes = repo.remotes()?;
                    let mut it = remotes.iter();
                    if let Some(remote_name) = it.next() {
                        if it.next().is_some() {
                            anyhow::bail!(
                                "multiple remotes are available for repository at {}",
                                path.display()
                            );
                        }
                        if let Some(remote_name) = remote_name {
                            let mut remote = repo.find_remote(remote_name)?;
                            let name_bytes = remote.default_branch()?;
                            let name = std::str::from_utf8(&name_bytes)?;
                            remote.fetch(&[name], None, None)?;
                            let fetch_head = repo.find_reference("FETCH_HEAD")?;
                            let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
                            let analysis = repo.merge_analysis(&[&fetch_commit])?;
                            if analysis.0.is_up_to_date() {
                                return anyhow::Ok(());
                            } else if analysis.0.is_fast_forward() {
                                let refname = format!("refs/heads/{name}");
                                let mut reference = repo.find_reference(&refname)?;
                                reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                                repo.set_head(&refname)?;
                                repo.checkout_head(Some(
                                    git2::build::CheckoutBuilder::default().force(),
                                ))?;
                            } else {
                                anyhow::bail!(
                                    "remote {remote_name} in repo {} is not fast-forward",
                                    path.display()
                                )
                            }
                        } else {
                            anyhow::bail!("remote name is not UTF-8")
                        }
                    } else {
                        anyhow::bail!("no remotes are avaiable in {}", path.display())
                    }
                } else {
                    git2::Repository::clone_recurse(&url, &path)?;
                }
            }
            Git(Either::Right(GitInfo {
                url,
                branch: Some(GitLocation::Commit(commit)),
                ..
            })) => {
                if !path.exists() {
                    let repo = git2::Repository::clone_recurse(&url, &path)?;
                    let (object, reference) = repo.revparse_ext(&commit)?;
                    repo.checkout_tree(&object, None)?;
                    if let Some(gref) = reference {
                        repo.set_head(std::str::from_utf8(gref.name_bytes())?)?
                    } else {
                        repo.set_head_detached(object.id())?
                    }
                }
            }
            Git(Either::Right(GitInfo {
                url,
                branch: Some(GitLocation::Branch(branch)),
                ..
            })) => {
                let repo = if path.exists() {
                    git2::Repository::open(&path)?
                } else {
                    git2::Repository::clone_recurse(&url, &path)?
                };
                let remotes = repo.remotes()?;
                let mut it = remotes.iter();
                if let Some(remote_name) = it.next() {
                    if it.next().is_some() {
                        anyhow::bail!(
                            "multiple remotes are available for repository at {}",
                            path.display()
                        );
                    }
                    if let Some(remote_name) = remote_name {
                        let mut remote = repo.find_remote(remote_name)?;
                        let name_bytes = remote.default_branch()?;
                        let name = std::str::from_utf8(&name_bytes)?;
                        remote.fetch(&[&branch], None, None)?;
                        let fetch_head = repo.find_reference("FETCH_HEAD")?;
                        let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
                        let analysis = repo.merge_analysis(&[&fetch_commit])?;
                        if analysis.0.is_up_to_date() {
                            return anyhow::Ok(());
                        } else if analysis.0.is_fast_forward() {
                            let refname = format!("refs/heads/{name}");
                            let mut reference = repo.find_reference(&refname)?;
                            reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                            repo.set_head(&refname)?;
                            repo.checkout_head(Some(
                                git2::build::CheckoutBuilder::default().force(),
                            ))?;
                        } else {
                            anyhow::bail!(
                                "remote {remote_name} in repo {} is not fast-forward",
                                path.display()
                            )
                        }
                    } else {
                        anyhow::bail!("remote name is not UTF-8")
                    }
                } else {
                    anyhow::bail!("no remotes are avaiable in {}", path.display())
                }
            }
            Tar(url) => tar::Archive::new(flate2::read::GzDecoder::new(
                ureq::get(&url).call()?.into_reader(),
            ))
            .unpack(path)?,
            Zip(url) => {
                let mut buf = vec![];
                ureq::get(&url)
                    .call()?
                    .into_reader()
                    .read_to_end(&mut buf)?;
                zip_extract::extract(std::io::Cursor::new(buf), &path, true)?;
            }
            Cmd(cmd) => {
                std::process::Command::new("sh")
                    .arg("-c")
                    .arg(cmd)
                    .status()?;
            }
            Manual => {}
        }
    }
    Ok(())
}
/// An installation specification
/// It has a terse `FromStr` impl for command line use
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstallSpec {
    pub name: String,
    pub version: VersionReq,
    pub targets: Option<Vec<String>>,
}
impl InstallSpec {
    pub fn new(name: String, version: VersionReq, targets: Option<Vec<String>>) -> Self {
        Self {
            name,
            version,
            targets,
        }
    }
    pub fn from_pkgdep(name: String, spec: build::PkgDepSpec) -> Self {
        Self {
            name,
            version: spec.version,
            targets: spec.targets,
        }
    }
}
impl std::str::FromStr for InstallSpec {
    type Err = semver::Error;
    fn from_str(req: &str) -> Result<Self, Self::Err> {
        let mut targets: Option<Vec<String>> = None;
        let mut version = VersionReq::STAR;
        let mut it = req.match_indices(['@', ':']).peekable();
        let name = if let Some(&(idx, _)) = it.peek() {
            req[..idx].to_string()
        } else {
            req.to_string()
        };
        while let Some((idx, ch)) = it.next() {
            let blk = if let Some(&(next, _)) = it.peek() {
                req[idx..next].trim()
            } else {
                req[idx..].trim()
            };
            match ch {
                "@" => version
                    .comparators
                    .append(&mut VersionReq::parse(blk)?.comparators),
                ":" => targets
                    .get_or_insert_with(Vec::new)
                    .extend(blk.split(',').map(str::trim).map(String::from)),
                x => unreachable!("should be '@' or ':', got {x:?}"),
            }
        }
        Ok(Self {
            name,
            version,
            targets,
        })
    }
}
#[derive(Debug, Clone, Error)]
pub enum InstallError {
    /// Package does not exist
    #[error("couldn't find package {0:?}")]
    CantFindPkg(&'static str),
    /// Something needs to be downloaded
    #[error("package {0:?} is not installed and would need to be downloaded")]
    Frozen(String),
    /// This package doesn't have the target we need
    #[error("package {0:?} (version {1}) doesn't have the {2:?} target")]
    NoMatchingTarget(&'static str, Version, &'static str),
    /// This package doesn't have the right version
    #[error("package {0:?} doesn't have any versions matching the requirement: {1:?}")]
    NoMatchingVersion(&'static str, VersionReq),
    /// This package doesn't have a default target
    #[error("package {0:?} does not have a `default` target, and none were specified")]
    NoDefaultTarget(&'static str),
    /// There was a dependency cycle
    /// This prints out the cycle, in the format: package.target@version -> next.target@version,
    /// ending with the first package
    /// This only prints out one cycle, but there could be multiple.
    #[error("dependencies couldn't be resolved due to a cycle: {}", DisplayCycle(.0))]
    DependencyCycle(VecDeque<(graph::Id, graph::Id, Version)>),
}
struct DisplayCycle<'a>(pub &'a VecDeque<(graph::Id, graph::Id, Version)>);
impl std::fmt::Display for DisplayCycle<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (pkg, tar, v) in self.0 {
            write!(
                f,
                "{}.{}@{v} -> ",
                graph::STRINGS.resolve(pkg),
                graph::STRINGS.resolve(tar)
            )?
        }
        let (pkg, tar, v) = self.0.front().unwrap();
        write!(
            f,
            "{}.{}@{v}",
            graph::STRINGS.resolve(pkg),
            graph::STRINGS.resolve(tar)
        )
    }
}
/// Installation options
#[derive(Debug, Clone)]
pub struct InstallOptions {
    pub force_build: bool,
    pub frozen: bool,
    /// This is the target triple
    pub target: String,
}
impl Default for InstallOptions {
    fn default() -> Self {
        Self {
            force_build: false,
            frozen: false,
            target: inkwell::targets::TargetMachine::get_default_triple()
                .as_str()
                .to_str()
                .unwrap()
                .to_string(),
        }
    }
}
/// Convenience method to find the path that a package is installed to
pub fn installed_path(
    package: &str,
    target: &str,
    version: &Version,
) -> Result<PathBuf, NoCobaltDir> {
    let mut path = cobalt_dir()?;
    path.push("installed");
    path.push(package);
    path.push(version.to_string());
    path.push(target);
    Ok(path)
}
/// Install a singular package. It is assumed that all of its dependencies are already met
fn install_single(
    pkg: &'static str,
    tar: &'static str,
    version: &Version,
    opts: &InstallOptions,
    package: &Package,
    plan: &IndexMap<(&str, &str), Version>,
) -> anyhow::Result<()> {
    let path = installed_path(pkg, tar, version)?;
    if !opts.force_build && path.exists() {
        return Ok(());
    } // already exists
    if !opts.force_build {
        // Search for prebuilds that match the target
        // If opts.frozen == true, only allow files starting with "/" or "file://", which are
        // recognized as local files
        if let Some(mut url) = package.releases[version]
            .prebuilds
            .iter()
            .flat_map(|(os, ts)| ts.iter().map(|(tar, url)| (os.as_str(), tar, url)))
            .find_map(|(os, t, url)| {
                ((!opts.frozen || url.starts_with('/') || url.starts_with("file://"))
                    && t == tar
                    && glob::Pattern::new(os).ok()?.matches(&opts.target))
                .then_some(url.as_str())
            })
        {
            if if url.starts_with("file://") {
                url = &url[7..];
                true
            } else {
                url.starts_with('/')
            } {
                Path::new(url).copy_anyhow(path)?;
            } else {
                assert!(!opts.frozen); // this should always be true, but one small assertion won't hurt performance
                let mut file = std::fs::File::create(path)?;
                std::io::copy(&mut ureq::get(url).call()?.into_reader(), &mut file)?;
            }
            return Ok(());
        }
    }
    let mut src_path = cobalt_dir()?;
    src_path.push("packages");
    src_path.push(pkg);
    src_path.push(version.to_string());
    src_path.push("src");
    if let Source::Git(Either::Right(GitInfo { dir: Some(p), .. })) =
        &package.releases[version].source
    {
        src_path.push(p)
    }
    if !src_path.exists() {
        if opts.frozen {
            anyhow::bail!(InstallError::Frozen(pkg.to_string()))
        }
        package.releases[version].source.install(&src_path)?;
    }
    let mut build_path = cobalt_dir()?;
    build_path.push("packages");
    build_path.push(pkg);
    build_path.push(version.to_string());
    build_path.push("build");
    let proj =
        toml::from_str::<build::Project>(&src_path.join("cobalt.toml").read_to_string_anyhow()?)?;
    let target =
        proj.targets
            .get(tar)
            .ok_or(InstallError::NoMatchingTarget(pkg, version.clone(), tar))?;
    let out = build::build_target_single(
        target,
        pkg,
        tar,
        version,
        plan,
        &build::BuildOptions {
            source_dir: src_path.as_path().into(),
            build_dir: build_path.as_path().into(),
            continue_comp: false,
            continue_build: false,
            rebuild: true,
            profile: "default".into(),
            triple: opts.target.as_str().into(),
            link_dirs: vec![],
            no_default_link: false,
        },
    )?;
    out.copy_anyhow(path)?;
    Ok(())
}
/// Installation entry point
/// All packages can just be given, and everything is handled
pub fn install<I: IntoIterator<Item = InstallSpec>>(
    pkgs: I,
    opts: &InstallOptions,
) -> anyhow::Result<IndexMap<(&'static str, &'static str), Version>> {
    let reg = REGISTRY
        .iter()
        .map(|pkg| (pkg.name.as_str(), pkg))
        .collect::<HashMap<&'static str, _>>();
    let mut graph = graph::DependencyGraph::new();
    graph.is_frozen = opts.frozen;
    graph.build_tree(pkgs)?;
    let order = graph
        .build_order()
        .map_err(InstallError::DependencyCycle)?
        .into_iter()
        .map(|(p, t, v)| ((graph::STRINGS.resolve(&p), graph::STRINGS.resolve(&t)), v))
        .collect::<IndexMap<_, _>>();
    order
        .iter()
        .try_for_each(|((p, t), v)| install_single(p, t, v, &Default::default(), reg[p], &order))?;
    Ok(order)
}
