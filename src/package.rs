use serde::*;
use semver::{Version, VersionReq};
use flate2::read::GzDecoder;
use super::build::{BuildOptions, build};
use git2::Repository;
use std::io::{Read, Cursor};
use std::path::PathBuf;
use std::collections::HashMap;
use try_lazy_init::Lazy;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    name: String,
    author: String,
    #[serde(alias = "description")]
    desc: String,
    #[serde(alias = "release")]
    #[serde(default)]
    releases: HashMap<Version, Release>
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Source {
    #[serde(rename = "git")]
    Git(String),
    #[serde(rename = "tar")]
    #[serde(alias = "tarball")]
    Tarball(String),
    #[serde(rename = "zip")]
    #[serde(alias = "zipball")]
    Zipball(String)
}
impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Source::*;
        match self {
            Git(x) | Tarball(x) | Zipball(x) => write!(f, "{x}")
        }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Release {
    #[serde(flatten)]
    pub src: Source,
    #[serde(default)]
    #[serde(alias = "dependencies")]
    pub deps: HashMap<String, String>,
    pub prebuilt: HashMap<String, Source>
}
lazy_static::lazy_static! {
    static ref REGISTRY: Lazy<HashMap<String, Package>> = Lazy::new();
}
impl Package {
    fn init_pkgs() -> Result<HashMap<String, Package>, PackageUpdateError> {
        let cobalt_path = 
            if let Ok(path) = std::env::var("COBALT_DIR") {format!("{path}/registry")}
            else if let Ok(path) = std::env::var("HOME") {format!("{path}/.cobalt/registry")}
            else {return Err(PackageUpdateError::NoInstallDirectory)};
        match Repository::open(&cobalt_path) {
            Ok(repo) => { // pull
                repo.find_remote("origin")?.fetch(&["main"], None, None)?;
                let fetch_head = repo.find_reference("FETCH_HEAD")?;
                let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
                let analysis = repo.merge_analysis(&[&fetch_commit])?;
                if analysis.0.is_up_to_date() {}
                else if analysis.0.is_fast_forward() {
                    let mut reference = repo.find_reference("refs/heads/main")?;
                    reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                    repo.set_head("refs/heads/main")?;
                    repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))?;
                }
                else {
                    panic!("Pull from Cobalt package registry is not fast-forward!");
                }
            },
            Err(_) => {Repository::clone("https://github.com/matt-cornell/cobalt-registry.git", &cobalt_path)?;} // eventually, this might be under a cobalt-lang org, or somewhere else
        }
        Ok(std::fs::read_dir(cobalt_path)?.filter_map(|entry| Some(toml::from_str::<Package>(&std::fs::read_to_string(entry.as_ref().ok()?.file_name()).ok()?).ok()?)).map(|pkg| (pkg.name.clone(), pkg)).collect())
    }
    pub fn init_registry() -> Result<(), PackageUpdateError> {REGISTRY.try_get_or_create(Self::init_pkgs).map(std::mem::drop)}
    pub fn registry() -> &'static HashMap<String, Package> {REGISTRY.get().expect("Package::init_registry() must be successfully called before Package::registry() can be used")}
    pub fn install<'a>(&'a self, triple: &str, version: Option<VersionReq>, opts: InstallOptions) -> Result<(), InstallError<'a>> {
        let (v, rel) = if let Some(val) = self.releases.iter().filter(|(k, _)| if let Some(v) = &version {v.matches(k)} else {true}).max_by_key(|x| x.0) {val} else {return Err(InstallError::NoMatchesError)};
        if !opts.output.quiet() {eprintln!("installing {} version {v}", self.name);}
        for (dep, ver) in rel.deps.iter() {
            if ver == "system" {} // Linking is specified in the library file
            else if let Ok(ver) = ver.parse::<VersionReq>() {
                if let Some(pkg) = Self::registry().get(dep) {
                    pkg.install(triple, Some(ver), opts)?;
                }
                else {
                    return Err(InstallError::PkgNotFound(dep));
                }
            }
            else {
                return Err(InstallError::InvalidVersionSpec(dep, ver))
            }
        }
        let mut install_loc = 
            if let Ok(dir) = std::env::var("COBALT_PKG_DIR") {PathBuf::from(dir)} 
            else if let Ok(dir) = std::env::var("COBALT_DIR") {PathBuf::from(format!("{dir}/packages"))}
            else if let Ok(dir) = std::env::var("HOME") {PathBuf::from(format!("{dir}/.cobalt/packages"))}
            else {return Err(InstallError::NoInstallDirectory)};
        install_loc.push(format!("{}-{v}", self.name));
        if !install_loc.exists() {std::fs::create_dir_all(&install_loc)?;}
        else if !opts.reinstall {return Ok(())}
        let install_dir = install_loc.clone();
        if let (Some(url), false) = (rel.prebuilt.get(triple), opts.build_source) {
            if opts.output.verbose() {eprintln!("\tdownloading prebuilt version from {url}");}
            match url {
                Source::Git(url) => {
                    if opts.output.verbose() {eprintln!("cloning from git repository from {url}");}
                    Repository::clone(&url, &install_dir)?;
                },
                Source::Tarball(url) => {
                    if opts.output.verbose() {eprintln!("downloading prebuilt tarball from {url}");}
                    let mut body = reqwest::blocking::get(url)?;
                    let mut vec = Vec::new();
                    body.read_to_end(&mut vec)?;
                    let decoded = GzDecoder::new(Cursor::new(vec));
                    tar::Archive::new(decoded).unpack(&install_dir)?;
                },
                Source::Zipball(url) => {
                    if opts.output.verbose() {eprintln!("downloading prebuilt zipball from {url}");}
                    let mut body = reqwest::blocking::get(url)?;
                    let mut vec = Vec::new();
                    body.read_to_end(&mut vec)?;
                    zip_extract::extract(Cursor::new(vec), &install_dir, true)?;
                }
            }
            Ok(())
        }
        else {
            if opts.output.verbose() {
                if opts.build_source {
                    eprint!("\tforced to build from source... ");
                }
                else {
                    eprint!("\tno prebuilt version available, building from source... ");
                }
            }
            install_loc.push(".download");
            let triple = inkwell::targets::TargetTriple::create(&triple);
            match &rel.src {
                Source::Git(url) => {
                    if opts.output.verbose() {eprintln!("cloning from git repository from {url}");}
                    Repository::clone(&url, &install_loc)?;
                },
                Source::Tarball(url) => {
                    if opts.output.verbose() {eprintln!("downloading source tarball from {url}");}
                    let mut body = reqwest::blocking::get(url)?;
                    let mut vec = Vec::new();
                    body.read_to_end(&mut vec)?;
                    let decoded = GzDecoder::new(Cursor::new(vec));
                    tar::Archive::new(decoded).unpack(&install_loc)?;
                },
                Source::Zipball(url) => {
                    if opts.output.verbose() {eprintln!("downloading source zipball from {url}");}
                    let mut body = reqwest::blocking::get(url)?;
                    let mut vec = Vec::new();
                    body.read_to_end(&mut vec)?;
                    zip_extract::extract(Cursor::new(vec), &install_loc, true)?;
                }
            }
            if !opts.output.quiet() {eprintln!("building {}", self.name)}
            install_loc.push("cobalt.toml");
            let cfg = std::fs::read_to_string(&install_loc)?;
            install_loc.pop();
            let res = build(toml::from_str(&cfg)?, None, &BuildOptions {
                source_dir: &install_loc,
                build_dir: &install_dir,
                continue_comp: false,
                continue_build: false,
                triple: &triple,
                profile: "default",
                link_dirs:  if let Ok(home) = std::env::var("HOME") {vec![format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]}
                            else {["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from).collect()}
            });
            if opts.clean {std::fs::remove_dir_all(install_loc)?}
            if res == 0 {Ok(())} else {Err(InstallError::BuildFailed(res))}
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputLevel {Quiet, Normal, Verbose}
impl OutputLevel {
    pub fn verbose(self) -> bool {self == OutputLevel::Verbose}
    pub fn quiet(self) -> bool {self == OutputLevel::Quiet}
}
#[derive(Debug, Clone, Copy)]
pub struct InstallOptions {
    pub output: OutputLevel,
    pub clean: bool,
    pub reinstall: bool,
    pub build_source: bool
}
impl Default for InstallOptions {
    fn default() -> Self {
        InstallOptions {
            output: OutputLevel::Normal, 
            clean: true,
            reinstall: false,
            build_source: false
        }
    }
}
#[derive(Debug)]
pub enum InstallError<'a> {
    NoInstallDirectory,
    NoMatchesError,
    ZipExtractError(zip_extract::ZipExtractError),
    DownloadError(reqwest::Error),
    GitCloneError(git2::Error),
    StdIoError(std::io::Error),
    CfgFileError(toml::de::Error),
    PkgNotFound(&'a String),
    InvalidVersionSpec(&'a String, &'a String),
    BuildFailed(i32)
}
impl From<zip_extract::ZipExtractError> for InstallError<'_> {
    fn from(err: zip_extract::ZipExtractError) -> Self {InstallError::ZipExtractError(err)}
}
impl From<reqwest::Error> for InstallError<'_> {
    fn from(err: reqwest::Error) -> Self {InstallError::DownloadError(err)}
}
impl From<git2::Error> for InstallError<'_> {
    fn from(err: git2::Error) -> Self {InstallError::GitCloneError(err)}
}
impl From<std::io::Error> for InstallError<'_> {
    fn from(err: std::io::Error) -> Self {InstallError::StdIoError(err)}
}
impl From<toml::de::Error> for InstallError<'_> {
    fn from(err: toml::de::Error) -> Self {InstallError::CfgFileError(err)}
}
#[derive(Debug)]
pub enum PackageUpdateError {
    NoInstallDirectory,
    GitError(git2::Error),
    StdIoError(std::io::Error)
}
impl From<git2::Error> for PackageUpdateError {
    fn from(err: git2::Error) -> Self {PackageUpdateError::GitError(err)}
}
impl From<std::io::Error> for PackageUpdateError {
    fn from(err: std::io::Error) -> Self {PackageUpdateError::StdIoError(err)}
}
