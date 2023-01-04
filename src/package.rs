use serde::*;
use semver::{Version, VersionReq};
use flate2::write::GzDecoder;
use super::build::{Project, BuildOptions, build};
use git2::Repository;
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Release {
    #[serde(flatten)]
    src: Source,
    prebuilt: HashMap<String, Source>
}
impl Package {
    pub fn install(&self, triple: String, version: Option<VersionReq>, opts: InstallOptions) -> Result<(), InstallError> {
        let (v, rel) = self.releases.iter().filter(|(k, _)| if let Some(v) = version {k.matches(v)} else {true}).max_by_key(|x| x.0);
        if !opts.output.quiet() {eprintln!("installing {} version {v}", self.name);}
        let mut install_loc = 
            if let Some(dir) = std::env::get("COBALT_PKG_DIR") {PathBuf::from(dir)} 
            else if let Some(dir) = std::env::get("COBALT_DIR") {PathBuf::from(format!("{dir}/packages"))}
            else if let Some(dir) = std::env::get("HOME") {PathBuf::from(format!("{dir}/.cobalt/packages"))}
            else {return Err(InstallError::NoInstallDirectory)};
        install_loc.push(self.name);
        if !install_loc.exists() {
            std::fs::create_dir_all(install_loc).map_err(InstallError::StdIoError)?;
        }
        let install_dir = install_loc.clone();
        if let Some(url) = rel.prebuilt.get(triple) {
            if opts.output.verbose() {eprintln!("\tdownloading prebuilt version from {url}");}
            let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
            let decoded = GzDecoder::new(body);
            tar::Archive::new(decoded).unpack(install_loc);
        }
        else {
            if opts.verbose {eprint!("\tno prebuilt version available, building from source... ");}
            install_loc.push(".download");
            let triple = inkwell::targets::TargetTriple::new(triple);
            match rel.src {
                Git(url) => {
                    if opts.output.verbose() {eprintln!("cloning from git repository from {url}");}
                    Repository::clone(url, install_loc).map_err(InstallError::GitCloneError);
                },
                Tarball(url) => {
                    if opts.output.verbose() {eprintln!("downloading source tarball from {url}");}
                    let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
                    let decoded = GzDecoder::new(body);
                    tar::Archive::new(decoded).unpack(install_loc);
                },
                Zipball(url) => {
                    if opts.output.verbose() {eprintln!("downloading source zipball from {url}");}
                    let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
                    let reader = std::io::BufReader::new(body);
                    zip_extract::extract(reader, &install_loc, true).map_err(InstallError::ZipExtractError)?;
                }
            }
            if !opts.output.quiet() {eprintln!("building {}", self.name)}
            install_loc.push("cobalt.toml");
            let cfg = std::fs::read_to_string(install_loc).map_err(InstallError::StdIoError)?;
            install_loc.pop();
            let res = build(toml::from_str(cfg), BuildOptions {
                source_dir: &install_loc,
                build_dir: &install_dir,
                continue_comp: false,
                continue_build: false,
                triple: &triple
            });
            if opts.clean {std::fs::remove_all(install_loc)}
            if res == 0 {Ok(())} else {Err(InstallError::BuildFailed(res))}
        }
    }
}
#[derive(Debug, Clone, Copy, Default)]
pub enum OutputLevel {Normal, Quiet, Verbose}
impl OutputLevel {
    pub fn verbose(self) -> bool {self == Verbose}
    pub fn quiet(self) -> bool {self == Quiet}
}
#[derive(Debug, Clone, Copy)]
pub struct InstallOptions {
    pub output: OutputLevel,
    pub clean: bool
}
impl Default for InstallOptions {
    fn default() -> Self {InstallOptions {output: OutputLevel::Normal, clean: true}}
}
#[derive(Debug, Clone)]
pub enum InstallError {
    NoInstallDirectory,
    ZipExtractError(zip_extract::ZipError),
    DownloadError(reqwest::Error),
    GitCloneError(git2::Error),
    StdIoError(std::io::Error),
    BuildFailed(i32)
}
impl From<zip_extract::ZipError> for InstallError {
    pub fn from(err: zip_extract::ZipError) -> Self {InstallError::ZipExtractError(err)}
}
impl From<reqwest::Error> for InstallError {
    pub fn from(err: reqwest::Error) -> Self {InstallError::DownloadError(err)}
}
impl From<git2::Error> for InstallError {
    pub fn from(err: git2::Error) -> Self {InstallError::GitError(err)}
}
impl From<std::io::::Error> for InstallError {
    pub fn from(err: std::io::Error) -> Self {InstallError::StdIoError(err)}
}
#[derive(Debug, Clone)]
pub enum PackageUpdateError {
    NoInstallDirectory,
    GitError(git2::Error),
    StdIoError(std::io::Error)
}
impl From<git2::Error> for PackageUpdateError {
    pub fn from(err: git2::Error) -> Self {PackageUpdateError::GitError(err)}
}
impl From<std::io::Error> for PackageUpdateError {
    pub fn from(err: std::io::Error) -> Self {PackageUpdateError::StdIoError(err)}
}
pub fn packages() -> Result<HashMap<String, Package>, PackageUpdateError> {
    let cobalt_path = 
        if let Some(path) = std::env::get("COBALT_DIR") {format!("{path}/registry")}
        else if let Some(path) = std::env::get("HOME") {format!("{path}/.cobalt/registry")}
        else {return Err(PackageUpdateError::NoInstallDirectory)};
    match Repository::open(&cobalt_path)) {
        Ok(repo) => { // pull
            repo.find_remote("origin")?.fetch("main", None, None)?;
            let fetch_head = repo.find_reference("FETCH_HEAD")?;
            let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
            let analysis = repo.merge_analysis(&[&fetch_commit])?;
            if analysis.0.is_up_to_date() {}
            else if analysis.0.is_fast_forward() {
                let mut reference = repo.find_reference("refs/heads/main")?;
                reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                repo.set_head("refs/heads/main")?;
                repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))
            }
            else {
                panic!("Pull from Cobalt package registry is not fast-forward!");
            }
        },
        Err(_) => Repository::clone("https://github.com/matt-cornell/cobalt-registry.git", &cobalt_path) // eventually, this might be under a cobalt-lang org, or somewhere else
    }?
    Ok(std::fs::read_dir(cobalt_path)?.filter_map(|entry| Some((entry.file_name(), toml::from_str::<Package>(std::fs::read_to_string(entry.ok()?.file_name()).ok()?).ok()?))).collect())
}
