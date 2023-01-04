use serde::*;
use semver::{Version, VersionReq};
use flate2::write::GzDecoder;
use super::build::{Project, BuildOptions, build};
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
        if !install_loc.exists() {
            std::fs::create_dir_all(install_loc).map_err(InstallError::StdIoError)?;
        }
        install_loc.push(self.name);
        let install_dir = install_loc.clone();
        if let Some(url) = rel.prebuilt.get(triple) {
            if opts.output.verbose() {eprintln!("\tdownloading prebuilt version from {url}");}
            let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
            let decoded = GzDecoder::new(body);
            tar::Archive::new(decoded).unpack(install_loc);
        }
        else {
            if opts.verbose {eprintln!("\tno prebuilt version available, building from source");}
            install_loc.push(".download");
            let triple = inkwell::targets::TargetTriple::new(triple);
            match rel.src {
                Git(url) => {
                    if opts.output.verbose() {eprintln!("\tcloning from git repository from {url}");}
                    git2::Repository::clone(url, install_loc).map_err(InstallError::GitCloneError);
                },
                Tarball(url) => {
                    if opts.output.verbose() {eprintln!("\tdownloading source tarball from {url}");}
                    let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
                    let decoded = GzDecoder::new(body);
                    tar::Archive::new(decoded).unpack(install_loc);
                },
                Zipball(url) => {
                    if opts.output.verbose() {eprintln!("\tdownloading source zipball from {url}");}
                    let body = reqwest::blocking::get(url).map_err(InstallError::DownloadError)?;
                    let reader = std::io::BufReader::new(body);
                    zip_extract::extract(reader, &install_loc, true).map_err(InstallError::ZipExtractError);
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
            if res == 0 {Ok(())} else {Err(InstallError::BuildFailed(res))}
        }
    }
}
#[derive(Debug, Clone, Copy, Default)]
pub enum OutputLevel {Normal, Quiet Verbose}
impl OutputLevel {
    pub fn verbose(self) -> bool {self == Verbose}
    pub fn quiet(self) -> bool {self == Quiet}
}
#[derive(Debug, Clone, Copy, Default)]
pub struct InstallOptions {
    pub output: OutputLevel,
    pub clean: bool
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
