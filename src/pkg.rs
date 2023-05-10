use serde::{Serialize, Deserialize};
use std::path::PathBuf;
use std::collections::HashMap;
use either::Either;
use anyhow_std::*;
use crate::cobalt_dir;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitInfo {
    #[serde(alias = "path")]
    url: String, // URL/path of repository
    #[serde(flatten)]
    branch: Option<GitLocation>, // branch/commit/tag to use
    dir: Option<PathBuf> // subdirectory
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitLocation {
    Branch(String),
    #[serde(alias = "tag")]
    Commit(String)
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegistryType {
    #[serde(with = "either::serde_untagged")]
    Git(Either<String, GitInfo>),
    Tar(String), // URL/path of .tar.gz archive
    Zip(String), // URL/path of .zip archive
    Cmd(String), // Command to run to update
    Manual       // Something we don't have to worry about handles it
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registry {
    #[serde(flatten, rename = "type")]
    pub reg_type: RegistryType,
    pub path: PathBuf
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryList {
    #[serde(default)]
    pub registry: Vec<Registry>
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub author: String,
    pub source: Source,
    #[serde(alias = "description")]
    pub desc: Option<String>,
    pub releases: HashMap<String, Release>
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Release {
    pub source: Source,
    pub prebuilds: HashMap<String, HashMap<String, Source>>
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Source {
    #[serde(with = "either::serde_untagged")]
    Git(Either<String, GitInfo>),
    Tar(String),
    Zip(String)
}
pub fn get_packages() -> anyhow::Result<Vec<Package>> {
    let reg_path = cobalt_dir().join("registries.toml");
    if !reg_path.exists() {return Ok(vec![])}
    let registries = toml::from_str::<RegistryList>(&reg_path.read_to_string_anyhow()?)?;
    let mut out = vec![];
    for reg in registries.registry {
        let mut path = reg.path;
        if let RegistryType::Git(Either::Right(GitInfo {dir: Some(ref dir), ..})) = reg.reg_type {path.push(dir)}
        let it = path.read_dir_anyhow()?;
        let bounds = it.size_hint();
        if let Some(upper) = bounds.1 {out.reserve(upper);}
        else {out.reserve(bounds.0);}
        for entry in it {
            out.push(toml::from_str::<Package>(&entry?.path().read_to_string_anyhow()?)?);
        }
    }
    Ok(out)
}
pub fn update_packages() -> anyhow::Result<()> {
    let reg_path = cobalt_dir().join("registries.toml");
    if !reg_path.exists() {return Ok(())}
    let registries = toml::from_str::<RegistryList>(&reg_path.read_to_string_anyhow()?)?;
    for reg in registries.registry {
        let path = reg.path;
        use RegistryType::*;
        match reg.reg_type {
            Git(Either::Left(url) | Either::Right(GitInfo {url, branch: None, ..})) => {
                if path.exists() {
                    let repo = git2::Repository::open(&path)?;
                    let remotes = repo.remotes()?;
                    let mut it = remotes.iter();
                    if let Some(remote_name) = it.next() {
                        if it.next().is_some() {
                            anyhow::bail!("multiple remotes are available for repository at {}", path.display());
                        }
                        if let Some(remote_name) = remote_name {
                            let mut remote = repo.find_remote(remote_name)?;
                            let name_bytes = remote.default_branch()?;
                            let name = std::str::from_utf8(&name_bytes)?;
                            remote.fetch(&[name], None, None)?;
                            let fetch_head = repo.find_reference("FETCH_HEAD")?;
                            let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
                            let analysis = repo.merge_analysis(&[&fetch_commit])?;
                            if analysis.0.is_up_to_date() {return anyhow::Ok(())}
                            else if analysis.0.is_fast_forward() {
                                let refname = format!("refs/heads/{name}");
                                let mut reference = repo.find_reference(&refname)?;
                                reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                                repo.set_head(&refname)?;
                                repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))?;
                            }
                            else {anyhow::bail!("remote {remote_name} in repo {} is not fast-forward", path.display())}
                        }
                        else {anyhow::bail!("remote name is not UTF-8")}
                    }
                    else {anyhow::bail!("no remotes are avaiable in {}", path.display())}
                }
                else {git2::Repository::clone_recurse(&url, &path)?;}
            },
            Git(Either::Right(GitInfo {url, branch: Some(GitLocation::Commit(commit)), ..})) => {
                if !path.exists() {
                    let repo = git2::Repository::clone_recurse(&url, &path)?;
                    let (object, reference) = repo.revparse_ext(&commit)?;
                    repo.checkout_tree(&object, None)?;
                    if let Some(gref) = reference {repo.set_head(std::str::from_utf8(gref.name_bytes())?)?}
                    else {repo.set_head_detached(object.id())?}
                }
            },
            Git(Either::Right(GitInfo {url, branch: Some(GitLocation::Branch(branch)), ..})) => {
                let repo = if path.exists() {git2::Repository::open(&path)?} else {git2::Repository::clone_recurse(&url, &path)?};
                let remotes = repo.remotes()?;
                let mut it = remotes.iter();
                if let Some(remote_name) = it.next() {
                    if it.next().is_some() {
                        anyhow::bail!("multiple remotes are available for repository at {}", path.display());
                    }
                    if let Some(remote_name) = remote_name {
                        let mut remote = repo.find_remote(remote_name)?;
                        let name_bytes = remote.default_branch()?;
                        let name = std::str::from_utf8(&name_bytes)?;
                        remote.fetch(&[&branch], None, None)?;
                        let fetch_head = repo.find_reference("FETCH_HEAD")?;
                        let fetch_commit = repo.reference_to_annotated_commit(&fetch_head)?;
                        let analysis = repo.merge_analysis(&[&fetch_commit])?;
                        if analysis.0.is_up_to_date() {return anyhow::Ok(())}
                        else if analysis.0.is_fast_forward() {
                            let refname = format!("refs/heads/{name}");
                            let mut reference = repo.find_reference(&refname)?;
                            reference.set_target(fetch_commit.id(), "Fast-Forward")?;
                            repo.set_head(&refname)?;
                            repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))?;
                        }
                        else {anyhow::bail!("remote {remote_name} in repo {} is not fast-forward", path.display())}
                    }
                    else {anyhow::bail!("remote name is not UTF-8")}
                }
                else {anyhow::bail!("no remotes are avaiable in {}", path.display())}
            },
            Tar(url) => tar::Archive::new(flate2::read::GzDecoder::new(ureq::get(&url).call()?.into_reader())).unpack(path)?,
            Zip(url) => {
                let mut buf = vec![];
                ureq::get(&url).call()?.into_reader().read_to_end(&mut buf)?;
                zip_extract::extract(std::io::Cursor::new(buf), &path, true)?;
            },
            Cmd(cmd) => {std::process::Command::new("sh").arg("-c").arg(cmd).status()?;},
            Manual => {}
        }
    }
    Ok(())
}