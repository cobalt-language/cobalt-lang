use serde::{Serialize, Deserialize};
use std::path::PathBuf;
use std::collections::HashMap;
use either::Either;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitInfo {
    git: String, // URL/path of repository
    #[serde(alias = "commit", alias = "tag")]
    branch: Option<String>, // branch/commit/tag to use
    dir: Option<PathBuf> // subdirectory
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