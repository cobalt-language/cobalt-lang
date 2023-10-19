#![allow(clippy::type_complexity)]
pub mod build;
pub mod cc;
#[cfg(feature = "gluon-build")]
mod gluon_build;
pub mod graph;
pub mod libs;
pub mod obj;
pub mod opt;
pub mod pkg;

pub use build::clear_mod;
use cobalt_ast::*;
use cobalt_errors::error;
use cobalt_llvm::*;
use path_calculate::*;
use std::io::{self, prelude::*, ErrorKind};
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Debug, Clone, Copy, Error)]
#[error("build failed because of {0} compiler error{}", if *.0 == 1 {""} else {"s"})]
pub struct CompileErrors(pub usize);

#[derive(Debug, Error)]
#[error("LLVM verification failed: {0}")]
pub struct LlvmVerifierError(Box<str>);
impl From<String> for LlvmVerifierError {
    fn from(value: String) -> Self {
        Self(value.into_boxed_str())
    }
}
impl From<inkwell::support::LLVMString> for LlvmVerifierError {
    fn from(value: inkwell::support::LLVMString) -> Self {
        Self(value.to_string().into_boxed_str())
    }
}
#[derive(Debug, Clone, Copy, Error)]
#[error("cobalt directory could not be found")]
pub struct NoCobaltDir;
impl From<NoCobaltDir> for io::Error {
    fn from(_: NoCobaltDir) -> Self {
        io::Error::new(ErrorKind::Other, "cobalt directory could not be found")
    }
}
#[derive(Debug, Clone, Error)]
#[error("couldn't find libraries: {}", .0.join(", "))]
pub struct LibsNotFound(pub Vec<String>);

pub fn cobalt_dir() -> Result<PathBuf, NoCobaltDir> {
    if let Ok(path) = std::env::var("COBALT_DIR") {
        Ok(path.into())
    } else if let Ok(path) = std::env::var("HOME") {
        Ok(Path::new(&path).join(".cobalt"))
    } else {
        Err(NoCobaltDir)
    }
}
pub fn load_projects() -> io::Result<Vec<[String; 2]>> {
    let mut cobalt_dir = cobalt_dir()
        .map_err(|_| io::Error::new(ErrorKind::Other, "cobalt directory could not be found"))?;
    if !cobalt_dir.exists() {
        std::fs::create_dir_all(&cobalt_dir)?;
    }
    cobalt_dir.push("tracked.txt");
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(cobalt_dir)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    let mut it = buf.split('\0');
    let len = it.size_hint();
    let mut vec = Vec::<[String; 2]>::with_capacity(len.1.unwrap_or(len.0));
    while let (Some(name), Some(path)) = (it.next(), it.next()) {
        if Path::new(path).exists() {
            vec.push([name.to_string(), path.to_string()])
        }
    }
    Ok(vec)
}
pub fn track_project(name: &str, path: PathBuf, vec: &mut Vec<[String; 2]>) {
    if let Some(entry) = vec.iter_mut().find(|[_, p]| {
        if let Ok(path) = path.as_absolute_path() {
            path == Path::new(&p)
        } else {
            false
        }
    }) {
        entry[0] = name.to_string()
    } else if let Some(entry) = vec.iter_mut().find(|[n, _]| n == name) {
        entry[1] = path
            .as_absolute_path()
            .unwrap()
            .to_string_lossy()
            .to_string()
    } else {
        vec.sort_by(|[n1, _], [n2, _]| n1.cmp(n2));
        vec.dedup_by(|[n1, _], [n2, _]| n1 == n2);
        vec.push([
            name.to_string(),
            path.as_absolute_path()
                .unwrap()
                .to_string_lossy()
                .to_string(),
        ])
    }
}
pub fn save_projects(vec: Vec<[String; 2]>) -> io::Result<()> {
    let mut cobalt_dir = cobalt_dir()
        .map_err(|_| io::Error::new(ErrorKind::Other, "cobalt directory could not be found"))?;
    if !cobalt_dir.exists() {
        std::fs::create_dir_all(&cobalt_dir)?;
    }
    cobalt_dir.push("tracked.txt");
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(cobalt_dir)?;
    vec.into_iter().flatten().try_for_each(|s| {
        file.write_all(s.as_bytes())?;
        file.write_all(&[0])
    })?;
    let pos = file.stream_position()?;
    file.set_len(pos)
}
