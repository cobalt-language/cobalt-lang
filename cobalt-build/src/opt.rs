use anyhow_std::*;
use std::env::var_os;
use std::path::PathBuf;
use thiserror::Error;

/// A profile wasn't found in any of the searched paths
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Error)]
#[error("couldn't find profile {0}")]
pub struct MissingProfile(pub String);

/// Something went wrong while running passes
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Error)]
#[error("encountered an error while running passes:{}{0}", if .0.contains('\n') {'\n'} else {' '})]
pub struct PassError(pub String);
impl PassError {
    /// Convenience method for use with `map_err`
    #[inline(always)]
    pub fn from_llvm(s: cobalt_llvm::inkwell::support::LLVMString) -> Self {
        Self(s.to_string())
    }
}

lazy_static::lazy_static! {
    static ref SEARCH_DIRS: Vec<PathBuf> = [
        var_os("COBALT_DIR").map(|p| {
            let mut p = PathBuf::from(p);
            p.push("profiles");
            p
        }),
        var_os("HOME").map(|p| {
            let mut p = PathBuf::from(p);
            p.push(".cobalt");
            p.push("profiles");
            p
        }),
        var_os("HOME").map(|p| {
            let mut p = PathBuf::from(p);
            p.push(".config");
            p.push("cobalt");
            p.push("profiles");
            p
        }),
        Some("/usr/local/share/cobalt/profiles".into()),
        Some("/usr/share/cobalt/profiles".into()),
    ].into_iter().filter_map(|p| p.and_then(|p| p.is_dir().then_some(p))).collect();
}

fn expand_profile_name(name: &str) -> anyhow::Result<String> {
    for p in SEARCH_DIRS.iter() {
        let p = p.join(name);
        if p.is_file() {
            return p.read_to_string_anyhow();
        }
    }
    Ok(match name {
        "none" | "0" => "default<O0>",
        "less" | "1" => "default<O1>",
        "some" | "2" | "default" => "default<O2>",
        "aggr" | "3" => "default<O3>",
        _ => Err(MissingProfile(name.to_string()))?,
    }
    .to_string())
}

fn non_ident(ch: char) -> bool {
    !matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '$')
}

/// Expand a pass string, replacing `@name `with the contents of a pass.
pub fn expand_pass_string(mut passes: &str) -> anyhow::Result<String> {
    let mut out = String::with_capacity(passes.len());
    while let Some(idx) = passes.find('@') {
        out += &passes[..idx];
        passes = &passes[(idx + 1)..];
        let idx = passes.find(non_ident).unwrap_or(passes.len());
        out += &expand_profile_name(&passes[..idx])?;
        passes = &passes[idx..];
    }
    out += passes;
    Ok(out)
}
