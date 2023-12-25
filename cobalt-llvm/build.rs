// This file is the same as llvm-sys's, with some slight modifications to make it run as a submodule.
// Notable changes:
// - LLVM version is chosen from features
// - No linking is actually done here
// - Cargo variables are set
// - Small refactors have been made to satisfy Clippy

use const_format::formatcp;
use lazy_static::lazy_static;
use regex::Regex;
use semver::Version;
use std::env;
use std::ffi::OsStr;
use std::io::{self, ErrorKind};
use std::path::PathBuf;
use std::process::Command;

#[cfg(feature = "llvm-15")]
const LLVM_MAJOR: u64 = 15;
#[cfg(feature = "llvm-16")]
const LLVM_MAJOR: u64 = 16;

// Environment variables that can guide compilation
//
// When adding new ones, they should also be added to main() to force a
// rebuild if they are changed.
static ENV_LLVM_PREFIX: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_PREFIX");
static ENV_IGNORE_BLOCKLIST: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_IGNORE_BLOCKLIST");
static ENV_STRICT_VERSIONING: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_STRICT_VERSIONING");
static ENV_NO_CLEAN_CFLAGS: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_NO_CLEAN_CFLAGS");
static ENV_USE_DEBUG_MSVCRT: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_USE_DEBUG_MSVCRT");
static ENV_FORCE_FFI: &str = formatcp!("LLVM_SYS_{LLVM_MAJOR}_FFI_WORKAROUND");

lazy_static! {
    /// LLVM version used by this version of the crate.
    static ref CRATE_VERSION: Version = {
        Version::new(
            LLVM_MAJOR,
            0,
            0
        )
    };

    /// Filesystem path to an llvm-config binary for the correct version.
    static ref LLVM_CONFIG_PATH: Option<PathBuf> = locate_llvm_config();
}

fn target_os_is(name: &str) -> bool {
    match env::var_os("CARGO_CFG_TARGET_OS") {
        Some(s) => s == name,
        None => false,
    }
}

/// Try to find a version of llvm-config that is compatible with this crate.
///
/// If $LLVM_SYS_<VERSION>_PREFIX is set, look for llvm-config ONLY in there. The assumption is
/// that the user know best, and they want to link to a specific build or fork of LLVM.
///
/// If $LLVM_SYS_<VERSION>_PREFIX is NOT set, then look for llvm-config in $PATH.
///
/// Returns None on failure.
fn locate_llvm_config() -> Option<PathBuf> {
    let prefix = env::var_os(ENV_LLVM_PREFIX)
        .map(|p| PathBuf::from(p).join("bin"))
        .unwrap_or_default();
    for binary_name in llvm_config_binary_names() {
        let binary_name = prefix.join(binary_name);
        match llvm_version(&binary_name) {
            Ok(ref version) if is_compatible_llvm(version) => {
                // Compatible version found. Nice.
                return Some(binary_name);
            }
            Ok(version) => {
                // Version mismatch. Will try further searches, but warn that
                // we're not using the system one.
                println!(
                    "Found LLVM version {} on PATH, but need {}.",
                    version, *CRATE_VERSION
                );
            }
            Err(ref e) if e.kind() == ErrorKind::NotFound => {
                // Looks like we failed to execute any llvm-config. Keep
                // searching.
            }
            // Some other error, probably a weird failure. Give up.
            Err(e) => panic!("Failed to search PATH for llvm-config: {}", e),
        }
    }

    None
}

/// Return an iterator over possible names for the llvm-config binary.
fn llvm_config_binary_names() -> std::vec::IntoIter<String> {
    let mut base_names: Vec<String> = vec![
        "llvm-config".into(),
        formatcp!("llvm-config-{LLVM_MAJOR}").into(),
        formatcp!("llvm{LLVM_MAJOR}-config").into(),
        formatcp!("llvm-config-{LLVM_MAJOR}.0").into(),
        formatcp!("llvm-config{LLVM_MAJOR}0").into(),
    ];

    // On Windows, also search for llvm-config.exe
    if target_os_is("windows") {
        let mut exe_names = base_names.clone();
        for name in exe_names.iter_mut() {
            name.push_str(".exe");
        }
        base_names.extend(exe_names);
    }

    base_names.into_iter()
}

/// Check whether the given version of LLVM is blocklisted,
/// returning `Some(reason)` if it is.
fn is_blocklisted_llvm(llvm_version: &Version) -> Option<&'static str> {
    static BLOCKLIST: &[(u64, u64, u64, &str)] = &[];

    if let Some(x) = env::var_os(ENV_IGNORE_BLOCKLIST) {
        if &x == "YES" {
            println!(
                "cargo:warning=Ignoring blocklist entry for LLVM {}",
                llvm_version
            );
            return None;
        } else {
            println!(
                "cargo:warning={} is set but not exactly \"YES\"; blocklist is still honored.",
                ENV_IGNORE_BLOCKLIST
            );
        }
    }

    for &(major, minor, patch, reason) in BLOCKLIST.iter() {
        let bad_version = Version {
            major,
            minor,
            patch,
            pre: semver::Prerelease::EMPTY,
            build: semver::BuildMetadata::EMPTY,
        };

        if &bad_version == llvm_version {
            return Some(reason);
        }
    }
    None
}

/// Check whether the given LLVM version is compatible with this version of
/// the crate.
fn is_compatible_llvm(llvm_version: &Version) -> bool {
    if let Some(reason) = is_blocklisted_llvm(llvm_version) {
        println!(
            "Found LLVM {}, which is blocklisted: {}",
            llvm_version, reason
        );
        return false;
    }

    let strict =
        env::var_os(ENV_STRICT_VERSIONING).is_some() || cfg!(feature = "strict-versioning");
    if strict {
        llvm_version.major == CRATE_VERSION.major && llvm_version.minor == CRATE_VERSION.minor
    } else {
        llvm_version.major == CRATE_VERSION.major && llvm_version.minor >= CRATE_VERSION.minor
    }
}

/// Get the output from running `llvm-config` with the given argument.
///
/// Lazily searches for or compiles LLVM as configured by the environment
/// variables.
fn llvm_config(arg: &str) -> String {
    try_llvm_config(Some(arg).into_iter()).expect("Surprising failure from llvm-config")
}

/// Get the output from running `llvm-config` with the given argument.
///
/// Does not panic on failure.
fn try_llvm_config<'a>(arg: impl Iterator<Item = &'a str>) -> io::Result<String> {
    llvm_config_ex(LLVM_CONFIG_PATH.clone().unwrap(), arg)
}

/// Invoke the specified binary as llvm-config.
///
/// Explicit version of the `llvm_config` function that bubbles errors
/// up.
fn llvm_config_ex<'a, S: AsRef<OsStr>>(
    binary: S,
    args: impl Iterator<Item = &'a str>,
) -> io::Result<String> {
    Command::new(binary).args(args).output().and_then(|output| {
        if output.status.code() != Some(0) {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "llvm-config failed with error code {:?}",
                    output.status.code()
                ),
            ))
        } else if output.stdout.is_empty() {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "llvm-config returned empty output",
            ))
        } else {
            Ok(String::from_utf8(output.stdout)
                .expect("Output from llvm-config was not valid UTF-8"))
        }
    })
}

/// Get the LLVM version using llvm-config.
fn llvm_version<S: AsRef<OsStr>>(binary: &S) -> io::Result<Version> {
    let version_str = llvm_config_ex(binary.as_ref(), ["--version"].iter().copied())?;

    // LLVM isn't really semver and uses version suffixes to build
    // version strings like '3.8.0svn', so limit what we try to parse
    // to only the numeric bits.
    let re = Regex::new(r"^(?P<major>\d+)\.(?P<minor>\d+)(?:\.(?P<patch>\d+))??").unwrap();
    let c = match re.captures(&version_str) {
        Some(c) => c,
        None => {
            panic!(
                "Could not determine LLVM version from llvm-config. Version string: {}",
                version_str
            );
        }
    };

    // some systems don't have a patch number but Version wants it so we just append .0 if it isn't
    // there
    let s = match c.name("patch") {
        None => format!("{}.0", &c[0]),
        Some(_) => c[0].to_string(),
    };
    Ok(Version::parse(&s).unwrap())
}

fn main() {
    // Behavior can be significantly affected by these vars.
    println!("cargo:rerun-if-env-changed={}", ENV_LLVM_PREFIX);
    if let Ok(path) = env::var(ENV_LLVM_PREFIX) {
        println!("cargo:rerun-if-changed={}", path);
    }

    println!("cargo:rerun-if-env-changed={}", ENV_IGNORE_BLOCKLIST);
    println!("cargo:rerun-if-env-changed={}", ENV_NO_CLEAN_CFLAGS);
    println!("cargo:rerun-if-env-changed={}", ENV_USE_DEBUG_MSVCRT);
    println!("cargo:rerun-if-env-changed={}", ENV_FORCE_FFI);

    println!("cargo:rustc-env=LLVM_VERSION={}", llvm_config("--version"));

    if cfg!(feature = "no-llvm-linking") {
        // exit early as we don't need to do anything and llvm-config isn't needed at all
        return;
    }

    if LLVM_CONFIG_PATH.is_none() {
        println!("cargo:rustc-cfg=LLVM_SYS_NOT_FOUND");
    }
}
