#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate semver;

use regex::Regex;
use semver::Version;
use std::env;
use std::ffi::OsStr;
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};
use std::process::Command;

/// A single path to search for LLVM in (containing bin/llvm-config)
const ENV_LLVM_PREFIX: &str = "COBALT_LLVM_PREFIX";

/// If exactly "YES", ignore the version blocklist
const ENV_IGNORE_BLOCKLIST: &str = "COBALT_LLVM_IGNORE_BLOCKLIST";

/// If set, enforce precise correspondence between crate and binary versions.
const ENV_STRICT_VERSIONING: &str = "COBALT_LLVM_STRICT_VERSIONING";

/// If set, do not attempt to strip irrelevant options for llvm-config --cflags
const ENV_NO_CLEAN_CFLAGS: &str = "COBALT_LLVM_NO_CLEAN_CFLAGS";

/// If set and targeting MSVC, force the debug runtime library
const ENV_USE_DEBUG_MSVCRT: &str = "COBALT_LLVM_USE_DEBUG_MSVCRT";

/// If set, always link against libffi
const ENV_FORCE_FFI: &str = "COBALT_LLVM_FFI_WORKAROUND";

lazy_static! {
    /// Filesystem path to an llvm-config binary for the correct version.
    static ref LLVM_CONFIG_PATH: Option<PathBuf> = locate_llvm_config();

    /// LLVM version to use. Takes *lower* priority than llvm-config
    static ref LLVM_VERSION: Option<String> = env::var("COBALT_LLVM_VERSION").ok();

    /// LLVM major version
    static ref LLVM_MAJOR: Option<u64> = LLVM_VERSION.as_ref().map(|v| Version::parse(v).expect("LLVM version should be valid semver!").major);

    /// LLVM prefix
    static ref LLVM_PREFIX: Option<PathBuf> = env::var_os(ENV_LLVM_PREFIX).map(PathBuf::from);
}

fn target_env_is(name: &str) -> bool {
    match env::var_os("CARGO_CFG_TARGET_ENV") {
        Some(s) => s == name,
        None => false,
    }
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
    let prefix = LLVM_PREFIX
        .as_ref()
        .map(|p| p.join("bin"))
        .unwrap_or_default();
    for binary_name in llvm_config_binary_names() {
        let binary_name = prefix.join(binary_name);
        match llvm_version(&binary_name) {
            Ok(ref version) => if is_compatible_llvm(version) {
                // Compatible version found. Nice.
                return Some(binary_name);
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
fn llvm_config_binary_names() -> &'static [&'static str] {
    macro_rules! name_list {
        (@single $suf:expr, $major:expr) => {
            [concat!("llvm-config-", stringify!($major), $suf),
            concat!("llvm-config", stringify!($major), $suf),
            concat!("llvm", stringify!($major), "config", $suf),
            concat!("llvm-config-", stringify!($major), ".0", $suf),
            concat!("llvm-config", stringify!($major), "0", $suf),
            ]
        };
        ($suf:expr, $($args:expr),*) => {
            {
                #[allow(clippy::no_effect)]
                const VAL: [&str; $({$args; 5}+)*0] = array_concat::concat_arrays!($(name_list!(@single $suf, $args)),*);
                VAL
            }
        };
    }

    static BASE_NAMES: &[&str; 22] = &array_concat::concat_arrays!(
        name_list!("", 16, 15),
        ["llvm-config"],
        name_list!(".exe", 16, 15),
        ["llvm-config.exe"]
    );
    if target_os_is("windows") {
        BASE_NAMES
    } else {
        &BASE_NAMES[..(BASE_NAMES.len() / 2)]
    }
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
        let bad_version = Version::new(major, minor, patch);

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
    if let Some(v) = env::var(ENV_STRICT_VERSIONING).ok().and_then(|v| Version::parse(&v).ok()) {
        llvm_version == &v || {
            println!("Found LLVM {llvm_version}, but need exactly {v}");
            false
        }
    }
    else if strict {
        (llvm_version.major == 16 && llvm_version.minor == 0) || {
            println!("Found LLVM {llvm_version}, but need 16.0.x");
            false
        }
    } else {
        llvm_version.major >= 15 || {
            println!("Found LLVM {llvm_version}, but need at least LLVM 15");
            false
        }
    }
}

/// Get the output from running `llvm-config` with the given argument.
///
/// Lazily searches for or compiles LLVM as configured by the environment
/// variables.
fn llvm_config(arg: &str) -> String {
    try_llvm_config(std::iter::once(arg)).expect("Surprising failure from llvm-config")
}

/// Get the output from running `llvm-config` with the given argument.
///
/// Does not panic on failure.
fn try_llvm_config<'a>(arg: impl IntoIterator<Item = &'a str>) -> io::Result<String> {
    llvm_config_ex(&*LLVM_CONFIG_PATH.clone().unwrap(), arg.into_iter())
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

/// Get the names of the dylibs required by LLVM, including the C++ standard
/// library.
fn get_system_libraries(kind: LibraryKind) -> Vec<String> {
    let link_arg = match kind {
        LibraryKind::Static => "--link-static",
        LibraryKind::Dynamic => "--link-shared",
    };
    if let Ok(var) = env::var("LLVM_SYSTEM_LIBS") {
        var
    } else if LLVM_CONFIG_PATH.is_some() {
        try_llvm_config(["--system-libs", link_arg].iter().copied())
            .expect("Surprising failure from llvm-config")
    } else if kind == LibraryKind::Static {
        "-lrt -ldl -lm -lz -lzstd -ltinfo -lxml2".to_string()
    } else {
        String::new()
    }
    .split(&[' ', '\n', ','] as &[char])
    .filter(|s| !s.is_empty())
    .map(|flag| {
        if target_env_is("msvc") {
            // Same as --libnames, foo.lib
            assert!(
                flag.ends_with(".lib"),
                "system library {:?} does not appear to be a MSVC library file",
                flag
            );
            &flag[..flag.len() - 4]
        } else {
            if let Some(flag) = flag.strip_prefix("-l") {
                // Linker flags style, -lfoo
                if target_os_is("macos") && flag.starts_with("-llib") && flag.ends_with(".tbd") {
                    // .tdb libraries are "text-based stub" files that provide lists of symbols,
                    // which refer to libraries shipped with a given system and aren't shipped
                    // as part of the corresponding SDK. They're named like the underlying
                    // library object, including the 'lib' prefix that we need to strip.
                    return flag[3..flag.len() - 4].to_owned();
                }

                if let Some(i) = flag.find(".so.") {
                    // On some distributions (OpenBSD, perhaps others), we get sonames
                    // like "-lz.so.7.0". Correct those by pruning the file extension
                    // and library version.
                    return flag[..i].to_owned();
                }
                return flag.to_owned();
            }

            let maybe_lib = Path::new(&flag);
            if maybe_lib.is_file() {
                // Library on disk, likely an absolute path to a .so. We'll add its location to
                // the library search path and specify the file as a link target.
                println!(
                    "cargo:rustc-link-search={}",
                    maybe_lib.parent().unwrap().display()
                );

                // Expect a file named something like libfoo.so, or with a version libfoo.so.1.
                // Trim everything after and including the last .so and remove the leading 'lib'
                let soname = maybe_lib
                    .file_name()
                    .unwrap()
                    .to_str()
                    .expect("Shared library path must be a valid string");
                let stem = soname
                    .rsplit_once(target_dylib_extension())
                    .expect("Shared library should be a .so file")
                    .0;

                stem.trim_start_matches("lib")
            } else {
                panic!(
                    "Unable to parse result of llvm-config --system-libs: was {:?}",
                    flag
                )
            }
        }
        .to_owned()
    })
    .chain(get_system_libcpp().map(str::to_owned))
    .collect::<Vec<String>>()
}

/// Return additional linker search paths that should be used but that are not discovered
/// by other means.
///
/// In particular, this should include only directories that are known from platform-specific
/// knowledge that aren't otherwise discovered from either `llvm-config` or a linked library
/// that includes an absolute path.
fn get_system_library_dirs() -> impl IntoIterator<Item = &'static str> {
    if target_os_is("openbsd") {
        Some("/usr/local/lib")
    } else {
        None
    }
}

fn target_dylib_extension() -> &'static str {
    if target_os_is("macos") {
        ".dylib"
    } else {
        ".so"
    }
}

/// Get the library that must be linked for C++, if any.
fn get_system_libcpp() -> Option<&'static str> {
    if target_env_is("msvc") {
        // MSVC doesn't need an explicit one.
        None
    } else if target_os_is("macos")
        || target_os_is("freebsd")
        || target_os_is("openbsd")
        || target_env_is("musl")
    {
        // The one built with musl.
        Some("c++")
    } else {
        // Otherwise assume GCC's libstdc++.
        // This assumption is probably wrong on some platforms, but would need
        // testing on them.
        Some("stdc++")
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LibraryKind {
    Static,
    Dynamic,
}

impl LibraryKind {
    pub fn from_is_static(is_static: bool) -> Self {
        if is_static {
            LibraryKind::Static
        } else {
            LibraryKind::Dynamic
        }
    }

    pub fn string(&self) -> &'static str {
        match self {
            LibraryKind::Static => "static",
            LibraryKind::Dynamic => "dylib",
        }
    }
}

/// Get the names of libraries to link against, along with whether it is static or shared library.
fn get_link_libraries(preferences: &LinkingPreferences) -> (LibraryKind, Vec<String>) {
    // Using --libnames in conjunction with --libdir is particularly important
    // for MSVC when LLVM is in a path with spaces, but it is generally less of
    // a hack than parsing linker flags output from --libs and --ldflags.

    fn get_link_libraries_impl(is_static: bool) -> std::io::Result<String> {
        // Windows targets don't get dynamic support.
        // See: https://gitlab.com/taricorp/llvm-sys.rs/-/merge_requests/31#note_1306397918
        if target_env_is("msvc") && !is_static {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Dynamic linking to LLVM is not supported on Windows.",
            ));
        }

        let link_arg = if is_static {
            "--link-static"
        } else {
            "--link-shared"
        };
        if let Ok(names) = env::var("LLVM_LIBNAMES") {
            Ok(names)
        } else if LLVM_CONFIG_PATH.is_some() {
            try_llvm_config(["--libnames", link_arg].iter().copied())
        } else if is_static {
            Ok(include_str!("build-stuff/static-libs.txt").to_string())
        } else if let Some(major) = *LLVM_MAJOR {
            Ok(format!("LLVM-{major}"))
        } else {
            println!("cargo:warning=LLVM libraries were not set, defaulting to libLLVM");
            Ok("LLVM".to_string())
        }
    }

    fn lib_kind(is_static: bool) -> &'static str {
        if is_static {
            "static"
        } else {
            "shared"
        }
    }

    let mut errs = vec![];
    let is_static = preferences.prefer_static;

    match get_link_libraries_impl(is_static) {
        Ok(s) => {
            return (
                LibraryKind::from_is_static(is_static),
                extract_library(&s, is_static),
            )
        }
        Err(e) => errs.push((lib_kind(is_static), e)),
    }

    if !preferences.force {
        println!(
            "cargo:warning=failed to get {} libraries from llvm-config, falling back to {}",
            lib_kind(is_static),
            lib_kind(!is_static),
        );

        match get_link_libraries_impl(!is_static) {
            Ok(s) => {
                return (
                    LibraryKind::from_is_static(!is_static),
                    extract_library(&s, !is_static),
                )
            }
            Err(e) => errs.push((lib_kind(!is_static), e)),
        }
    }

    panic!("failed to get link libraries from llvm-config: {:?}", errs);
}

fn extract_library(s: &str, is_static: bool) -> Vec<String> {
    s.split(&[' ', '\n'] as &[char])
        .filter(|s| !s.is_empty())
        .map(|name| {
            // --libnames gives library filenames. Extract only the name that
            // we need to pass to the linker.
            if is_static {
                // Match static library
                if name.ends_with(".a") {
                    // Unix (Linux/Mac)
                    // libLLVMfoo.a
                    &name[3..name.len() - 2]
                } else if let Some(name) = name.strip_suffix(".lib") {
                    // Windows
                    // LLVMfoo.lib
                    name
                } else {
                    panic!("{:?} does not look like a static library name", name)
                }
            } else {
                // Match shared library
                if name.ends_with(".dylib") {
                    // Mac
                    // libLLVMfoo.dylib
                    &name[3..name.len() - 6]
                } else if name.ends_with(".so") {
                    // Linux
                    // libLLVMfoo.so
                    &name[3..name.len() - 3]
                } else if name.ends_with(".dll") || name.ends_with(".lib") {
                    // Windows
                    // LLVMfoo.{dll,lib}
                    &name[..name.len() - 4]
                } else {
                    panic!("{:?} does not look like a shared library name", name)
                }
            }
            .to_string()
        })
        .filter(|s| !s.contains("Polly")) // polly isn't needed, and it's not included in APT packages
        .collect::<Vec<String>>()
}

#[derive(Debug, Clone, Copy)]
struct LinkingPreferences {
    /// Prefer static linking over dynamic linking.
    prefer_static: bool,
    /// Force the use of the preferred kind of linking.
    force: bool,
}

impl LinkingPreferences {
    fn init() -> LinkingPreferences {
        let prefer_static = cfg!(feature = "prefer-static");
        let prefer_dynamic = cfg!(feature = "prefer-dynamic");
        let force_static = cfg!(feature = "force-static");
        let force_dynamic = cfg!(feature = "force-dynamic");

        // more than one preference is an error
        if [prefer_static, prefer_dynamic, force_static, force_dynamic]
            .iter()
            .filter(|&&x| x)
            .count()
            > 1
        {
            panic!(
                "Only one of the features `prefer-static`, `prefer-dynamic`, `force-static`, \
                 `force-dynamic` can be enabled at once."
            );
        }

        LinkingPreferences {
            prefer_static: force_static || prefer_static,
            force: force_static || force_dynamic,
        }
    }
}

fn get_llvm_cflags() -> String {
    let output = if let Ok(flags) = env::var("LLVM_CFLAGS") {
        flags
    } else if LLVM_CONFIG_PATH.is_some() {
        llvm_config("--cflags")
    } else if let Some(major) = *LLVM_MAJOR {
        format!("-I/usr/lib/llvm-{major}/include -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS")
    } else if let Some(prefix) = LLVM_PREFIX.as_ref() {
        format!("-I{prefix:?}/include -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS")
    } else {
        "-I/usr/lib/llvm-17/include -I/usr/lib/llvm-16/include -I/usr/lib/llvm-15/include -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS".to_string()
    }; // TODO: add for non-linux platforms?

    // llvm-config includes cflags from its own compilation with --cflags that
    // may not be relevant to us. In particularly annoying cases, these might
    // include flags that aren't understood by the default compiler we're
    // using. Unless requested otherwise, clean CFLAGS of options that are
    // known to be possibly-harmful.
    let no_clean = env::var_os(ENV_NO_CLEAN_CFLAGS).is_some();
    if no_clean || target_env_is("msvc") {
        // MSVC doesn't accept -W... options, so don't try to strip them and
        // possibly strip something that should be retained. Also do nothing if
        // the user requests it.
        return output;
    }

    llvm_config("--cflags")
        .split(&[' ', '\n', ','][..])
        .filter(|word| !word.starts_with("-W"))
        .collect::<Vec<_>>()
        .join(" ")
}

fn is_llvm_debug() -> bool {
    // Has to be either Debug or Release
    LLVM_CONFIG_PATH.is_none() || llvm_config("--build-mode").contains("Debug")
}

fn main() {
    // Behavior can be significantly affected by these vars.
    println!("cargo:rerun-if-env-changed={}", ENV_LLVM_PREFIX);
    if let Ok(path) = env::var(ENV_LLVM_PREFIX) {
        println!("cargo:rerun-if-changed={}", path);
    }

    println!("cargo:rerun-if-env-changed={}", ENV_IGNORE_BLOCKLIST);
    println!("cargo:rerun-if-env-changed={}", ENV_STRICT_VERSIONING);
    println!("cargo:rerun-if-env-changed={}", ENV_NO_CLEAN_CFLAGS);
    println!("cargo:rerun-if-env-changed={}", ENV_USE_DEBUG_MSVCRT);
    println!("cargo:rerun-if-env-changed={}", ENV_FORCE_FFI);

    println!(
        "cargo:rustc-env=LLVM_VERSION={}",
        if LLVM_CONFIG_PATH.is_some() {
            llvm_config("--version")
        } else if let Some(version) = &*LLVM_VERSION {
            version.clone()
        } else {
            "<unknown>".to_string()
        }
    );

    if cfg!(feature = "no-llvm-linking") && cfg!(feature = "disable-alltargets-init") {
        // exit early as we don't need to do anything and llvm-config isn't needed at all
        return;
    }

    if LLVM_CONFIG_PATH.is_none() {
        println!("cargo:warning=couldn't find llvm-config, proceeding with best gueses");
    }

    #[cfg(not(feature = "no-llvm-linking"))]
    {
        let dirs = if let Ok(dirs) = env::var("LLVM_LIBDIR") {
            dirs
        } else if LLVM_CONFIG_PATH.is_some() {
            llvm_config("--libdir")
        } else if let Some(prefix) = LLVM_PREFIX.as_ref() {
            // Export information to other crates
            println!(
                "cargo:config_path={}",
                LLVM_CONFIG_PATH.clone().unwrap().display()
            ); // will be DEP_LLVM_CONFIG_PATH
            prefix.join("lib").to_string_lossy().into_owned()
        } else {
            println!("cargo:warning=LLVM_LIBDIR is not set!");
            String::new()
        };
        for libdir in dirs.split(['\n']).filter(|s| !s.is_empty()) {
            println!("cargo:libdir={}", libdir); // DEP_LLVM_LIBDIR

            // Link LLVM libraries
            println!("cargo:rustc-link-search=native={}", libdir);
        }
        let preferences = LinkingPreferences::init();

        for link_search_dir in get_system_library_dirs() {
            println!("cargo:rustc-link-search=native={}", link_search_dir);
        }
        // We need to take note of what kind of libraries we linked to, so that
        // we can link to the same kind of system libraries
        let (kind, libs) = get_link_libraries(&preferences);
        for name in libs {
            println!("cargo:rustc-link-lib={}={}", kind.string(), name);
        }

        // Link system libraries
        // We get the system libraries based on the kind of LLVM libraries we link to, but we link to
        // system libs based on the target environment.
        let sys_lib_kind = if target_env_is("musl") {
            LibraryKind::Static
        } else {
            LibraryKind::Dynamic
        };
        for name in get_system_libraries(kind) {
            println!("cargo:rustc-link-lib={}={}", sys_lib_kind.string(), name);
        }

        let use_debug_msvcrt = env::var_os(ENV_USE_DEBUG_MSVCRT).is_some();
        if target_env_is("msvc") && (use_debug_msvcrt || is_llvm_debug()) {
            println!("cargo:rustc-link-lib=msvcrtd");
        }

        // Link libffi if the user requested this workaround.
        // See https://bitbucket.org/tari/llvm-sys.rs/issues/12/
        let force_ffi = env::var_os(ENV_FORCE_FFI).is_some();
        if force_ffi {
            println!("cargo:rustc-link-lib=dylib=ffi");
        }
    }

    #[cfg(not(feature = "disable-alltargets-init"))]
    {
        std::env::set_var("CFLAGS", get_llvm_cflags());
        cc::Build::new()
            .file("build-stuff/target.c")
            .compile("targetwrappers");
    }
}
