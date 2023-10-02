use crate::*;
use os_str_bytes::{OsStrBytes, OsStringBytes};
use std::ffi::OsString;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompileCommand {
    pub libs: Vec<OsString>,
    pub abss: Vec<PathBuf>,
    pub dirs: Vec<PathBuf>,
    pub objects: Vec<PathBuf>,
    pub output_file: PathBuf,
    pub target_: Option<String>,
    pub is_lib: bool,
    pub no_default_link: bool,
}
impl CompileCommand {
    pub fn new() -> Self {
        Self {
            libs: vec![],
            abss: vec![],
            dirs: vec![],
            objects: vec![],
            output_file: PathBuf::default(),
            target_: None,
            is_lib: false,
            no_default_link: false,
        }
    }
    pub fn obj<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.objects.push(path.into());
        self
    }
    pub fn objs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(&mut self, paths: I) -> &mut Self {
        self.objects
            .extend(paths.into_iter().map(Into::<PathBuf>::into));
        self
    }
    pub fn link_dir<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.dirs.push(path.into());
        self
    }
    pub fn link_dirs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.dirs.extend(paths.into_iter().map(Into::into));
        self
    }
    pub fn link_lib<P: Into<OsString>>(&mut self, path: P) -> &mut Self {
        self.libs.push(path.into());
        self
    }
    pub fn link_libs<P: Into<OsString>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.libs.extend(paths.into_iter().map(Into::into));
        self
    }
    pub fn link_abs<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.abss.push(path.into());
        self
    }
    pub fn link_abss<P: Into<PathBuf>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.abss.extend(paths.into_iter().map(Into::into));
        self
    }
    pub fn output<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.output_file = path.into();
        self
    }
    pub fn lib(&mut self, is_lib: bool) -> &mut Self {
        self.is_lib = is_lib;
        self
    }
    pub fn target<S: Into<String>>(&mut self, target: S) -> &mut Self {
        self.target_ = Some(target.into());
        self
    }
    #[cfg(windows)]
    pub fn lib_dirs(&mut self) -> anyhow::Result<Vec<PathBuf>> {
        unimplemented!("I don't know how to do this on Windows. Get a better OS maybe?")
    }
    #[cfg(not(windows))]
    pub fn lib_dirs(&mut self) -> anyhow::Result<Vec<PathBuf>> {
        let native = inkwell::targets::TargetMachine::get_default_triple();
        let native = native.as_str().to_str()?;
        let triple = self.target_.as_deref().unwrap_or(native);
        let compatible = self.target_.is_none()
            || native == triple
            || (triple.split_once('-').map_or(false, |(t, _)| {
                let n = native.split_once('-').unwrap().0;
                ((is_x86(n) || n == "x86_64") && (t == "x86_64" || is_x86(t)))
                    || ((is_arm32(n) || is_arm64(n)) && (is_arm32(t) || is_arm64(t)))
            }) && native
                .split('-')
                .zip(triple.split('-'))
                .skip(1)
                .all(|(n, t)| n == "unknown" || t == "unknown" || n == t));
        let mut out = self.dirs.clone();
        fn from_env(name: &str, out: &mut Vec<PathBuf>) {
            if let Some(dirs) = std::env::var_os(name) {
                out.extend(dirs.into_raw_vec().split(|&b| b == b':').filter_map(|b| {
                    let p = Path::assert_from_raw_bytes(b);
                    p.exists().then(|| p.to_path_buf())
                }));
            }
        }
        fn parse_bool(val: &str) -> Option<bool> {
            match val {
                "YES" | "ON" | "TRUE" | "ENABLE" | "ENABLED" | "Y" => Some(true),
                "NO" | "OFF" | "FALSE" | "DISABLE" | "DISABLED" | "N" => Some(false),
                _ => None,
            }
        }
        fn env_flag(var: &str) -> Option<bool> {
            std::env::var(var).ok().and_then(|v| parse_bool(&v))
        }
        if triple.starts_with("wasm") {
            // WASM doesn't have system link dirs
            let wasm32 = triple.contains("wasm32");
            let wasm64 = !wasm32 && triple.contains("wasm64");
            if wasm32 {
                from_env("COBALT_WASM32_LINK_DIRS", &mut out);
            }
            if wasm64 {
                from_env("COBALT_WASM64_LINK_DIRS", &mut out);
            }
            from_env("COBALT_WASM_LINK_DIRS", &mut out);
            return Ok(out);
        }
        fn append_paths<I: IntoIterator<Item = P>, P: Into<PathBuf>>(
            out: &mut Vec<PathBuf>,
            paths: I,
        ) {
            out.extend(paths.into_iter().map(Into::into).filter(|p| p.exists()))
        }
        if native == triple {
            from_env("COBALT_NATIVE_LINK_DIRS", &mut out);
        }
        let default = {
            let mut it = triple.split('-');
            let arch = it.next().unwrap_or("UNKNOWN").to_ascii_uppercase();
            it.next();
            let os = it.next().unwrap_or("UNKNOWN").to_ascii_uppercase();
            from_env(&format!("COBALT_{arch}_{os}_LINK_DIRS"), &mut out);
            from_env(&format!("COBALT_{arch}_LINK_DIRS"), &mut out);
            from_env(&format!("COBALT_{os}_LINK_DIRS"), &mut out);
            !(self.no_default_link
                || (native == triple)
                    .then(|| env_flag("COBALT_NATIVE_NO_DEFAULT_LINK"))
                    .flatten()
                    .or_else(|| env_flag(&format!("COBALT_{arch}_{os}_NO_DEFAULT_LINK")))
                    .or_else(|| env_flag(&format!("COBALT_{arch}_NO_DEFAULT_LINK")))
                    .or_else(|| env_flag(&format!("COBALT_{os}_NO_DEFAULT_LINK")))
                    .or_else(|| env_flag("COBALT_NO_DEFAULT_LINK"))
                    .unwrap_or(false))
        };
        from_env("COBALT_LINK_DIRS", &mut out);
        if default {
            #[cfg(target_os = "linux")]
            'specific: {
                let mut it = triple.split('-');
                let Some(arch) = it.next() else {break 'specific};
                let Some(_)    = it.next() else {break 'specific};
                let Some(os)   = it.next() else {break 'specific};
                let trip = it.next().map_or_else(
                    || format!("{arch}-{os}"),
                    |info| format!("{arch}-{os}-{info}"),
                );
                append_paths(
                    &mut out,
                    [format!("/usr/lib/{trip}"), format!("/lib/{trip}")],
                );
            }
            if compatible {
                append_paths(
                    &mut out,
                    ["/usr/local/lib", "/usr/lib", "/lib", "/opt/homebrew/opt"],
                );
            }
        }
        if let Ok(cobalt) = std::env::var("COBALT_DIR") {
            append_paths(&mut out, [format!("{cobalt}/installed/lib/{triple}")]);
        }
        if let Ok(home) = std::env::var("HOME") {
            append_paths(
                &mut out,
                [
                    format!("{home}/.cobalt/installed/{triple}"),
                    format!("{home}/.local/lib/cobalt/installed/{triple}"),
                ],
            )
        }
        Ok(out)
    }
    pub fn search_libs<L: AsRef<str>, I: IntoIterator<Item = L>>(
        &mut self,
        libs: I,
        ctx: Option<&CompCtx>,
        load: bool,
    ) -> anyhow::Result<Vec<L>> {
        let mut remaining = libs.into_iter().collect::<Vec<_>>();
        if remaining.is_empty() {
            return Ok(vec![]);
        }
        let lib_dirs = self.lib_dirs()?;
        let triple = inkwell::targets::TargetMachine::get_default_triple();
        let triple = self
            .target_
            .as_deref()
            .unwrap_or_else(|| triple.as_str().to_str().unwrap());
        let windows = triple.contains("windows");
        let apple = triple.contains("apple");
        let dyn_os_ext = if windows {
            ".dll"
        } else if apple {
            ".dylib"
        } else {
            ".so"
        };
        // check for lib_dir/lib, which is most common
        for dir in &lib_dirs {
            remaining = remaining
                .into_iter()
                .filter_map(|lib| {
                    // there should really be a try_retain, but this had to be done instead
                    let path = dir.join(format!(
                        "{}{}{}",
                        if windows { "" } else { "lib" },
                        lib.as_ref(),
                        dyn_os_ext
                    ));
                    if path.exists() {
                        if let Some(ctx) = ctx {
                            if let Ok(c) = libs::load_lib(&path, ctx) {
                                if !c.is_empty() {
                                    return Some(Err(anyhow::anyhow!(libs::ConflictingDefs(c))));
                                }
                            } else {
                                return Some(Ok(lib));
                            }
                        }
                        if load {
                            if let Some(path) = path.to_str() {
                                inkwell::support::load_library_permanently(path);
                            }
                        }
                        self.libs.push(lib.as_ref().into());
                        None
                    } else {
                        Some(Ok(lib))
                    }
                })
                .collect::<anyhow::Result<_>>()?;
        }
        if remaining.is_empty() {
            return Ok(vec![]);
        }
        // dynamic libraries, 1 deep
        for (path, libname) in lib_dirs
            .iter()
            .flat_map(|d| std::fs::read_dir(d).ok().into_iter().flatten())
            .filter_map(|entry| {
                let entry = entry.ok()?; // is it accessible?
                entry.file_type().ok()?.is_file().then_some(())?;
                let name = entry.file_name();
                let mut name = name.to_str()?;
                (windows || name.starts_with("lib")).then_some(())?; // "lib" prefix
                name = &name[..name.find(dyn_os_ext)?]; // check for matching extension and remove it
                name = &name[..name.find('.').unwrap_or(name.len())];
                Some((entry.path(), name[(!windows as usize * 3)..].to_string()))
                // remove "lib" prefix if not windows
            })
        {
            remaining = remaining
                .into_iter()
                .filter_map(|lib| {
                    // there should really be a try_retain, but this had to be done instead
                    if lib.as_ref() == libname {
                        if let Some(ctx) = ctx {
                            match libs::load_lib(&path, ctx) {
                                Ok(c) => {
                                    if !c.is_empty() {
                                        return Some(Err(anyhow::anyhow!(libs::ConflictingDefs(
                                            c
                                        ))));
                                    }
                                }
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        if load {
                            if let Some(path) = path.to_str() {
                                inkwell::support::load_library_permanently(path);
                            }
                        }
                        self.libs.push(lib.as_ref().into());
                        None
                    } else {
                        Some(Ok(lib))
                    }
                })
                .collect::<anyhow::Result<_>>()?;
        }
        if remaining.is_empty() {
            return Ok(vec![]);
        }
        // dynamic libraries
        for (path, libname) in lib_dirs
            .iter()
            .flat_map(|d| {
                walkdir::WalkDir::new(d)
                    .follow_links(true)
                    .contents_first(true)
                    .min_depth(2)
            })
            .filter_map(|entry| {
                let entry = entry.ok()?; // is it accessible?
                entry.file_type().is_file().then_some(())?;
                let name = entry.file_name();
                let mut name = name.to_str()?;
                (windows || name.starts_with("lib")).then_some(())?; // "lib" prefix
                name = &name[..name.find(dyn_os_ext)?]; // check for matching extension and remove it
                name = &name[..name.find('.').unwrap_or(name.len())];
                Some((
                    entry.path().to_path_buf(),
                    name[(!windows as usize * 3)..].to_string(),
                ))
                // remove "lib" prefix if not windows
            })
        {
            remaining = remaining
                .into_iter()
                .filter_map(|lib| {
                    // there should really be a try_retain, but this had to be done instead
                    if lib.as_ref() == libname {
                        if let Some(ctx) = ctx {
                            match libs::load_lib(&path, ctx) {
                                Ok(c) => {
                                    if !c.is_empty() {
                                        return Some(Err(anyhow::anyhow!(libs::ConflictingDefs(
                                            c
                                        ))));
                                    }
                                }
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        if load {
                            if let Some(path) = path.to_str() {
                                inkwell::support::load_library_permanently(path);
                            }
                        }
                        self.libs.push(lib.as_ref().into());
                        None
                    } else {
                        Some(Ok(lib))
                    }
                })
                .collect::<anyhow::Result<_>>()?;
        }
        if remaining.is_empty() {
            return Ok(vec![]);
        }
        // static libraries
        for (path, libname) in lib_dirs
            .iter()
            .flat_map(|d| {
                walkdir::WalkDir::new(d)
                    .follow_links(true)
                    .contents_first(true)
            })
            .filter_map(|entry| {
                let entry = entry.ok()?; // is it accessible?
                entry.file_type().is_file().then_some(())?;
                let name = entry.file_name();
                let mut name = name.to_str()?;
                (windows || name.starts_with("lib")).then_some(())?; // "lib" prefix
                name = &name[..name.find(".a")?]; // check for matching extension and remove it
                Some((
                    entry.path().to_path_buf(),
                    name[(!windows as usize * 3)..].to_string(),
                ))
                // remove "lib" prefix if not windows
            })
        {
            remaining = remaining
                .into_iter()
                .filter_map(|lib| {
                    // there should really be a try_retain, but this had to be done instead
                    if lib.as_ref() == libname {
                        if let Some(ctx) = ctx {
                            match libs::load_lib(&path, ctx) {
                                Ok(c) => if !c.is_empty() {
                                    return Some(Err(anyhow::anyhow!(libs::ConflictingDefs(c))));
                                },
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        if load {
                            cobalt_errors::warning!("{} was found for {libname}, but it is a static archive and can't be loaded", path.display());
                        }
                        self.libs.push(lib.as_ref().into());
                        None
                    } else {
                        Some(Ok(lib))
                    }
                })
                .collect::<anyhow::Result<_>>()?;
        }
        Ok(remaining)
    }
}
impl Default for CompileCommand {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
fn is_x86(s: &str) -> bool {
    let b = s.as_bytes();
    matches!(b, [b'i', _, b'8', b'6']) && b[0].is_ascii_digit()
}
fn is_arm32(s: &str) -> bool {
    s.starts_with("arm") && !s.contains("64")
}
fn is_arm64(s: &str) -> bool {
    s == "arm64" || s == "aarch64"
}

#[cfg(feature = "cc-build")]
mod impl_cc;
