use crate::*;
use anyhow_std::*;
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
    pub fn run(&mut self) -> anyhow::Result<()> {
        todo!()
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
        let mut out = vec![];
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
                from_env("CO_WASM32_LINK_DIRS", &mut out);
            }
            if wasm64 {
                from_env("CO_WASM64_LINK_DIRS", &mut out);
            }
            from_env("CO_WASM_LINK_DIRS", &mut out);
            return Ok(out);
        }
        fn append_paths<I: IntoIterator<Item = P>, P: Into<PathBuf>>(
            out: &mut Vec<PathBuf>,
            paths: I,
        ) {
            out.extend(paths.into_iter().map(Into::into).filter(|p| p.exists()))
        }
        let default = {
            let mut it = triple.split('-');
            let arch = it.next().unwrap_or("UNKNOWN").to_ascii_uppercase();
            it.next();
            let os = it.next().unwrap_or("UNKNOWN").to_ascii_uppercase();
            from_env(&format!("CO_{arch}_{os}_LINK_DIRS"), &mut out);
            from_env(&format!("CO_{arch}_LINK_DIRS"), &mut out);
            from_env(&format!("CO_{os}_LINK_DIRS"), &mut out);
            !(self.no_default_link
                || env_flag(&format!("CO_{arch}_{os}_NO_DEFAULT_LINK"))
                    .or_else(|| env_flag(&format!("CO_{arch}_NO_DEFAULT_LINK")))
                    .or_else(|| env_flag(&format!("CO_{os}_NO_DEFAULT_LINK")))
                    .unwrap_or(false))
        };
        if default {
            if compatible {
                if is_x86(triple) || is_arm32(triple) {
                    append_paths(&mut out, ["/usr/lib32", "/lib32"]);
                }
                // all 64-bit targets have 64 somewhere in their name
                if triple.contains("64") {
                    append_paths(&mut out, ["/usr/lib64", "/lib64"]);
                }
            }
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
    pub fn search_libs<
        L: AsRef<str>,
        I: IntoIterator<Item = L>,
        P: AsRef<Path>,
        D: IntoIterator<Item = P>,
    >(
        &mut self,
        libs: I,
        dirs: D,
        ctx: Option<&CompCtx>,
        load: bool,
    ) -> Result<Vec<L>, anyhow::Error> {
        let mut lib_dirs = self.lib_dirs()?;
        self.dirs
            .extend(dirs.into_iter().map(|p| p.as_ref().to_path_buf()));
        lib_dirs.extend_from_slice(&self.dirs);
        let mut remaining = libs.into_iter().collect::<Vec<_>>();
        let triple = inkwell::targets::TargetMachine::get_default_triple();
        let triple = self
            .target_
            .as_deref()
            .unwrap_or_else(|| triple.as_str().to_str().unwrap());
        let windows = triple.contains("windows");
        let dyn_os_ext = if windows {
            ".dll"
        } else if triple.contains("apple") {
            ".dylib"
        } else {
            ".so"
        };
        let sta_os_ext = if windows { ".lib" } else { ".a" };
        let mut conflicts = vec![];
        // dynamic libraries
        for dir in lib_dirs
            .iter()
            .map(AsRef::<Path>::as_ref)
            .filter_map(|dir| dir.read_dir().ok())
        {
            for (path, libname) in dir.filter_map(|entry| {
                let entry = entry.ok()?; // is it accessible?
                entry
                    .file_type()
                    .map_or(false, |x| x.is_file())
                    .then_some(())?; // is it a file?
                let name = entry.file_name();
                let mut name = name.to_str()?;
                (windows || name.starts_with("lib")).then_some(())?; // "lib" prefix
                name = &name[..name.find(dyn_os_ext)?]; // check for matching extension and remove it
                Some((entry.path(), name[(!windows as usize * 3)..].to_string()))
                // remove "lib" prefix if not windows
            }) {
                remaining = remaining
                    .into_iter()
                    .filter_map(|lib| {
                        // there should really be a try_retain, but this had to be done instead
                        if lib.as_ref() == libname {
                            if let Some(ctx) = ctx {
                                match libs::load_lib(&path, ctx) {
                                    Ok(mut libs) => conflicts.append(&mut libs),
                                    Err(e) => return Some(Err(e)),
                                }
                            }
                            if load {
                                match path.to_str_anyhow() {
                                    Ok(path) => inkwell::support::load_library_permanently(path),
                                    Err(e) => return Some(Err(e)),
                                };
                            }
                            self.libs.push(lib.as_ref().into());
                            None
                        } else {
                            Some(Ok(lib))
                        }
                    })
                    .collect::<anyhow::Result<_>>()?;
            }
        }
        // static libraries
        for dir in lib_dirs
            .iter()
            .map(AsRef::<Path>::as_ref)
            .filter_map(|dir| dir.read_dir().ok())
        {
            for libname in dir.filter_map(|entry| {
                let entry = entry.ok()?; // is it accessible?
                entry
                    .file_type()
                    .map_or(false, |x| x.is_file())
                    .then_some(())?; // is it a file?
                let name = entry.file_name();
                let mut name = name.to_str()?;
                (windows || name.starts_with("lib")).then_some(())?; // "lib" prefix
                name = &name[..name.find(sta_os_ext)?]; // check for matching extension and remove it
                Some(name[(!windows as usize * 3)..].to_string()) // remove "lib" prefix if not windows
            }) {
                remaining.retain(|lib| {
                    // we can use retain here beause there aren't any errors
                    if lib.as_ref() == libname {
                        self.libs.push(lib.as_ref().into());
                        false
                    } else {
                        true
                    }
                });
            }
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
