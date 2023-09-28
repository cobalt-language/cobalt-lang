use crate::*;
pub use anyhow_std::*;
#[cfg(not(lld_enabled))]
mod prelude {
    pub use bstr::ByteSlice;
    pub use cc::{self, Tool};
    pub use std::process::Command;
}
#[cfg(lld_enabled)]
mod prelude {
    pub use os_str_bytes::{OsStrBytes, OsStringBytes};
    pub use std::borrow::Cow;
}
#[allow(unused_imports)]
use prelude::*;
pub use std::ffi::*;
pub use std::path::PathBuf;
#[derive(Debug, Clone)]
pub struct CompileCommand {
    pub libs: Vec<OsString>,
    pub abss: Vec<PathBuf>,
    pub dirs: Vec<PathBuf>,
    pub objects: Vec<PathBuf>,
    pub output_file: PathBuf,
    pub target_: Option<String>,
    pub extras: Vec<OsString>,
    pub is_lib: bool,
    pub no_default_link: bool,
    #[cfg(not(lld_enabled))]
    tool: Option<Tool>,
    #[cfg(not(lld_enabled))]
    modified: bool,
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
            extras: vec![],
            is_lib: false,
            no_default_link: false,
            #[cfg(not(lld_enabled))]
            tool: None,
            #[cfg(not(lld_enabled))]
            modified: true,
        }
    }

    #[cfg(lld_enabled)]
    fn set_modified(&mut self) -> &mut Self {
        self
    }
    #[cfg(not(lld_enabled))]
    fn set_modified(&mut self) -> &mut Self {
        self.modified = true;
        self
    }
    pub fn obj<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.objects.push(path.into());
        self.set_modified()
    }
    pub fn objs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(&mut self, paths: I) -> &mut Self {
        self.objects
            .extend(paths.into_iter().map(Into::<PathBuf>::into));
        self.set_modified()
    }
    pub fn link_dir<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.dirs.push(path.into());
        self.set_modified()
    }
    pub fn link_dirs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.dirs.extend(paths.into_iter().map(Into::into));
        self.set_modified()
    }
    pub fn link_lib<P: Into<OsString>>(&mut self, path: P) -> &mut Self {
        self.libs.push(path.into());
        self.set_modified()
    }
    pub fn link_libs<P: Into<OsString>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.libs.extend(paths.into_iter().map(Into::into));
        self.set_modified()
    }
    pub fn link_abs<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.abss.push(path.into());
        self.set_modified()
    }
    pub fn link_abss<P: Into<PathBuf>, I: IntoIterator<Item = P>>(
        &mut self,
        paths: I,
    ) -> &mut Self {
        self.abss.extend(paths.into_iter().map(Into::into));
        self.set_modified()
    }
    pub fn output<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.output_file = path.into();
        self.set_modified()
    }
    pub fn lib(&mut self, is_lib: bool) -> &mut Self {
        self.is_lib = is_lib;
        self.set_modified()
    }
    pub fn extra_arg<A: Into<OsString>>(&mut self, arg: A) -> &mut Self {
        self.extras.push(arg.into());
        self.set_modified()
    }
    pub fn extra_args<A: Into<OsString>, I: IntoIterator<Item = A>>(
        &mut self,
        args: I,
    ) -> &mut Self {
        self.extras.extend(args.into_iter().map(Into::into));
        self.set_modified()
    }
    pub fn target<S: Into<String>>(&mut self, target: S) -> &mut Self {
        self.target_ = Some(target.into());
        self.set_modified()
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
    ) -> anyhow::Result<Vec<L>> {
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
                name = &name[..name
                    .find(".a")
                    .or_else(|| windows.then(|| name.find(".lib")).flatten())?]; // check for matching extension and remove it
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
        if triple.contains("wasm") {
            // WASM doesn't have system link dirs
            return Ok(vec![]);
        }
        let mut out = vec![];
        fn append_paths<I: IntoIterator<Item = P>, P: Into<PathBuf>>(
            out: &mut Vec<PathBuf>,
            paths: I,
        ) {
            out.extend(paths.into_iter().map(Into::into).filter(|p| p.exists()))
        }
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
            let Some(info) = it.next() else {break 'specific};
            let trip = format!("{arch}-{os}-{info}");
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
}
#[cfg(not(lld_enabled))]
impl CompileCommand {
    pub const USING_LLD: bool = false;
    fn init_clang(&self, cmd: &mut Command) {
        self.init_gnu(cmd)
    }
    fn init_gnu(&self, cmd: &mut Command) {
        cmd.args(&self.objects);
        cmd.arg("-o");
        cmd.arg(&self.output_file);
        if self.is_lib {
            cmd.arg("-shared");
        }
        for dir in &self.dirs {
            if !dir.exists() {
                continue;
            }
            let mut l = OsString::from("-L");
            l.push(dir);
            cmd.arg(l);
            cmd.arg("-rpath");
            cmd.arg(dir);
        }
        for lib in &self.libs {
            let mut l = OsString::from("-l");
            l.push(lib);
            cmd.arg(l);
        }
        for lib in &self.abss {
            if !lib.exists() {
                continue;
            }
            cmd.arg(lib);
        }
    }
    #[allow(unreachable_code, unused_variables)]
    fn init_msvc(&self, cmd: &mut Command) {
        panic!("MSVC is pain, use MinGW");
        cmd.args(&self.objects);
        let mut a = OsString::from("/OUT:");
        a.push(&self.output_file);
        cmd.arg(a);
        if self.is_lib {
            cmd.arg("/DLL");
        }
    }
    fn init_tool(&mut self) -> Result<(), cc::Error> {
        let mut build = cc::Build::new();
        build
            .opt_level(0)
            .cargo_metadata(false)
            .warnings(false)
            .host(env!("HOST"));
        let default = inkwell::targets::TargetMachine::get_default_triple();
        let default = default.as_str().to_str().unwrap();
        build.target(self.target_.as_deref().unwrap_or(default));
        if let Some(target) = self.target_.as_ref() {
            build.target(target);
        }
        build.flag_if_supported("-nodefaultlibs");
        self.tool = Some(build.try_get_compiler()?);
        self.modified = false;
        Ok(())
    }
    fn get_tool(&mut self) -> Result<&Tool, cc::Error> {
        if self.modified || self.tool.is_none() {
            self.init_tool()?
        }
        Ok(self.tool.as_ref().unwrap())
    }
    fn build_cmd(&mut self) -> anyhow::Result<Command> {
        let tool = self.get_tool()?;
        let mut cmd = tool.to_command();
        if tool.is_like_clang() {
            self.init_clang(&mut cmd)
        } else if tool.is_like_gnu() {
            self.init_gnu(&mut cmd)
        } else if tool.is_like_msvc() {
            self.init_msvc(&mut cmd)
        } else {
            panic!("C compiler is not like Clang, GNU, or MSVC!")
        }
        cmd.args(self.extras.clone());
        Ok(cmd)
    }
    pub fn run_command(&mut self) -> anyhow::Result<impl FnOnce() -> std::io::Result<i32>> {
        let mut cmd = self.build_cmd()?;
        Ok(move || cmd.status().map(|s| s.code().unwrap_or(-1)))
    }
}
#[cfg(lld_enabled)]
impl CompileCommand {
    pub const USING_LLD: bool = true;
    /// Find the dynamic linker
    /// Assumes a linux system
    fn dynamic_linker(triple: &str) -> anyhow::Result<&'static str> {
        let arch = triple
            .split_once('-')
            .ok_or(anyhow::anyhow!("invalid triple \"{triple}\""))?
            .0;
        if arch == "x86_64" {
            Ok("/lib64/ld-linux-x86-64.so.2")
        } else if is_x86(arch) {
            Ok("/lib/ld-linux.so.2")
        } else if is_arm32(arch) {
            Ok("/lib/ld-linux.so.3")
        } else if is_arm64(arch) {
            Ok("/lib/ld-linux-aarch64.so.1")
        } else {
            anyhow::bail!("unknown arch \"{triple}\"")
        }
    }
    pub fn run_command(&mut self) -> anyhow::Result<impl FnOnce() -> std::io::Result<i32> + '_> {
        fn make_flag<D: std::ops::Deref<Target = A>, A: OsStrBytes + ?Sized>(
            prefix: &[u8],
            arg: &D,
        ) -> Cow<'static, [u8]> {
            let arg = arg.to_raw_bytes();
            let mut out = Vec::with_capacity(prefix.len() + arg.len());
            out.extend_from_slice(prefix);
            out.extend_from_slice(&arg);
            out.into()
        }
        let libs = self.lib_dirs()?;
        let args = std::iter::once::<Cow<[u8]>>(b"lld"[..].into())
            .chain(self.objects.iter().map(|p| p.to_raw_bytes()))
            .chain(
                libs.into_iter()
                    .map(Cow::Owned)
                    .chain(self.dirs.iter().map(|p| Cow::Borrowed(&**p)))
                    .flat_map(|d| {
                        let nonstandard = !(d.starts_with("/lib") || d.starts_with("/usr/lib"));
                        if nonstandard {
                            [Some(make_flag(b"-L", &d)), None, None]
                        } else {
                            [
                                Some(make_flag(b"-L", &d)),
                                Some(b"-R"[..].into()),
                                Some(match d {
                                    Cow::Borrowed(d) => d.to_raw_bytes(),
                                    Cow::Owned(d) => d.into_raw_vec().into(),
                                }),
                            ]
                        }
                    })
                    .flatten(),
            )
            .chain(self.abss.iter().map(|p| p.to_raw_bytes()))
            .chain(self.libs.iter().map(|l| make_flag(b"-l", l)))
            .chain([b"-o"[..].into(), self.output_file.to_raw_bytes()])
            .chain(self.extras.iter().map(|a| a.to_raw_bytes()));
        use lld::LldFlavor;
        let native = inkwell::targets::TargetMachine::get_default_triple();
        let native = native.as_str().to_str().unwrap();
        let triple = self.target_.as_deref().unwrap_or(native);
        let (flavor, args) = if triple.contains("wasm") {
            (
                LldFlavor::Wasm,
                args.chain([None, None, None].into_iter().flatten()),
            )
        } else if triple.contains("windows") {
            (
                LldFlavor::Coff,
                args.chain([None, None, None].into_iter().flatten()),
            )
        } else if triple.contains("apple") || triple.contains("ios") || triple.contains("darwin") {
            (
                LldFlavor::MachO,
                args.chain(
                    [
                        Some(b"-dynamic"[..].into()),
                        self.is_lib.then_some(b"-dylib"[..].into()),
                        None,
                    ]
                    .into_iter()
                    .flatten(),
                ),
            )
        } else {
            (
                LldFlavor::Elf,
                args.chain(
                    [
                        Some(b"-dynamic-linker"[..].into()),
                        Some(Self::dynamic_linker(triple)?.as_bytes().into()),
                        self.is_lib.then_some(b"--shared"[..].into()),
                    ]
                    .into_iter()
                    .flatten(),
                ),
            )
        };
        Ok(move || Ok(lld::lld_link(args, None, None, flavor)))
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
