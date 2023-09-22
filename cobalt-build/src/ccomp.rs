use crate::*;
#[cfg(not(lld_enabled))]
mod prelude {
    pub use anyhow_std::*;
    pub use bstr::ByteSlice;
    pub use cc::{self, Tool};
    pub use os_str_bytes::OsStrBytes;
    pub use std::process::Command;
}
#[cfg(lld_enabled)]
mod prelude {}
#[allow(unused_imports)]
use prelude::*;
pub use std::ffi::OsString;
pub use std::path::PathBuf;
#[derive(Debug, Clone)]
pub struct CompileCommand {
    pub libs: Vec<OsString>,
    pub abss: Vec<PathBuf>,
    pub dirs: Vec<PathBuf>,
    pub objects: Vec<PathBuf>,
    pub output_file: PathBuf,
    pub target_: Option<String>,
    pub is_lib: bool,
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
            is_lib: false,
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
    pub fn target<S: Into<String>>(&mut self, target: S) -> &mut Self {
        self.target_ = Some(target.into());
        self.set_modified()
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
        Ok(cmd)
    }
    pub fn lib_dirs(&mut self) -> anyhow::Result<Vec<PathBuf>> {
        let tool = self.get_tool()?;
        let mut cmd = tool.to_command();
        cmd.arg("-print-search-dirs");
        let output = cmd.output()?;
        if !output.status.success() {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("cc -print-search-dirs failed with code {}", output.status),
            ))?
        }
        let stdout = output.stdout.as_bstr();
        let idx = stdout.find("libraries: =").ok_or(io::Error::new(
            io::ErrorKind::Other,
            "couldn't find libraries in output of cc -print-search-dirs",
        ))? + 12;
        let mut buf = &stdout[idx..];
        buf = &buf[..buf.find("\n").unwrap_or(buf.len())];
        Ok(buf
            .split_str(":")
            .map(OsStrBytes::assert_from_raw_bytes)
            .map(PathBuf::from)
            .collect())
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
    pub fn run_command(&mut self) -> anyhow::Result<impl FnOnce() -> std::io::Result<i32>> {
        let mut cmd = self.build_cmd()?;
        Ok(move || cmd.status().map(|s| s.code().unwrap_or(-1)))
    }
}
#[cfg(lld_enabled)]
impl CompileCommand {
    pub const USING_LLD: bool = true;
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
        todo!()
    }
    pub fn lib_dirs(&mut self) -> anyhow::Result<Vec<PathBuf>> {
        todo!()
    }
    pub fn run_command(&mut self) -> anyhow::Result<impl FnOnce() -> std::io::Result<i32>> {
        Ok(|| todo!())
    }
}
impl Default for CompileCommand {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
