use std::process::Command;
use std::path::PathBuf;
use std::ffi::OsString;
#[derive(Debug, Clone, Default)]
pub struct CompileCommand {
    pub libs: Vec<PathBuf>,
    pub objects: Vec<PathBuf>,
    pub output_file: PathBuf,
    pub target_: Option<String>,
    pub is_lib: bool
}
impl CompileCommand {
    pub fn new() -> Self {Self::default()}
    pub fn obj<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.objects.push(path.into());
        self
    }
    pub fn objs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(&mut self, paths: I) -> &mut Self {
        self.objects.extend(paths.into_iter().map(Into::<PathBuf>::into));
        self
    }
    #[allow(dead_code)]
    pub fn link_lib<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        self.libs.push(path.into());
        self
    }
    pub fn link_libs<P: Into<PathBuf>, I: IntoIterator<Item = P>>(&mut self, paths: I) -> &mut Self {
        self.libs.extend(paths.into_iter().map(Into::<PathBuf>::into));
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
    fn init_clang(&self, cmd: &mut Command) {self.init_gnu(cmd)}
    fn init_gnu(&self, cmd: &mut Command) {
        cmd.args(&self.objects);
        cmd.arg("-o");
        cmd.arg(&self.output_file);
        if self.is_lib {cmd.arg("-shared");}
        for lib in &self.libs {
            let parent = lib.parent().unwrap();
            let mut a = OsString::from("-L");
            a.push(&parent);
            cmd.arg(a);
            cmd.arg("-rpath");
            cmd.arg(parent);
            a = OsString::from("-l:");
            a.push(lib.file_name().unwrap());
            cmd.arg(a);
        }
    }
    #[allow(unreachable_code, unused_variables)]
    fn init_msvc(&self, cmd: &mut Command) {
        panic!("MSVC is pain, use MinGW");
        cmd.args(&self.objects);
        let mut a = OsString::from("/OUT:");
        a.push(&self.output_file);
        cmd.arg(a);
        if self.is_lib {cmd.arg("/DLL");}
        for lib in &self.libs {
            std::mem::drop((cmd, lib));
        }
    }
    pub fn build(&self) -> Result<Command, cc::Error> {
        let mut build = cc::Build::new();
        build.opt_level(0).cargo_metadata(false).warnings(false);
        let default = inkwell::targets::TargetMachine::get_default_triple();
        let default = default.as_str().to_str().unwrap();
        build.target(self.target_.as_deref().unwrap_or(default));
        if let Some(target) = self.target_.as_ref() {build.target(&target);}
        let tool = build.try_get_compiler()?;
        let mut cmd = tool.to_command();
        if tool.is_like_clang() {self.init_clang(&mut cmd)}
        else if tool.is_like_gnu() {self.init_gnu(&mut cmd)}
        else if tool.is_like_msvc() {self.init_msvc(&mut cmd)}
        else {panic!("C compiler is not like Clang, GNU, or MSVC!")}
        Ok(cmd)
    }
}