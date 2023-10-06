use anyhow_std::*;
use cobalt_llvm::*;
use std::ffi::OsString;

impl super::CompileCommand {
    pub fn run(&mut self) -> anyhow::Result<()> {
        let native = inkwell::targets::TargetMachine::get_default_triple();
        let native = native.as_str().to_str()?;
        let triple = self.target_.as_deref().unwrap_or(native);
        let mut build = cc::Build::new();
        build
            .cargo_metadata(false)
            .warnings(false)
            .host(env!("HOST"))
            .target(triple)
            .opt_level(0)
            .debug(false);
        let tool = build.try_get_compiler()?;
        let mut cmd = tool.to_command();
        if tool.is_like_clang() || tool.is_like_gnu() {
            cmd.args(self.objects.iter().chain(&self.abss));
            for dir in self.lib_dirs()? {
                if !(dir.starts_with("/lib")
                    || dir.starts_with("/usr/lib")
                    || dir.starts_with("/lib32")
                    || dir.starts_with("/lib64")
                    || dir.starts_with("/usr/lib32")
                    || dir.starts_with("/usr/lib64"))
                {
                    let mut arg = OsString::with_capacity(dir.as_os_str().len() + 11);
                    arg.push("-Wl,-rpath,");
                    arg.push(&dir);
                    cmd.arg(arg);
                }
                let mut arg = OsString::with_capacity(dir.as_os_str().len() + 2);
                arg.push("-L");
                arg.push(dir);
                cmd.arg(arg);
            }
            for lib in &self.libs {
                let mut arg = OsString::with_capacity(lib.len() + 2);
                arg.push("-l");
                arg.push(lib);
                cmd.arg(arg);
            }
            cmd.arg("-o");
            cmd.arg(&self.output_file);
            if self.is_lib {
                cmd.arg("--shared");
            }
        } else {
            unimplemented!("Command isn't like GCC, I don't know how to handle it")
        }
        cmd.status_anyhow()?.exit_ok()
    }
}
