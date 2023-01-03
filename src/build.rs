#![allow(non_snake_case)]
#![allow(unused_variables)]
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::ffi::{OsStr, OsString};
use std::process::Command;
use std::cell::RefCell;
use serde::*;
use colored::Colorize;
use either::Either;
use semver::{Version, VersionReq};
use path_dedot::ParseDot;
use cobalt::context::CompCtx;
use super::{libs, opt};
#[derive(Debug, Clone, Deserialize)]
pub struct Project {
    pub name: String,
    pub version: Version,
    pub author: Option<String>,
    pub co_version: Option<VersionReq>,
    #[serde(alias = "description")]
    pub desc: Option<String>,
    #[serde(alias = "target")]
    targets: Option<Vec<Target>>,
    #[serde(alias = "lib")]
    library: Option<Vec<Library>>,
    #[serde(alias = "exe")]
    #[serde(alias = "bin")]
    #[serde(alias = "executable")]
    executable: Option<Vec<Executable>>,
    meta: Option<Vec<Meta>>
}
impl Project {
    pub fn into_targets(self) -> impl Iterator<Item = Target> {
        self.targets.unwrap_or(vec![]).into_iter()
        .chain(self.executable.unwrap_or(vec![]).into_iter().map(|Executable {name, files, deps, needs_crt}| Target {target_type: TargetType::Executable, name, files, deps, needs_crt}))
        .chain(self.library.unwrap_or(vec![]).into_iter().map(|Library {name, files, deps, needs_crt}| Target {target_type: TargetType::Library, name, files, deps, needs_crt}))
        .chain(self.meta.unwrap_or(vec![]).into_iter().map(|Meta {name, deps, needs_crt}| Target {target_type: TargetType::Library, files: None, name, deps, needs_crt}))
    }
}
#[derive(Debug, Clone, Deserialize)]
pub enum TargetType {
    #[serde(rename = "exe")]
    #[serde(alias = "executable")]
    #[serde(alias = "bin")]
    #[serde(alias = "binary")]
    Executable,
    #[serde(rename = "lib")]
    #[serde(alias = "library")]
    Library,
    #[serde(rename = "meta")]
    Meta
}
#[derive(Debug, Clone, Deserialize)]
pub struct Target {
    pub name: String,
    #[serde(default)]
    #[serde(alias = "needs-crt")]
    #[serde(alias = "needs_libc")]
    #[serde(alias = "needs-libc")]
    pub needs_crt: bool,
    #[serde(rename = "type")]
    pub target_type: TargetType,
    #[serde(with = "either::serde_untagged_optional")]
    pub files: Option<Either<String, Vec<String>>>,
    #[serde(alias = "dependencies")]
    pub deps: Option<HashMap<String, String>>
}
#[derive(Debug, Clone, Deserialize)]
struct Executable {
    pub name: String,
    #[serde(default)]
    #[serde(alias = "needs-crt")]
    #[serde(alias = "needs_libc")]
    #[serde(alias = "needs-libc")]
    pub needs_crt: bool,
    #[serde(with = "either::serde_untagged_optional")]
    pub files: Option<Either<String, Vec<String>>>,
    #[serde(alias = "dependencies")]
    pub deps: Option<HashMap<String, String>>
}
#[derive(Debug, Clone, Deserialize)]
struct Library {
    pub name: String,
    #[serde(default)]
    #[serde(alias = "needs-crt")]
    #[serde(alias = "needs_libc")]
    #[serde(alias = "needs-libc")]
    pub needs_crt: bool,
    #[serde(with = "either::serde_untagged_optional")]
    pub files: Option<Either<String, Vec<String>>>,
    #[serde(alias = "dependencies")]
    pub deps: Option<HashMap<String, String>>
}
#[derive(Debug, Clone, Deserialize)]
struct Meta {
    pub name: String,
    #[serde(default)]
    #[serde(alias = "needs-crt")]
    #[serde(alias = "needs_libc")]
    #[serde(alias = "needs-libc")]
    pub needs_crt: bool,
    #[serde(alias = "dependencies")]
    pub deps: Option<HashMap<String, String>>
}
#[derive(Debug, Clone)]
pub struct BuildOptions<'a, 'b, 'c, 'd> {
    pub source_dir: &'a Path,
    pub build_dir: &'b Path,
    pub continue_build: bool,
    pub continue_comp: bool,
    pub triple: &'c inkwell::targets::TargetTriple,
    pub profile: &'d str
}
enum LibInfo {
    Name(String),
    Path(PathBuf)
}
#[derive(Default)]
struct TargetData {
    pub libs: Vec<LibInfo>,
    pub deps: Vec<String>,
    pub needs_crt: bool
}
impl TargetData {
    pub fn init_lib(&mut self, name: &str, path: &Path, targets: &HashMap<String, (Target, RefCell<Option<TargetData>>)>) {
        for lib in self.libs.iter_mut() {
            if let LibInfo::Name(l) = lib {
                if l == name {
                    *lib = LibInfo::Path(path.to_path_buf());
                }
            }
        }
        for dep in self.deps.iter() {
            targets.get(dep).expect("Dependency should exist!").1.borrow_mut().as_mut().expect("Dependency should be initialized!").init_lib(name, path, targets);
        }
    }
    fn missing_libs<'a, 'b: 'a>(&'b self, targets: &'a HashMap<String, (Target, RefCell<Option<TargetData>>)>) -> Vec<String> {
        let mut out = self.libs.iter().filter_map(|lib| if let LibInfo::Name(lib) = lib {Some(lib.clone())} else {None}).collect::<Vec<_>>();
        for dep in self.deps.iter() {out.extend(targets.get(dep).expect("Dependency should exist!").1.borrow().as_ref().expect("Dependency should be initialized!").missing_libs(targets))}
        out
    }
    pub fn init_all(&mut self, targets: &HashMap<String, (Target, RefCell<Option<TargetData>>)>, link_dirs: Vec<&str>) -> Result<(), i32> {
        let ERROR = "error".bright_red().bold();
        let mut libs = self.missing_libs(targets);
        libs.sort();
        libs.dedup();
        let (libs, notfound) = libs::find_libs(libs.iter().map(|l| l.as_str()).collect(), link_dirs);
        for nf in notfound.iter() {eprintln!("{ERROR}: couldn't find library {nf}");}
        if notfound.len() > 0 {return Err(102)}
        libs.into_iter().for_each(|(path, name)| self.init_lib(name, &path, targets));
        Ok(())
    }
    pub fn initialized_libs<'a, 'b: 'a>(&'b self, targets: &'a HashMap<String, (Target, RefCell<Option<TargetData>>)>) -> Vec<PathBuf> {
        let mut out = self.libs.iter().filter_map(|lib| if let LibInfo::Path(p) = lib {Some(p.clone())} else {None}).collect::<Vec<_>>();
        for dep in self.deps.iter() {out.extend(targets.get(dep).expect("Dependency should exist!").1.borrow().as_ref().expect("Dependency should be initialized!").initialized_libs(targets))}
        out
    }
}
static mut FILENAME: String = String::new();
fn clear_mod<'ctx>(this: &mut HashMap<String, cobalt::Symbol<'ctx>>, module: &inkwell::module::Module<'ctx>) {
    for (_, sym) in this.iter_mut() {
        match sym {
            cobalt::Symbol::Module(m) => clear_mod(m, module),
            cobalt::Symbol::Variable(v) => if let Some(inkwell::values::BasicValueEnum::PointerValue(pv)) = v.comp_val {
                let t = inkwell::types::BasicTypeEnum::try_from(pv.get_type().get_element_type());
                if let Ok(t) = t {
                    v.comp_val = Some(inkwell::values::BasicValueEnum::PointerValue(module.add_global(t, None, pv.get_name().to_str().expect("Global variable should have a name!")).as_pointer_value()));
                }
            }
        }
    }
}
fn build_file<'ctx>(path: &Path, ctx: &mut CompCtx<'ctx>, opts: &BuildOptions) -> Result<PathBuf, i32> {
    let ERROR = &"error".bright_red().bold();
    let WARNING = &"warning".bright_yellow().bold();
    let MODULE = &"module".blue().bold();
    let name = path.to_str().expect("File name must be valid UTF-8");
    let flags = cobalt::Flags::default();
    ctx.module.set_name(name);
    ctx.module.set_source_file_name(name);
    let fname = unsafe {&mut FILENAME};
    let mut fail = false;
    let mut overall_fail = false;
    let code = match std::fs::read_to_string(path.parse_dot().unwrap()) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("error occured when trying to open file: {e}");
            return Err(100)
        }
    };
    let (toks, errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
    for err in errs {
        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
        for note in err.notes {
            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
        }
    }
    if fail && !opts.continue_comp {return Err(101)}
    let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
    fail = false;
    for err in errs {
        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
        for note in err.notes {
            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
        }
    }
    if fail && !opts.continue_comp {return Err(101)}
    let (_, errs) = ast.codegen(&ctx);
    fail = false;
    for err in errs {
        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
        for note in err.notes {
            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
        }
    }
    if fail && !opts.continue_comp {
        let new = ctx.context.create_module("");
        new.set_triple(&ctx.module.get_triple());
        ctx.with_vars(|v| clear_mod(&mut v.symbols, &new));
        ctx.module = new;
        return Err(101)
    }
    if let Err(msg) = ctx.module.verify() {
        eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
        let new = ctx.context.create_module("");
        new.set_triple(&ctx.module.get_triple());
        ctx.with_vars(|v| clear_mod(&mut v.symbols, &new));
        ctx.module = new;
        return Err(101)
    }
    if overall_fail {
        let new = ctx.context.create_module("");
        new.set_triple(&ctx.module.get_triple());
        ctx.with_vars(|v| clear_mod(&mut v.symbols, &new));
        ctx.module = new;
        return Err(101)
    }
    let pm = inkwell::passes::PassManager::create(());
    opt::load_profile(Some(opts.profile), &pm);
    pm.run_on(&ctx.module);
    let target_machine = inkwell::targets::Target::from_triple(opts.triple).unwrap().create_target_machine(
        opts.triple,
        "",
        "",
        inkwell::OptimizationLevel::None,
        inkwell::targets::RelocMode::PIC,
        inkwell::targets::CodeModel::Small
    ).expect("failed to create target machine");
    let mut out_path = opts.build_dir.to_path_buf();
    out_path.push(".artifacts");
    out_path.push(path.strip_prefix(opts.source_dir).unwrap_or(path));
    out_path.set_extension("o");
    if let Err(e) = if out_path.parent().unwrap().exists() {Ok(())} else {std::fs::create_dir_all(out_path.parent().unwrap())} {
        eprintln!("error when creating directory {}: {e}", out_path.parent().unwrap().display());
        return Err(100)
    }
    target_machine.write_to_file(&ctx.module, inkwell::targets::FileType::Object, out_path.as_path()).unwrap();
    let new = ctx.context.create_module("");
    new.set_triple(&ctx.module.get_triple());
    ctx.with_vars(|v| clear_mod(&mut v.symbols, &new));
    ctx.module = new;
    Ok(out_path)
}
fn build_target<'ctx>(t: &Target, data: &RefCell<Option<TargetData>>, targets: &HashMap<String, (Target, RefCell<Option<TargetData>>)>, ctx: &mut CompCtx<'ctx>, opts: &BuildOptions) -> i32 {
    let ERROR = "error".bright_red().bold();
    match t.target_type {
        TargetType::Executable => {
            if t.needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
            for (target, version) in t.deps.as_ref().unwrap_or(&HashMap::new()).iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            eprintln!("{ERROR}: target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {return res}
                        if d.borrow().as_ref().unwrap().needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        todo!("externally downloaded projects aren't implemented")
                    }
                    else {
                        eprintln!("{ERROR}: unknown version specification {x:?}");
                        return 107
                    }
                }
            }
            let mut paths = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => for file in (match glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("error in file glob: {e}");
                        return 108;
                    }
                }).filter_map(Result::ok) {
                    if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                    match build_file(file.as_path(), ctx, opts) {
                        Ok(path) => paths.push(path),
                        Err(code) => return code
                    }
                },
                Some(either::Right(files)) => for file in files.iter().filter_map(|f| match glob::glob((opts.source_dir.to_str()?.to_string() + "/" + f).as_str()) {
                    Ok(f) => Some(f),
                    Err(e) => {
                        eprintln!("error in file glob: {e}");
                        None
                    }
                }).flatten().filter_map(Result::ok) {
                    if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                    match build_file(file.as_path(), ctx, opts) {
                        Ok(path) => paths.push(path),
                        Err(code) => return code
                    }
                },
                None => {
                    eprintln!("{ERROR}: executable target must have files");
                    return 106;
                }
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(&t.name);
            let mut args = vec![OsString::from("-o"), output.into_os_string()];
            args.extend(paths.into_iter().map(|x| x.into_os_string()));
            if let Err(e) = data.borrow_mut().as_mut().unwrap().init_all(targets, vec!["/usr/local/lib/", "/usr/lib/", "/lib/"]) {return e}
            for lib in data.borrow().as_ref().unwrap().initialized_libs(targets) {
                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                args.push(OsString::from("-L"));
                args.push(parent.clone());
                args.push(OsString::from("-rpath"));
                args.push(parent);
                args.push(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
            }
            if data.borrow().as_ref().unwrap().needs_crt {
                Command::new("cc").args(args.iter()).status()
                .or_else(|_| Command::new("clang").args(args.iter()).status())
                .or_else(|_| Command::new("gcc").args(args.iter()).status())
                .ok().and_then(|x| x.code()).unwrap_or(0)
            }
            else {Command::new("ld").args(args).status().ok().and_then(|x| x.code()).unwrap_or(0)}
        },
        TargetType::Library => {
            if t.needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
            for (target, version) in t.deps.as_ref().unwrap_or(&HashMap::new()).iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            eprintln!("{ERROR}: target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {return res}
                        if d.borrow().as_ref().unwrap().needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        todo!("externally downloaded projects aren't implemented")
                    }
                    else {
                        eprintln!("{ERROR}: unknown version specification {x:?}");
                        return 107
                    }
                }
            }
            let mut paths = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => for file in (match glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("error in file glob: {e}");
                        return 108;
                    }
                }).filter_map(Result::ok) {
                    if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                    match build_file(file.as_path(), ctx, opts) {
                        Ok(path) => paths.push(path),
                        Err(code) => return code
                    }
                },
                Some(either::Right(files)) => for file in files.iter().filter_map(|f| match glob::glob((opts.source_dir.to_str()?.to_string() + "/" + f).as_str()) {
                    Ok(f) => Some(f),
                    Err(e) => {
                        eprintln!("error in file glob: {e}");
                        None
                    }
                }).flatten().filter_map(Result::ok) {
                    if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                    match build_file(file.as_path(), ctx, opts) {
                        Ok(path) => paths.push(path),
                        Err(code) => return code
                    }
                },
                None => {
                    eprintln!("{ERROR}: library target must have files");
                    return 106;
                }
            }
            let mut output = opts.build_dir.to_path_buf();
            output.push(format!("{}.colib", t.name));
            Command::new("ar").args([OsStr::new("-rcs"), output.as_os_str()].into_iter().chain(paths.iter().map(|f| f.as_os_str()))).status().ok().and_then(|x| x.code()).unwrap_or_else(|| {
                eprintln!("{ERROR}: could not invoke ar");
                109
            })
        },
        TargetType::Meta => {
            if t.files.is_some() {
                eprintln!("{ERROR}: meta target cannot have files");
                return 106;
            }
            if t.needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
            for (target, version) in t.deps.as_ref().unwrap_or(&HashMap::new()).iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            eprintln!("{ERROR}: target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {return res}
                        if d.borrow().as_ref().unwrap().needs_crt {data.borrow_mut().as_mut().unwrap().needs_crt = true;}
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        todo!("externally downloaded projects aren't implemented")
                    }
                    else {
                        eprintln!("{ERROR}: unknown version specification {x:?}");
                        return 107
                    }
                }
            }
            0
        }
    }
}
pub fn build(pkg: Project, to_build: Option<Vec<String>>, opts: &BuildOptions) -> i32 {
    let ERROR = "error".bright_red().bold();
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            eprintln!(r#"{ERROR}: project has Cobalt version requirement "{v}", but Cobalt version is {}"#, env!("CARGO_PKG_VERSION"));
            return 104;
        }
    }
    let targets = pkg.into_targets().map(|t| (t.name.clone(), (t, RefCell::new(None)))).collect::<HashMap<String, (Target, RefCell<Option<TargetData>>)>>();
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&target_name) {t} else {
                eprintln!("{ERROR}: target {target_name:?} is not a target in this project");
                return 105;
            };
            if data.borrow().is_some() {continue}
            else {*data.borrow_mut() = Some(TargetData::default())}
            let res = build_target(target, data, &targets, &mut ctx, opts);
            if res != 0 {return res}
        }
    }
    else {
        for (_, (target, data)) in targets.iter() {
            if data.borrow().is_some() {continue}
            else {*data.borrow_mut() = Some(TargetData::default())}
            let res = build_target(target, data, &targets, &mut ctx, opts);
            if res != 0 {return res}
        }
    }
    0
}
