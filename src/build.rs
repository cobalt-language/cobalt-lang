use codespan_reporting::term::{self, termcolor::{ColorChoice, StandardStream}};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::ffi::OsString;
use std::io::{Read, Write, BufWriter};
use std::process::{Command, exit};
use std::cell::RefCell;
use std::borrow::Cow;
use serde::*;
use either::Either;
use semver::{Version, VersionReq};
use path_calculate::*;
use cobalt::{CompCtx, Value, Type, InterData, AST, ast::TopLevelAST, errors::FILES};
use super::{libs, opt, package, warning, error};
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
    library: Option<Vec<SpecialTarget>>,
    #[serde(alias = "exe", alias = "bin", alias = "binary")]
    executable: Option<Vec<SpecialTarget>>,
    meta: Option<Vec<SpecialTarget>>
}
impl Project {
    pub fn into_targets(self) -> impl Iterator<Item = Target> {
        self.targets.unwrap_or_default().into_iter().map(|x| {
            if x.target_type == TargetType::Meta && x.files.is_some() {
                error!("meta target (name: {}) cannot have files", x.name);
                exit(100)
            }
            x
        })
        .chain(self.executable.unwrap_or_default().into_iter().map(|SpecialTarget {name, files, deps}| Target {target_type: TargetType::Executable, name, files, deps}))
        .chain(self.library.unwrap_or_default().into_iter().map(|SpecialTarget {name, files, deps}| Target {target_type: TargetType::Library, name, files, deps}))
        .chain(self.meta.unwrap_or_default().into_iter().map(|SpecialTarget {name, files, deps}| {
            if files.is_some() {
                error!("meta target (name: {name}) cannot have files");
                exit(100)
            }
            Target {target_type: TargetType::Meta, files: None, name, deps}
        }))
    }
    pub fn target_type(&self, name: &str) -> Option<TargetType> {
        self.targets.as_ref().and_then(|v| v.iter().find_map(|x| (x.name == name).then_some(x.target_type)))
        .or_else(|| self.executable.as_ref().and_then(|v| v.iter().any(|x| x.name == name).then_some(TargetType::Executable)))
        .or_else(|| self.library.as_ref().and_then(|v| v.iter().any(|x| x.name == name).then_some(TargetType::Library)))
        .or_else(|| self.meta.as_ref().and_then(|v| v.iter().any(|x| x.name == name).then_some(TargetType::Meta)))
    }
    pub fn get_exe<'a>(&'a self) -> Vec<&'a str> {
        let mut out = Vec::with_capacity(self.targets.as_ref().map_or(0, |v| v.len()) + self.executable.as_ref().map_or(0, |v| v.len()));
        if let Some(ref v) = self.targets {out.extend(v.iter().filter_map(|x| (x.target_type == TargetType::Executable).then_some(x.name.as_str())))}
        if let Some(ref v) = self.executable {out.extend(v.iter().map(|x| x.name.as_str()))}
        out
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum TargetType {
    #[serde(rename = "exe", alias = "executable", alias = "bin", alias = "binary")]
    Executable,
    #[serde(rename = "lib", alias = "library")]
    Library,
    #[serde(rename = "meta")]
    Meta
}
#[derive(Debug, Clone, Deserialize)]
pub struct Target {
    pub name: String,
    #[serde(rename = "type")]
    pub target_type: TargetType,
    #[serde(with = "either::serde_untagged_optional")]
    pub files: Option<Either<String, Vec<String>>>,
    #[serde(default, alias = "dependencies")]
    pub deps: HashMap<String, String>
}
#[derive(Debug, Clone, Deserialize)]
struct SpecialTarget {
    pub name: String,
    #[serde(with = "either::serde_untagged_optional")]
    pub files: Option<Either<String, Vec<String>>>,
    #[serde(default, alias = "dependencies")]
    pub deps: HashMap<String, String>
}
#[derive(Debug, Clone)]
pub struct BuildOptions<'a> {
    pub source_dir: &'a Path,
    pub build_dir: &'a Path,
    pub continue_build: bool,
    pub continue_comp: bool,
    pub triple: &'a inkwell::targets::TargetTriple,
    pub profile: &'a str,
    pub link_dirs: Vec<&'a str>
}
enum LibInfo {
    Name(String),
    Path(PathBuf)
}
#[derive(Default)]
struct TargetData {
    pub libs: Vec<LibInfo>,
    pub deps: Vec<String>
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
    pub fn init_all(&mut self, targets: &HashMap<String, (Target, RefCell<Option<TargetData>>)>, link_dirs: &Vec<&str>) -> Result<(), i32> {
        let mut libs = self.missing_libs(targets);
        libs.sort();
        libs.dedup();
        match libs::find_libs(libs.clone(), link_dirs, None) {
            Ok((libs, notfound, failed)) => {
                for nf in notfound.iter() {error!("couldn't find library {nf}");}
                if !notfound.is_empty() {return Err(102)}
                libs.into_iter().for_each(|(path, name)| self.init_lib(&name, &path, targets));
                if failed {Err(99)} else {Ok(())}
            },
            Err(e) => {
                error!("{e}");
                Err(100)
            }
        }
    }
    pub fn initialized_libs<'a, 'b: 'a>(&'b self, targets: &'a HashMap<String, (Target, RefCell<Option<TargetData>>)>) -> Vec<PathBuf> {
        let mut out = self.libs.iter().filter_map(|lib| if let LibInfo::Path(p) = lib {Some(p.clone())} else {None}).collect::<Vec<_>>();
        for dep in self.deps.iter() {out.extend(targets.get(dep).expect("Dependency should exist!").1.borrow().as_ref().expect("Dependency should be initialized!").initialized_libs(targets))}
        out
    }
}
fn clear_mod(this: &mut HashMap<String, cobalt::Symbol>) {
    for (_, sym) in this.iter_mut() {
        sym.1.export = false;
        match sym {
            cobalt::Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(m, ..)), ..}, _) => clear_mod(m),
            cobalt::Symbol(v, _) => if let Some(inkwell::values::BasicValueEnum::PointerValue(pv)) = v.comp_val {
                unsafe {
                    if matches!(v.data_type, Type::Function(..)) {
                        let f = std::mem::transmute::<_, inkwell::values::FunctionValue>(pv);
                        while let Some(bb) = f.get_first_basic_block() {bb.remove_from_function().unwrap();}
                    }
                    else {
                        std::mem::transmute::<_, inkwell::values::GlobalValue>(pv).set_externally_initialized(true)
                    }
                }
            }
        }
    }
}
fn build_file_1(path: &Path, ctx: &CompCtx, opts: &BuildOptions) -> Result<(([PathBuf; 2], bool), Option<TopLevelAST>), i32> {
    let mut out_path = opts.build_dir.to_path_buf();
    out_path.push(".artifacts");
    out_path.push("objects");
    out_path.push(path.strip_prefix(opts.source_dir).unwrap_or(path));
    out_path.set_extension("o");
    let mut head_path = opts.build_dir.to_path_buf();
    head_path.push(".artifacts");
    head_path.push("headers");
    head_path.push(path.strip_prefix(opts.source_dir).unwrap_or(path));
    head_path.set_extension("coh.o");
    let pname = match path.related_to(opts.source_dir) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error occured when finding relative path: {e}");
            return Err(100)
        }
    };
    let mut fail = false;
    let mut overall_fail = false;
    if out_path.exists() && head_path.exists() && (|| Ok::<bool, std::io::Error>(path.metadata()?.modified()? < out_path.metadata()?.modified()?))().unwrap_or(false) { // lambda to propagate errors
        println!("{} has already been built", pname.display());
        use object::read::{Object, ObjectSection};
        let mut buf = Vec::new();
        let mut file = std::fs::File::open(&path).map_err(|err| {error!("{err}"); 4})?;
        file.read_to_end(&mut buf).map_err(|err| {error!("{err}"); 4})?;
        let obj = object::File::parse(buf.as_slice()).map_err(|err| {
            error!("{err}");
            4
        })?;
        if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {
            for c in ctx.load(&mut &*colib).map_err(|err| {error!("{err}"); 4})? {
                error!("conflicting definitions for {c}");
                fail = true;
            }
        }
        return if fail {Err(101)} else {Ok((([out_path, head_path], false), None))};
    }
    let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
    let config = term::Config::default();
    let name = path.to_str().expect("File name must be valid UTF-8");
    ctx.module.set_name(name);
    ctx.module.set_source_file_name(name);
    fail = false;
    let code = match std::fs::read_to_string(path.as_absolute_path().unwrap()) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("error occured when trying to open file: {e}");
            return Err(100)
        }
    };
    let files = &mut *FILES.write().unwrap();
    let file = files.add_file(0, name.to_string(), code.clone());
    let (toks, errs) = cobalt::parser::lexer::lex(&code, (file, 0), &ctx.flags);
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    if fail && !opts.continue_comp {return Err(101)}
    overall_fail |= fail;
    fail = false;
    let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    if fail && !opts.continue_comp {return Err(101)}
    ast.run_passes(ctx);
    Ok((([out_path, head_path], overall_fail), Some(ast)))
}
fn build_file_2(ast: TopLevelAST, ctx: &CompCtx, opts: &BuildOptions, (out_path, head_path): (&Path, &Path), mut overall_fail: bool) -> Result<(), i32> {
    let files = &*FILES.read().unwrap();
    let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
    let config = term::Config::default();
    let (_, errs) = ast.codegen(ctx);
    let mut fail = false;
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    overall_fail |= fail;
    if fail && !opts.continue_comp {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        return Err(101)
    }
    if let Err(msg) = ctx.module.verify() {
        error!("\n{}", msg.to_string());
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        return Err(101)
    }
    if overall_fail {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
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
    if let Err(e) = if out_path.parent().unwrap().exists() {Ok(())} else {std::fs::create_dir_all(out_path.parent().unwrap())} {
        eprintln!("error when creating directory {}: {e}", out_path.parent().unwrap().display());
        return Err(100)
    }
    if let Err(e) = if head_path.parent().unwrap().exists() {Ok(())} else {std::fs::create_dir_all(head_path.parent().unwrap())} {
        eprintln!("error when creating directory {}: {e}", head_path.parent().unwrap().display());
        return Err(100)
    }
    target_machine.write_to_file(&ctx.module, inkwell::targets::FileType::Object, out_path).unwrap();
    let mut obj = libs::new_object(opts.triple);
    libs::populate_header(&mut obj, ctx);
    (|| {
        let mut file = BufWriter::new(std::fs::File::create(head_path)?);
        file.write_all(&obj.write()?)?;
        file.flush()?;
        anyhow::Ok(())
    })().map_err(|e| {
        println!("{e:#}");
        4
    })?;
    ctx.with_vars(|v| clear_mod(&mut v.symbols));
    let mut pname = match out_path.related_to(opts.build_dir) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error occured when finding relative path: {e}");
            return Err(100)
        }
    };
    if let Ok(base) = pname.strip_prefix(Path::new(".artifacts").join("objects")).map(ToOwned::to_owned).map(Cow::Owned) {pname = base;};
    println!("Built {}", pname.display());
    Ok(())
}
fn build_target(t: &Target, data: &RefCell<Option<TargetData>>, targets: &HashMap<String, (Target, RefCell<Option<TargetData>>)>, ctx: &CompCtx, opts: &BuildOptions) -> i32 {
    let name = &t.name;
    println!("Building target {name}");
    match t.target_type {
        TargetType::Executable => {
            for (target, version) in t.deps.iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            println!("Failed to build {name} because of a failure in dependencies");
                            error!("target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {
                            println!("Failed to build {name} because of a failure in dependencies");
                            return res
                        }
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        if let Some(pkg) = package::Package::registry().get(target) {
                            match pkg.install(opts.triple.as_str().to_str().unwrap(), Some(v), package::InstallOptions::default()) {
                                Err(package::InstallError::NoInstallDirectory) => panic!("This would only be reachable if $HOME was deleted in a data race, which may or may not even be possible"),
                                Err(package::InstallError::DownloadError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 4
                                },
                                Err(package::InstallError::StdIoError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 3
                                },
                                Err(package::InstallError::GitCloneError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 2
                                },
                                Err(package::InstallError::ZipExtractError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 5
                                },
                                Err(package::InstallError::BuildFailed(e)) => {
                                    eprintln!("failed to build package {target}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return e
                                },
                                Err(package::InstallError::NoMatchesError) => {
                                    eprintln!("package {target:?} has no releases");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 7
                                },
                                Err(package::InstallError::CfgFileError(e)) => {
                                    error!("could not parse {target}'s config file: {e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 8
                                },
                                Err(package::InstallError::InvalidVersionSpec(_, v)) => {
                                    error!("could not parse {target}'s dependencies: invalid version spec {v}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 9
                                },
                                Err(package::InstallError::PkgNotFound(p)) => {
                                    error!("could not parse {target}'s dependencies: can't find package {p}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 10
                                },
                                _ => {}
                            }
                        }
                        else {
                            error!("can't find package {target}");
                            println!("Failed to build {name} because of a failure in dependencies");
                            return 10
                        }
                    }
                    else {
                        error!("unknown version specification {x:?}");
                        println!("Failed to build {name} because of a failure in dependencies");
                        return 107
                    }
                }
            }
            let mut paths = vec![];
            let mut asts = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    let mut ecode = 0;
                    for file in (match glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()) {
                        Ok(f) => f,
                        Err(e) => {
                            eprintln!("error in file glob: {e}");
                            return 108;
                        }
                    }).filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        match build_file_1(file.as_path(), ctx, opts) {
                            Ok((path, ast)) => {
                                paths.push(path);
                                asts.push(ast);
                            },
                            Err(code) => if  opts.continue_build {ecode = code} else {
                                println!("Failed to build {name} because of compile errors");
                                return code
                            }
                        }
                        passed = true;
                    }
                    if ecode != 0 {
                        println!("Failed to build {name} because of compile errors");
                        return ecode;
                    }
                    if !passed {warning!("no files matching glob {files}")}
                },
                Some(either::Right(files)) => {
                    let mut failed = false;
                    let mut ecode = 0;
                    for file in files.iter().filter_map(|f| Some(opts.source_dir.to_str()?.to_string() + "/" + f)) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {
                            error!("couldn't find file {file}");
                            if opts.continue_build {
                                failed = true;
                                continue
                            }
                            else {
                                println!("Failed to build {name} because of missing files");
                                return 120
                            }
                        }
                        match build_file_1(Path::new(&file), ctx, opts) {
                            Ok((path, ast)) => {
                                paths.push(path);
                                asts.push(ast);
                            },
                            Err(code) => if opts.continue_build {ecode = code} else {
                                println!("Failed to build {name} because of compile errors");
                                return code
                            }
                        }
                    }
                    if ecode != 0 {
                        println!("Failed to build {name} because of compile errors");
                        return ecode;
                    }
                    if failed {
                        println!("Failed to build {name} because of missing files");
                        return 120
                    }
                },
                None => {
                    error!("library target must have files");
                    println!("Failed to build {name} because no files were specified");
                    return 106;
                }
            }
            asts.iter().for_each(|ast| if let Some(ast) = ast {ast.run_passes(ctx)});
            if let Err(err) = paths.iter().zip(asts).try_for_each(|(([p1, p2], fail), ast)| if let Some(ast) = ast {build_file_2(ast, ctx, opts, (&p1, &p2), *fail)} else {Ok(())}) {return err}
            let mut output = opts.build_dir.to_path_buf();
            output.push(&t.name);
            let mut args = vec![OsString::from("-o"), output.into_os_string()];
            args.extend(paths.into_iter().map(|x| x.0[0].clone().into_os_string()));
            if let Err(e) = data.borrow_mut().as_mut().unwrap().init_all(targets, &opts.link_dirs) {return e}
            for lib in data.borrow().as_ref().unwrap().initialized_libs(targets) {
                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                args.push(OsString::from("-L"));
                args.push(parent.clone());
                args.push(OsString::from("-rpath"));
                args.push(parent);
                args.push(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
            }
            let code = Command::new("cc").args(args.iter()).status()
            .or_else(|_| Command::new("clang").args(args.iter()).status())
            .or_else(|_| Command::new("gcc").args(args.iter()).status())
            .ok().and_then(|x| x.code()).unwrap_or(-1);
            if code == 0 {println!("Built {name}");}
            else {println!("Failed to build {name} because of link errors");}
            code
        },
        TargetType::Library => {
            for (target, version) in t.deps.iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            println!("Failed to build {name} because of a failure in dependencies");
                            error!("target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {
                            println!("Failed to build {name} because of a failure in dependencies");
                            return res
                        }
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        if let Some(pkg) = package::Package::registry().get(target) {
                            match pkg.install(opts.triple.as_str().to_str().unwrap(), Some(v), package::InstallOptions::default()) {
                                Err(package::InstallError::NoInstallDirectory) => panic!("This would only be reachable if $HOME was deleted in a data race, which may or may not even be possible"),
                                Err(package::InstallError::DownloadError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 4
                                },
                                Err(package::InstallError::StdIoError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 3
                                },
                                Err(package::InstallError::GitCloneError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 2
                                },
                                Err(package::InstallError::ZipExtractError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 5
                                },
                                Err(package::InstallError::BuildFailed(e)) => {
                                    eprintln!("failed to build package {target}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return e
                                },
                                Err(package::InstallError::NoMatchesError) => {
                                    eprintln!("package {target:?} has no releases");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 7
                                },
                                Err(package::InstallError::CfgFileError(e)) => {
                                    error!("could not parse {target}'s config file: {e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 8
                                },
                                Err(package::InstallError::InvalidVersionSpec(_, v)) => {
                                    error!("could not parse {target}'s dependencies: invalid version spec {v}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 9
                                },
                                Err(package::InstallError::PkgNotFound(p)) => {
                                    error!("could not parse {target}'s dependencies: can't find package {p}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 10
                                },
                                _ => {}
                            }
                        }
                        else {
                            error!("can't find package {target}");
                            println!("Failed to build {name} because of a failure in dependencies");
                            return 10
                        }
                    }
                    else {
                        error!("unknown version specification {x:?}");
                        println!("Failed to build {name} because of a failure in dependencies");
                        return 107
                    }
                }
            }
            let mut asts = vec![];
            let mut paths = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    let mut ecode = 0;
                    for file in (match glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()) {
                        Ok(f) => f,
                        Err(e) => {
                            eprintln!("error in file glob: {e}");
                            return 108;
                        }
                    }).filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        match build_file_1(file.as_path(), ctx, opts) {
                            Ok((path, ast)) => {
                                paths.push(path);
                                asts.push(ast);
                            },
                            Err(code) => if  opts.continue_build {ecode = code} else {
                                println!("Failed to build {name} because of compile errors");
                                return code
                            }
                        }
                        passed = true;
                    }
                    if ecode != 0 {
                        println!("Failed to build {name} because of compile errors");
                        return ecode;
                    }
                    if !passed {warning!("no files matching glob {files}")}
                },
                Some(either::Right(files)) => {
                    let mut failed = false;
                    let mut ecode = 0;
                    for file in files.iter().filter_map(|f| Some(opts.source_dir.to_str()?.to_string() + "/" + f)) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {
                            error!("couldn't find file {file}");
                            if opts.continue_build {
                                failed = true;
                                continue
                            }
                            else {
                                println!("Failed to build {name} because of missing files");
                                return 120
                            }
                        }
                        match build_file_1(Path::new(&file), ctx, opts) {
                            Ok((path, ast)) => {
                                paths.push(path);
                                asts.push(ast);
                            },
                            Err(code) => if opts.continue_build {ecode = code} else {
                                println!("Failed to build {name} because of compile errors");
                                return code
                            }
                        }
                    }
                    if ecode != 0 {
                        println!("Failed to build {name} because of compile errors");
                        return ecode;
                    }
                    if failed {
                        println!("Failed to build {name} because of missing files");
                        return 120
                    }
                },
                None => {
                    error!("library target must have files");
                    println!("Failed to build {name} because no files were specified");
                    return 106;
                }
            }
            if let Err(err) = paths.iter().zip(asts).try_for_each(|(([p1, p2], fail), ast)| if let Some(ast) = ast {build_file_2(ast, ctx, opts, (&p1, &p2), *fail)} else {Ok(())}) {return err}
            let mut output = opts.build_dir.to_path_buf();
            output.push(format!("lib{}.so", t.name));
            let mut cmd = Command::new("ld");
            cmd.args(["--shared", "-o"]).arg(&output);
            cmd.args(paths.into_iter().flat_map(|(x, _)| x));
            let code = cmd.status().ok().and_then(|x| x.code()).unwrap_or(-1);
            if code != 0 {println!("Failed to build {name} because of link errors")}
            code
        },
        TargetType::Meta => {
            if t.files.is_some() {
                error!("meta target cannot have files");
                return 106;
            }
            for (target, version) in t.deps.iter() {
                match version.as_str() {
                    "project" => {
                        let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {
                            println!("Failed to build {name} because of a failure in dependencies");
                            error!("target {target:?} is not a target in this project");
                            return 105
                        };
                        if d.borrow().is_some() {continue}
                        else {*d.borrow_mut() = Some(TargetData::default())}
                        let res = build_target(t, d, targets, ctx, opts);
                        if res != 0 {
                            println!("Failed to build {name} because of a failure in dependencies");
                            return res
                        }
                        data.borrow_mut().as_mut().unwrap().deps.push(target.clone());
                    },
                    "system" => {
                        data.borrow_mut().as_mut().unwrap().libs.push(LibInfo::Name(target.clone()));
                    },
                    x => if let Ok(v) = x.parse::<VersionReq>() {
                        if let Some(pkg) = package::Package::registry().get(target) {
                            match pkg.install(opts.triple.as_str().to_str().unwrap(), Some(v), package::InstallOptions::default()) {
                                Err(package::InstallError::NoInstallDirectory) => panic!("This would only be reachable if $HOME was deleted in a data race, which may or may not even be possible"),
                                Err(package::InstallError::DownloadError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 4
                                },
                                Err(package::InstallError::StdIoError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 3
                                },
                                Err(package::InstallError::GitCloneError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 2
                                },
                                Err(package::InstallError::ZipExtractError(e)) => {
                                    error!("{e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 5
                                },
                                Err(package::InstallError::BuildFailed(e)) => {
                                    eprintln!("failed to build package {target}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return e
                                },
                                Err(package::InstallError::NoMatchesError) => {
                                    eprintln!("package {target:?} has no releases");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 7
                                },
                                Err(package::InstallError::CfgFileError(e)) => {
                                    error!("could not parse {target}'s config file: {e}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 8
                                },
                                Err(package::InstallError::InvalidVersionSpec(_, v)) => {
                                    error!("could not parse {target}'s dependencies: invalid version spec {v}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 9
                                },
                                Err(package::InstallError::PkgNotFound(p)) => {
                                    error!("could not parse {target}'s dependencies: can't find package {p}");
                                    println!("Failed to build {name} because of a failure in dependencies");
                                    return 10
                                },
                                _ => {}
                            }
                        }
                        else {
                            error!("can't find package {target}");
                            println!("Failed to build {name} because of a failure in dependencies");
                            return 10
                        }
                    }
                    else {
                        error!("unknown version specification {x:?}");
                        println!("Failed to build {name} because of a failure in dependencies");
                        return 107
                    }
                }
            }
            println!("Built {name}");
            0
        }
    }
}
pub fn build(pkg: Project, to_build: Option<Vec<String>>, opts: &BuildOptions) -> i32 {
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            error!(r#"project has Cobalt version requirement "{v}", but Cobalt version is {}"#, env!("CARGO_PKG_VERSION"));
            return 104;
        }
    }
    let targets = pkg.into_targets().map(|t| (t.name.clone(), (t, RefCell::new(None)))).collect::<HashMap<String, (Target, RefCell<Option<TargetData>>)>>();
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&target_name) {t} else {
                error!("target {target_name:?} is not a target in this project");
                return 105;
            };
            if data.borrow().is_some() {continue}
            else {*data.borrow_mut() = Some(TargetData::default())}
            let res = build_target(target, data, &targets, &ctx, opts);
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
