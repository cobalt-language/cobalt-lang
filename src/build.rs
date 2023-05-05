use codespan_reporting::term::{self, termcolor::{ColorChoice, StandardStream}};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::ffi::OsString;
use std::io::{Read, Write, BufWriter};
use std::process::{Command, exit};
use std::borrow::Cow;
use serde::*;
use either::Either;
use semver::{Version, VersionReq};
use path_calculate::*;
use anyhow::Context;
use anyhow_std::*;
use cobalt::{CompCtx, Value, Type, InterData, AST, ast::TopLevelAST, errors::FILES};
use cobalt::misc::CellExt as Cell;
use super::{libs, opt, warning, error, CompileErrors};
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
fn build_file_1(path: &Path, ctx: &CompCtx, opts: &BuildOptions) -> anyhow::Result<(([PathBuf; 2], bool), Option<TopLevelAST>)> {
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
    let pname = path.related_to(opts.source_dir)?;
    if out_path.exists() && head_path.exists() && (|| Ok::<bool, std::io::Error>(path.metadata()?.modified()? < out_path.metadata()?.modified()?))().unwrap_or(false) { // lambda to propagate errors
        println!("{} has already been built", pname.display());
        use object::read::{Object, ObjectSection};
        let mut buf = Vec::new();
        let mut file = std::fs::File::open(&head_path)?;
        file.read_to_end(&mut buf)?;
        let obj = object::File::parse(buf.as_slice())?;
        if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {
            let vec = ctx.load(&mut &*colib)?;
            if !vec.is_empty() {anyhow::bail!(libs::ConflictingDefs(vec));}
        }
        return Ok((([out_path, head_path], false), None));
    }
    let mut fail = false;
    let mut overall_fail = false;
    let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
    let config = term::Config::default();
    let name = path.to_str().expect("File name must be valid UTF-8");
    ctx.module.set_name(name);
    ctx.module.set_source_file_name(name);
    let code = path.as_absolute_path().unwrap().read_to_string_anyhow()?;
    let files = &mut *FILES.write().unwrap();
    let file = files.add_file(0, name.to_string(), code.clone());
    let (toks, errs) = cobalt::parser::lexer::lex(&code, (file, 0), &ctx.flags);
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    if fail && !opts.continue_comp {anyhow::bail!(CompileErrors)}
    overall_fail |= fail;
    fail = false;
    let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    if fail && !opts.continue_comp {anyhow::bail!(CompileErrors)}
    ast.run_passes(ctx);
    Ok((([out_path, head_path], overall_fail), Some(ast)))
}
fn build_file_2(ast: TopLevelAST, ctx: &CompCtx, opts: &BuildOptions, (out_path, head_path): (&Path, &Path), mut overall_fail: bool) -> anyhow::Result<()> {
    let files = &*FILES.read().unwrap();
    let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
    let config = term::Config::default();
    let (_, errs) = ast.codegen(ctx);
    let mut fail = false;
    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
    overall_fail |= fail;
    if fail && !opts.continue_comp {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        anyhow::bail!(CompileErrors)
    }
    if let Err(msg) = ctx.module.verify() {
        error!("\n{}", msg.to_string());
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        anyhow::bail!(CompileErrors)
    }
    if overall_fail {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        anyhow::bail!(CompileErrors)
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
    if !out_path.parent().unwrap().exists() {out_path.parent().unwrap().create_dir_all_anyhow()?}
    if !head_path.parent().unwrap().exists() {head_path.parent().unwrap().create_dir_all_anyhow()?}
    target_machine.write_to_file(&ctx.module, inkwell::targets::FileType::Object, out_path).unwrap();
    let mut obj = libs::new_object(opts.triple);
    libs::populate_header(&mut obj, ctx);
    let mut file = BufWriter::new(std::fs::File::create(head_path)?);
    file.write_all(&obj.write()?)?;
    file.flush()?;
    ctx.with_vars(|v| clear_mod(&mut v.symbols));
    let mut pname = out_path.related_to(opts.build_dir)?;
    if let Ok(base) = pname.strip_prefix(Path::new(".artifacts").join("objects")).map(ToOwned::to_owned).map(Cow::Owned) {pname = base;};
    println!("Built {}", pname.display());
    Ok(())
}
#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum BuiltState {
    #[default]
    NotBuilt,
    BuiltMeta,
    Built(PathBuf)
}
impl BuiltState {
    pub fn is_built(&self) -> bool {*self != Self::NotBuilt}
}
fn resolve_deps(ctx: &CompCtx, t: &Target, targets: &HashMap<String, (Target, Cell<BuiltState>)>, opts: &BuildOptions) -> anyhow::Result<Vec<PathBuf>> {
    let mut libs = vec![];
    let mut conflicts = vec![];
    for (target, version) in t.deps.iter() {
        match version.as_str() {
            "project" => {
                let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {anyhow::bail!("target {target:?} is not a target in this project")};
                match d.clone().into_inner() {
                    BuiltState::BuiltMeta => continue,
                    BuiltState::Built(artifact) => {
                        use object::{Object, ObjectSection};
                        let buf = artifact.read_anyhow()?;
                        let obj = object::File::parse(buf.as_slice())?;
                        if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {conflicts.append(&mut ctx.load(&mut &*colib)?);}
                        continue
                    },
                    BuiltState::NotBuilt => {}
                }
                if let Some(artifact) = build_target(t, d, targets, opts)? {
                    use object::{Object, ObjectSection};
                    let buf = artifact.read_anyhow()?;
                    let obj = object::File::parse(buf.as_slice())?;
                    if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {
                        conflicts.append(&mut ctx.load(&mut &*colib)?);
                    }
                }
            },
            "system" => libs.push(target.clone()),
            x => {
                let v = x.parse::<VersionReq>().context("could not parse version requirement")?;
                std::mem::drop(v);
                anyhow::bail!("packages are not currently supported!");
            }
        }
    }
    if !conflicts.is_empty() {anyhow::bail!(libs::ConflictingDefs(conflicts))}
    let (libs, notfound) =  libs::find_libs(libs, &opts.link_dirs, Some(ctx))?;
    for lib in notfound {anyhow::bail!("couldn't find {lib}")}
    Ok(libs.into_iter().map(|(x, _)| x).collect())
}
fn build_target(t: &Target, data: &Cell<BuiltState>, targets: &HashMap<String, (Target, Cell<BuiltState>)>, opts: &BuildOptions) -> anyhow::Result<Option<PathBuf>> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let name = &t.name;
    println!("Building target {name}");
    let libs = resolve_deps(&ctx, t, targets, opts)?;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    for file in glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()).context("error in file glob")?.filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        let (path, ast) = build_file_1(file.as_path(), &ctx, opts)?;
                        paths.push(path);
                        asts.push(ast);
                        passed = true;
                    }
                    if !passed {warning!("no files matching glob {files}")}
                },
                Some(either::Right(files)) => {
                    let mut failed = None;
                    for file in files.iter().filter_map(|f| Some(opts.source_dir.to_str()?.to_string() + "/" + f)) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {
                            if opts.continue_build {
                                error!("couldn't find file {file}");
                                failed = Some(file);
                                continue
                            }
                            else {anyhow::bail!("couldn't find file {file}")}
                        }
                        let (path, ast) = build_file_1(Path::new(&file), &ctx, opts)?;
                        paths.push(path);
                        asts.push(ast);
                    }
                    if let Some(file) = failed {
                        anyhow::bail!("couldn't find file {file}")
                    }
                },
                None => anyhow::bail!("target must have files")
            }
            paths.iter().zip(asts).try_for_each(|(([p1, p2], fail), ast)| if let Some(ast) = ast {build_file_2(ast, &ctx, opts, (&p1, &p2), *fail)} else {Ok(())})?;
            let mut output = opts.build_dir.to_path_buf();
            output.push(&t.name);
            let mut args = vec![OsString::from("-o"), output.clone().into_os_string()];
            args.extend(paths.into_iter().map(|x| x.0[0].clone().into_os_string()));
            for lib in libs {
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
            else {anyhow::bail!("C compiler exited with code {code}");}
            data.set(BuiltState::Built(output.clone()));
            Ok(Some(output))
        },
        TargetType::Library => {
            let libs = resolve_deps(&ctx, t, targets, opts)?;
            let mut asts = vec![];
            let mut paths = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    for file in glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()).context("error in file glob")?.filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        let (path, ast) = build_file_1(file.as_path(), &ctx, opts)?;
                        paths.push(path);
                        asts.push(ast);
                        passed = true;
                    }
                    if !passed {warning!("no files matching glob {files}")}
                },
                Some(either::Right(files)) => {
                    let mut failed = None;
                    for file in files.iter().filter_map(|f| Some(opts.source_dir.to_str()?.to_string() + "/" + f)) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {
                            if opts.continue_build {
                                error!("couldn't find file {file}");
                                failed = Some(file);
                                continue
                            }
                            else {anyhow::bail!("couldn't find file {file}")}
                        }
                        let (path, ast) = build_file_1(Path::new(&file), &ctx, opts)?;
                        paths.push(path);
                        asts.push(ast);
                    }
                    if let Some(file) = failed {
                        anyhow::bail!("couldn't find file {file}")
                    }
                },
                None => anyhow::bail!("target must have files")
            }
            paths.iter().zip(asts).try_for_each(|(([p1, p2], fail), ast)| if let Some(ast) = ast {build_file_2(ast, &ctx, opts, (&p1, &p2), *fail)} else {Ok(())})?;
            let mut output = opts.build_dir.to_path_buf();
            output.push(libs::format_lib(&t.name, &opts.triple));
            let mut cmd = Command::new("ld");
            cmd.args(["--shared", "-o"]).arg(&output);
            cmd.args(paths.into_iter().flat_map(|(x, _)| x));
            for lib in libs {
                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                cmd.arg(OsString::from("-L"));
                cmd.arg(parent.clone());
                cmd.arg(OsString::from("-rpath"));
                cmd.arg(parent);
                cmd.arg(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
            }
            let code = cmd.status().ok().and_then(|x| x.code()).unwrap_or(-1);
            if code != 0 {anyhow::bail!("ld exited with code {code}")}
            data.set(BuiltState::Built(output.clone()));
            Ok(Some(output))
        },
        TargetType::Meta => {
            if t.files.is_some() {
                anyhow::bail!("meta target cannot have files");
            }
            let _ = resolve_deps(&ctx, t, targets, opts)?;
            println!("Built {name}");
            data.set(BuiltState::BuiltMeta);
            Ok(None)
        }
    }
}
pub fn build(pkg: Project, to_build: Option<Vec<String>>, opts: &BuildOptions) -> anyhow::Result<()> {
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            anyhow::bail!(r#"project has Cobalt version requirement "{v}", but Cobalt version is {}"#, env!("CARGO_PKG_VERSION"));
        }
    }
    let targets = pkg.into_targets().map(|t| (t.name.clone(), (t, Cell::default()))).collect::<HashMap<_, (Target, Cell<BuiltState>)>>();
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&target_name) {t} else {
                anyhow::bail!("target {target_name:?} is not a target in this project");
            };
            if data.with(|x| x.is_built()) {continue}
            build_target(target, data, &targets, opts)?;
        }
    }
    else {
        for (_, (target, data)) in targets.iter() {
            if data.with(|x| x.is_built()) {continue}
            build_target(target, data, &targets, opts)?;
        }
    }
    Ok(())
}
