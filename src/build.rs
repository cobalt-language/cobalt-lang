use codespan_reporting::term::{self, termcolor::{ColorChoice, StandardStream}};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::io::{Read, Write, BufWriter};
use std::borrow::Cow;
use serde::*;
use either::Either;
use semver::{Version, VersionReq};
use path_calculate::*;
use anyhow::Context;
use anyhow_std::*;
use cobalt::{CompCtx, Value, Type, InterData, AST, ast::TopLevelAST, errors::FILES};
use cobalt::misc::CellExt as Cell;
use super::*;
#[derive(Debug, Clone)]
pub struct Project {
    pub name: String,
    pub version: Version,
    pub author: Option<String>,
    pub co_version: Option<VersionReq>,
    pub desc: Option<String>,
    pub targets: HashMap<String, Target>
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
#[derive(Debug, Clone)]
pub struct Target {
    pub target_type: TargetType,
    pub files: Option<Either<String, Vec<String>>>,
    pub deps: HashMap<String, Dependency>
}
#[derive(Deserialize)]
#[serde(rename = "target")]
struct TargetShim {
    name: String,
    #[serde(rename = "type")]
    target_type: TargetType,
    #[serde(with = "either::serde_untagged_optional")]
    files: Option<Either<String, Vec<String>>>,
    #[serde(default)]
    deps: HashMap<String, Dependency>
}
#[derive(Deserialize)]
#[serde(rename = "target")]
struct KnownTargetShim {
    name: String,
    #[serde(with = "either::serde_untagged_optional")]
    files: Option<Either<String, Vec<String>>>,
    #[serde(default)]
    deps: HashMap<String, Dependency>
}
impl<'de> Deserialize<'de> for Project {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::*;
        use std::fmt::{self, Formatter};
        struct ProjectVisitor;
        impl<'de> Visitor<'de> for ProjectVisitor {
            type Value = Project;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {f.write_str("struct Project")}
            fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<Project, V::Error> {
                let mut name = None;
                let mut version = None;
                let mut author = None;
                let mut co_version = None;
                let mut desc = None;
                let mut targets = HashMap::new();
                enum Field {Name, Version, Author, CoVersion, Desc, Targets, Bin, Lib, Meta, Ignore}
                impl<'de> Deserialize<'de> for Field {
                    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Field, D::Error> {
                        struct FieldVisitor;
                        impl<'de> Visitor<'de> for FieldVisitor {
                            type Value = Field;
                            fn expecting(&self, f: &mut Formatter<'_>) -> fmt::Result {f.write_str("field identifier")}
                            fn visit_str<E: Error>(self, v: &str) -> Result<Field, E> {
                                Ok(match v {
                                    "name" => Field::Name,
                                    "version" => Field::Version,
                                    "author" => Field::Author,
                                    "co_version" | "co-version" | "cobalt" => Field::CoVersion,
                                    "desc" | "description" => Field::Desc,
                                    "target" | "targets" => Field::Targets,
                                    "bin" | "binary" | "exe" | "executable" => Field::Bin,
                                    "lib" | "library" => Field::Lib,
                                    "meta" => Field::Meta,
                                    _ => Field::Ignore
                                })
                            }
                        }
                        deserializer.deserialize_identifier(FieldVisitor)
                    }
                }
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Name => if name.is_some() {return Err(Error::duplicate_field("name"))} else {name = Some(map.next_value()?)}
                        Field::Version => if version.is_some() {return Err(Error::duplicate_field("version"))} else {version = Some(map.next_value()?)}
                        Field::Author => if author.is_some() {return Err(Error::duplicate_field("author"))} else {author = Some(map.next_value()?)}
                        Field::CoVersion => if co_version.is_some() {return Err(Error::duplicate_field("co_version"))} else {co_version = Some(map.next_value()?)}
                        Field::Desc => if desc.is_some() {return Err(Error::duplicate_field("desc"))} else {desc = Some(map.next_value()?)}
                        Field::Targets => targets.extend(&mut map.next_value::<Vec<TargetShim>>()?.into_iter().map(|TargetShim {name, target_type, deps, files}| (name, Target {target_type, deps, files}))),
                        Field::Bin => targets.extend(&mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(|KnownTargetShim {name, deps, files}| (name, Target {target_type: TargetType::Executable, deps, files}))),
                        Field::Lib => targets.extend(&mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(|KnownTargetShim {name, deps, files}| (name, Target {target_type: TargetType::Library, deps, files}))),
                        Field::Meta => targets.extend(&mut map.next_value::<Vec<KnownTargetShim>>()?.into_iter().map(|KnownTargetShim {name, deps, files}| (name, Target {target_type: TargetType::Meta, deps, files}))),
                        Field::Ignore => {}
                    }
                }
                let name = name.ok_or_else(|| Error::missing_field("name"))?;
                let version = version.ok_or_else(|| Error::missing_field("version"))?;
                Ok(Project {name, version, author, co_version, desc, targets})
            }
        }
        deserializer.deserialize_struct("Project", &["name", "version", "author", "co_version", "desc", "targets"], ProjectVisitor)
    }
}
#[derive(Debug, Clone)]
pub struct BuildOptions<'a> {
    pub source_dir: &'a Path,
    pub build_dir: &'a Path,
    pub continue_build: bool,
    pub continue_comp: bool,
    pub rebuild: bool,
    pub triple: &'a inkwell::targets::TargetTriple,
    pub profile: &'a str,
    pub link_dirs: Vec<&'a str>
}
#[derive(Debug, Clone, Default, Deserialize)]
pub struct PkgDepSpec {
    #[serde(default)]
    pub version: semver::VersionReq,
    pub targets: Option<Vec<String>>
}
#[derive(Debug, Clone)]
pub enum Dependency {
    Project,
    System,
    Package(PkgDepSpec)
}
impl<'de> Deserialize<'de> for Dependency {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use std::fmt::{self, Formatter};
        use serde::de::*;
        struct DepVisitor;
        impl<'de> Visitor<'de> for DepVisitor {
            type Value = Dependency;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {f.write_str(r#""project", "version", or a version statement"#)}
            fn visit_str<E: Error>(self, v: &str) -> Result<Dependency, E> {
                Ok(match v.trim() {
                    "project" => Dependency::Project,
                    "system" => Dependency::System,
                    x => Dependency::Package({
                        let mut it = v.match_indices(['@', ':']).peekable();
                        if let Some(&(next, _)) = it.peek() {
                            let mut targets: Option<Vec<String>> = None;
                            let mut version = VersionReq::parse(&v[..next]).map_err(Error::custom)?;
                            while let Some((idx, ch)) = it.next() {
                                let blk = if let Some(&(next, _)) = it.peek() {v[idx..next].trim()} else {v[idx..].trim()};
                                match ch {
                                    "@" => version.comparators.extend(VersionReq::parse(blk).map_err(Error::custom)?.comparators),
                                    ":" => targets.get_or_insert_with(Vec::new).extend(blk.split(',').map(str::trim).map(String::from)),
                                    x => unreachable!("expected '@' or ':', got {x:?}")
                                }
                            }
                            PkgDepSpec {targets, version}
                        }
                        else {PkgDepSpec {targets: None, version: VersionReq::parse(x).map_err(Error::custom)?}}
                    })
                })
            }
            fn visit_map<M: MapAccess<'de>>(self, mut v: M) -> Result<Dependency, M::Error> {
                enum Field {Version, Targets}
                impl<'de> Deserialize<'de> for Field {
                    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Field, D::Error> {
                        struct FieldVisitor;
                        impl<'de> Visitor<'de> for FieldVisitor {
                            type Value = Field;
                            fn expecting(&self, f: &mut Formatter) -> fmt::Result {f.write_str("`version` or `targets`")}
                            fn visit_str<E: Error>(self, v: &str) -> Result<Field, E> {
                                match v {
                                    "version" => Ok(Field::Version),
                                    "targets" => Ok(Field::Targets),
                                     _ => Err(Error::unknown_field(v, &["version", "targets"]))
                                }
                            }
                        }
                        deserializer.deserialize_identifier(FieldVisitor)
                    }
                }
                let mut targets: Option<Vec<String>> = None;
                let mut version: Option<VersionReq> = None;
                while let Some(key) = v.next_key()? {
                    match key {
                        Field::Version => if version.is_some() {return Err(Error::duplicate_field("version"))} else {version = Some(v.next_value()?)}
                        Field::Targets => if targets.is_some() {return Err(Error::duplicate_field("targets"))} else {targets = Some(v.next_value()?)}
                    }
                }
                let version = version.ok_or_else(|| Error::missing_field("version"))?;
                Ok(Dependency::Package(PkgDepSpec {targets, version}))
            }
        }
        deserializer.deserialize_any(DepVisitor)
    }
}
fn clear_mod(this: &mut HashMap<String, cobalt::Symbol>) {
    for (_, sym) in this.iter_mut() {
        sym.1.export = false;
        match sym {
            cobalt::Symbol(Value {data_type: Type::Module, inter_val: Some(InterData::Module(m, ..)), ..}, _) => clear_mod(m),
            cobalt::Symbol(v, d) => if let Some(inkwell::values::BasicValueEnum::PointerValue(pv)) = v.comp_val {
                if !d.fwd && d.init {
                    unsafe {
                        if matches!(v.data_type, Type::Function(..)) {
                            let f = std::mem::transmute::<_, inkwell::values::FunctionValue>(pv);
                            f.set_linkage(inkwell::module::Linkage::AvailableExternally);
                        }
                        else {
                            std::mem::transmute::<_, inkwell::values::GlobalValue>(pv).set_externally_initialized(true)
                        }
                    }
                }
            }
        }
    }
}
fn build_file_1(path: &Path, ctx: &CompCtx, opts: &BuildOptions, force_build: bool) -> anyhow::Result<(([PathBuf; 2], bool), Option<TopLevelAST>)> {
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
    if !(force_build || opts.rebuild) && out_path.exists() && head_path.exists() && (|| Ok::<bool, std::io::Error>(path.metadata()?.modified()? < out_path.metadata()?.modified()?))().unwrap_or(false) { // lambda to propagate errors
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
fn resolve_deps(ctx: &CompCtx, t: &Target, targets: &HashMap<String, (Target, Cell<BuiltState>)>, opts: &BuildOptions) -> anyhow::Result<(bool, Vec<PathBuf>)> {
    let mut out = vec![];
    let mut libs = vec![];
    let mut conflicts = vec![];
    let mut changed = false;
    for (target, version) in t.deps.iter() {
        match version {
            Dependency::Project => {
                let (t, d) = if let Some(t) = targets.get(target.as_str()) {t} else {anyhow::bail!("target {target:?} is not a target in this project")};
                match d.clone().into_inner() {
                    BuiltState::BuiltMeta => continue,
                    BuiltState::Built(artifact) => {
                        use object::{Object, ObjectSection};
                        let buf = artifact.read_anyhow()?;
                        let obj = object::File::parse(buf.as_slice())?;
                        if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {conflicts.append(&mut ctx.load(&mut &*colib)?)}
                        out.push(artifact);
                        continue
                    },
                    BuiltState::NotBuilt => {}
                }
                let (c, a) = build_target(t, &target, d, targets, opts)?;
                changed |= c;
                if let Some(artifact) = a {
                    use object::{Object, ObjectSection};
                    let buf = artifact.read_anyhow()?;
                    let obj = object::File::parse(buf.as_slice())?;
                    if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {conflicts.append(&mut ctx.load(&mut &*colib)?)}
                    out.push(artifact);
                }
            },
            Dependency::System => libs.push(target.clone()),
            Dependency::Package(_) => anyhow::bail!("packages are not currently supported!")
        }
    }
    if !conflicts.is_empty() {anyhow::bail!(libs::ConflictingDefs(conflicts))}
    let (libs, notfound) =  libs::find_libs(libs, &opts.link_dirs, Some(ctx))?;
    for lib in notfound {anyhow::bail!("couldn't find {lib}")}
    out.extend(libs.into_iter().map(|(x, _)| x));
    Ok((changed, out))
}
fn build_target(t: &Target, name: &str, data: &Cell<BuiltState>, targets: &HashMap<String, (Target, Cell<BuiltState>)>, opts: &BuildOptions) -> anyhow::Result<(bool, Option<PathBuf>)> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let (rebuild, libs) = resolve_deps(&ctx, t, targets, opts)?;
    let mut changed = rebuild;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    for file in glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()).context("error in file glob")?.filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        let (path, ast) = build_file_1(file.as_path(), &ctx, opts, rebuild)?;
                        changed |= ast.is_some();
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
                        let (path, ast) = build_file_1(Path::new(&file), &ctx, opts, rebuild)?;
                        changed |= ast.is_some();
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
            output.push(name);
            let mut cc = cc::CompileCommand::new();
            cc.objs(paths.into_iter().map(|([x, _], _)| x));
            cc.output(&output);
            cc.link_libs(libs);
            let code = cc.build()?.status_anyhow()?.code().unwrap_or(-1);
            if code != 0 {anyhow::bail!("C compiler exited with code {code}")}
            data.set(BuiltState::Built(output.clone()));
            Ok((changed, Some(output)))
        },
        TargetType::Library => {
            let mut asts = vec![];
            let mut paths = vec![];
            match t.files.as_ref() {
                Some(either::Left(files)) => {
                    let mut passed = false;
                    for file in glob::glob((opts.source_dir.to_str().unwrap_or("").to_string() + "/" + files).as_str()).context("error in file glob")?.filter_map(Result::ok) {
                        if std::fs::metadata(&file).map(|m| !m.file_type().is_file()).unwrap_or(true) {continue}
                        let (path, ast) = build_file_1(file.as_path(), &ctx, opts, rebuild)?;
                        changed |= ast.is_some();
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
                        let (path, ast) = build_file_1(Path::new(&file), &ctx, opts, rebuild)?;
                        changed |= ast.is_some();
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
            output.push(libs::format_lib(name, &opts.triple));
            let mut cc = cc::CompileCommand::new();
            cc.lib(true);
            cc.objs(paths.into_iter().flat_map(|x| x.0));
            cc.output(&output);
            cc.link_libs(libs);
            let code = cc.build()?.status_anyhow()?.code().unwrap_or(-1);
            if code != 0 {anyhow::bail!("C compiler exited with code {code}")}
            data.set(BuiltState::Built(output.clone()));
            Ok((changed, Some(output)))
        },
        TargetType::Meta => {
            if t.files.is_some() {
                anyhow::bail!("meta target cannot have files");
            }
            data.set(BuiltState::BuiltMeta);
            Ok((changed, None))
        }
    }
}
pub fn build(pkg: Project, to_build: Option<Vec<String>>, opts: &BuildOptions) -> anyhow::Result<()> {
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            anyhow::bail!(r#"project has Cobalt version requirement "{v}", but Cobalt version is {}"#, env!("CARGO_PKG_VERSION"));
        }
    }
    let targets = pkg.targets.into_iter().map(|(n, t)| (n, (t, Cell::default()))).collect::<HashMap<_, (Target, Cell<BuiltState>)>>();
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&target_name) {t} else {
                anyhow::bail!("target {target_name:?} is not a target in this project");
            };
            if data.with(|x| x.is_built()) {continue}
            build_target(target, &target_name, data, &targets, opts)?;
        }
    }
    else {
        for (name, (target, data)) in targets.iter() {
            if data.with(|x| x.is_built()) {continue}
            build_target(target, name, data, &targets, opts)?;
        }
    }
    Ok(())
}
