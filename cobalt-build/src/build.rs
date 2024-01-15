use crate::*;
use anyhow_std::*;
use cobalt_ast::ast::*;
use cobalt_errors::*;
use cobalt_parser::parse_str;
use cobalt_utils::CellExt as Cell;
use either::Either;
use indexmap::IndexMap;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{FileType, Target as InkwellTarget, TargetTriple};
use os_str_bytes::OsStrBytes;
use path_calculate::*;
use semver::{Version, VersionReq};
use serde::*;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{BufWriter, Write};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

pub mod file_list;
pub mod project;
pub use file_list::*;
pub use project::*;
pub(crate) mod cond;

#[derive(Debug, Clone, Copy, Error)]
#[error("source_dir and build_dir need to be set on a project before it is built!")]
pub struct MissingDirOptions;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildOptions<'a> {
    pub continue_build: bool,
    pub continue_comp: bool,
    pub rebuild: bool,
    pub no_default_link: bool,
    pub triple: Cow<'a, str>,
    pub profile: Cow<'a, str>,
    pub link_dirs: Vec<Cow<'a, Path>>,
}

pub fn clear_mod(this: &mut HashMap<std::borrow::Cow<str>, Symbol>) {
    for sym in this.values_mut() {
        sym.1.export = false;
        if let Some((m, ..)) = sym.0.as_mod_mut() {
            clear_mod(m);
        } else {
            let Symbol(v, d) = sym;
            if let Some(inkwell::values::BasicValueEnum::PointerValue(pv)) = v.comp_val {
                if !d.fwd && d.init {
                    unsafe {
                        std::mem::transmute::<_, inkwell::values::GlobalValue>(pv)
                            .set_linkage(inkwell::module::Linkage::AvailableExternally);
                    }
                }
            }
        }
    }
}
/// Start building a file
/// This stops after running the prepasses to allow them to be run on all files before continuing
fn build_file_1(
    path: &Path,
    ctx: &CompCtx<'static, '_>,
    opts: &BuildOptions,
    force_build: bool,
    ec: &mut usize,
    source_dir: &Path,
    build_dir: &Path,
) -> anyhow::Result<(([PathBuf; 2], bool), Option<TopLevelAST<'static>>)> {
    let mut out_path = build_dir.to_path_buf();
    out_path.push(".artifacts");
    out_path.push("objects");
    out_path.push(path.strip_prefix(source_dir).unwrap_or(path));
    out_path.set_extension("o");
    let mut head_path = build_dir.to_path_buf();
    head_path.push(".artifacts");
    head_path.push("headers");
    head_path.push(path.strip_prefix(source_dir).unwrap_or(path));
    head_path.set_extension("coh.o");
    if !(force_build || opts.rebuild)
        && out_path.exists()
        && head_path.exists()
        && (|| {
            Ok::<bool, std::io::Error>(
                path.metadata()?.modified()? < out_path.metadata()?.modified()?,
            )
        })()
        .unwrap_or(false)
    {
        // lambda to propagate errors
        let conflicts = libs::load_lib(&head_path, ctx)?;
        if !conflicts.is_empty() {
            anyhow::bail!(libs::ConflictingDefs(conflicts))
        }
        return Ok((([out_path, head_path], false), None));
    }
    let mut fail = false;
    let name = path.to_str().expect("File name must be valid UTF-8");
    ctx.module.set_name(name);
    ctx.module.set_source_file_name(name);
    let code = path.as_absolute_path().unwrap().read_to_string_anyhow()?;
    let file = FILES.add_file(0, name.to_string(), code.into());
    let (ast, errs) = parse_str(file.contents());
    let mut ast = ast.unwrap_or_default();
    for err in errs {
        let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
        if is_err {
            *ec += 1;
            fail = true;
        }
        eprintln!("{:?}", Report::from(err).with_source_code(file));
    }
    ast.file = Some(file);
    if fail && !opts.continue_comp {
        anyhow::bail!(CompileErrors(*ec))
    }
    ast.run_passes(ctx);
    Ok((([out_path, head_path], fail), Some(ast)))
}
/// Finish building the file
/// This picks up where `build_file_1` left off
fn build_file_2(
    ast: TopLevelAST<'static>,
    ctx: &CompCtx<'static, '_>,
    opts: &BuildOptions,
    (out_path, head_path): (&Path, &Path),
    mut fail: bool,
    ec: &mut usize,
) -> anyhow::Result<bool> {
    let mut errs = vec![];
    ast.codegen(ctx, &mut errs);
    for err in errs {
        *ec += 1;
        fail = true;
        eprintln!(
            "{:?}",
            Report::from(err).with_source_code(ast.file.unwrap())
        );
    }
    if fail && !opts.continue_comp {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        anyhow::bail!(CompileErrors(*ec))
    }
    ctx.module.verify().map_err(|m| {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        LlvmVerifierError::from(m)
    })?;
    if fail {
        ctx.with_vars(|v| clear_mod(&mut v.symbols));
        return Ok(false);
    }
    let trip = TargetTriple::create(&opts.triple);
    let target_machine = InkwellTarget::from_triple(&trip)
        .unwrap()
        .create_target_machine(
            &trip,
            "",
            "",
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Small,
        )
        .expect("failed to create target machine");
    ctx.module
        .run_passes(
            &opt::expand_pass_string(&opts.profile)?,
            &target_machine,
            PassBuilderOptions::create(),
        )
        .map_err(opt::PassError::from_llvm)?;
    if !out_path.parent().unwrap().exists() {
        out_path.parent().unwrap().create_dir_all_anyhow()?
    }
    if !head_path.parent().unwrap().exists() {
        head_path.parent().unwrap().create_dir_all_anyhow()?
    }
    target_machine
        .write_to_file(&ctx.module, FileType::Object, out_path)
        .unwrap();
    let mut obj = libs::new_object(&opts.triple);
    libs::populate_header(&mut obj, ctx);
    let mut file = BufWriter::new(std::fs::File::create(head_path)?);
    file.write_all(&obj.write()?)?;
    file.flush()?;
    ctx.with_vars(|v| clear_mod(&mut v.symbols));
    Ok(true)
}
/// Resolve dependencies for `build_target_single`
/// This replaces project dependencies with packages, checks `plan` for installation versions, and
/// assumes that all dependencies are already built
fn resolve_deps_internal(
    ctx: &CompCtx,
    cmd: &mut cc::CompileCommand,
    t: &Target,
    pkg: &str,
    v: &Version,
    plan: &IndexMap<(&str, &str), Version>,
) -> anyhow::Result<()> {
    let mut libs = vec![];
    let mut conflicts = vec![];
    let mut installed_path = cobalt_dir()?;
    installed_path.push("installed");
    for (target, version) in t.deps.iter() {
        match version {
            Dependency::Project => {
                let mut artifact = installed_path.clone();
                artifact.push(pkg);
                artifact.push(v.to_string());
                artifact.push(&**target);
                conflicts.append(&mut libs::load_lib(&artifact, ctx)?);
                cmd.link_abs(artifact);
            }
            Dependency::System => libs.push(target.clone()),
            Dependency::Package(spec) => spec
                .targets
                .clone()
                .unwrap_or_else(|| vec!["default".into()])
                .into_iter()
                .try_for_each(|tar| {
                    let mut path = installed_path.clone();
                    path.push(&**target);
                    path.push(plan[&(&**target, &*tar)].to_string());
                    path.push(&*tar);
                    conflicts.append(&mut libs::load_lib(&path, ctx)?);
                    cmd.link_abs(path);
                    anyhow::Ok(())
                })?,
        }
    }
    if !conflicts.is_empty() {
        anyhow::bail!(libs::ConflictingDefs(conflicts))
    }
    let notfound = cmd
        .search_libs(libs, Some(ctx), false)?
        .into_iter()
        .map(From::from)
        .collect::<Vec<_>>();
    if !notfound.is_empty() {
        anyhow::bail!(LibsNotFound(notfound))
    }
    Ok(())
}
/// Build a single target, for use in package installation
pub fn build_target_single(
    t: &Target,
    pkg: &str,
    name: &str,
    v: &Version,
    plan: &IndexMap<(&str, &str), Version>,
    opts: &BuildOptions,
    source_dir: &Path,
    build_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let mut cc = cc::CompileCommand::new();
    cc.link_dir("$ORIGIN");
    cc.link_dirs(opts.link_dirs.iter().cloned());
    cc.no_default_link = opts.no_default_link;
    resolve_deps_internal(&ctx, &mut cc, t, pkg, v, plan)?;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        anyhow::bail!("couldn't find file {}", file.display());
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            cc.objs(paths.into_iter().map(|([x, _], _)| x));
            cc.output(&output);
            cc.run()?;
            Ok(output)
        }
        TargetType::Library => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        anyhow::bail!("couldn't find file {}", file.display());
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            cc.lib(true);
            cc.objs(paths.into_iter().flat_map(|x| x.0));
            cc.output(&output);
            cc.run()?;
            Ok(output)
        }
        TargetType::Archive => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        anyhow::bail!("couldn't find file {}", file.display());
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                paths.push(path);
                asts.push(ast);
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            let mut builder = ar::Builder::new(output.create_file_anyhow()?);
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                    builder.append_path(p2)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            Ok(output)
        }
        TargetType::Objects => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        anyhow::bail!("couldn't find file {}", file.display());
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                paths.push(path);
                asts.push(ast);
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            Ok(output)
        }
        TargetType::Meta => {
            let output = build_dir.join(name);
            let mut vec = Vec::new();
            for lib in &cc.abss {
                vec.extend_from_slice(&lib.to_raw_bytes());
                vec.push(0);
            }
            vec.pop();
            output.write_anyhow(vec)?;
            Ok(output)
        }
    }
}
/// Resolve dependencies for a target
/// The header is loaded into the compilation context, and it returns a tuple containing if a
/// dependency has been rebuilt and a Vec containing the files that need to be linked
fn resolve_deps(
    ctx: &CompCtx,
    cmd: &mut cc::CompileCommand,
    t: &Target,
    targets: &HashMap<&str, (&Target, Cell<Option<PathBuf>>)>,
    opts: &BuildOptions,
    source_dir: &Path,
    build_dir: &Path,
) -> anyhow::Result<bool> {
    let mut out = vec![];
    let mut libs = vec![];
    let mut conflicts = vec![];
    let mut to_install = vec![];
    let mut changed = false;
    for (target, version) in t.deps.iter() {
        match version {
            Dependency::Project => {
                let (t, d) = if let Some(t) = targets.get(&**target) {
                    t
                } else {
                    anyhow::bail!("target {target:?} is not a target in this project")
                };
                if d.with(|d| {
                    if let Some(a) = d {
                        libs::load_lib(a, ctx)?;
                        anyhow::Ok(true)
                    } else {
                        anyhow::Ok(false)
                    }
                })? {
                    continue;
                }
                let (c, artifact) =
                    build_target(t, target, d, targets, opts, source_dir, build_dir)?;
                changed |= c;
                libs::load_lib(&artifact, ctx)?;
                cmd.link_abs(artifact);
            }
            Dependency::System => libs.push(target.clone()),
            Dependency::Package(PkgDepSpec { version, targets }) => {
                to_install.push(pkg::InstallSpec {
                    name: target.to_string(),
                    version: version.clone(),
                    targets: targets
                        .as_ref()
                        .map(|t| t.iter().map(ToString::to_string).collect()),
                })
            }
        }
    }
    if !to_install.is_empty() {
        let plan = pkg::install(
            to_install.iter().cloned(),
            &pkg::InstallOptions {
                target: opts.triple.to_string(),
                ..Default::default()
            },
        )?;
        let mut installed_path = cobalt_dir()?;
        installed_path.push("installed");
        to_install
            .into_iter()
            .try_for_each(|pkg::InstallSpec { name, targets, .. }| {
                targets
                    .unwrap_or_else(|| vec!["default".to_string()])
                    .into_iter()
                    .try_for_each(|target| {
                        let mut path = installed_path.clone();
                        path.push(&name);
                        path.push(plan[&(name.as_str(), target.as_str())].to_string());
                        path.push(&target);
                        conflicts.append(&mut libs::load_lib(&path, ctx)?);
                        out.push(path);
                        anyhow::Ok(())
                    })
            })?;
    }
    if !conflicts.is_empty() {
        anyhow::bail!(libs::ConflictingDefs(conflicts))
    }
    let notfound = cmd
        .search_libs(libs, Some(ctx), false)?
        .into_iter()
        .map(From::from)
        .collect::<Vec<_>>();
    if !notfound.is_empty() {
        anyhow::bail!(LibsNotFound(notfound))
    }
    Ok(changed)
}
/// Build a single target. This is used with the `build` entry function
fn build_target(
    t: &Target,
    name: &str,
    data: &Cell<Option<PathBuf>>,
    targets: &HashMap<&str, (&Target, Cell<Option<PathBuf>>)>,
    opts: &BuildOptions,
    source_dir: &Path,
    build_dir: &Path,
) -> anyhow::Result<(bool, PathBuf)> {
    let ink_ctx = inkwell::context::Context::create();
    let mut ctx = CompCtx::new(&ink_ctx, "");
    ctx.flags.prepass = false;
    let mut cc = cc::CompileCommand::new();
    cc.no_default_link = opts.no_default_link;
    cc.link_dirs(opts.link_dirs.iter().cloned());
    let rebuild = resolve_deps(&ctx, &mut cc, t, targets, opts, source_dir, build_dir)?;
    let mut changed = rebuild;
    match t.target_type {
        TargetType::Executable => {
            let mut paths = vec![];
            let mut asts = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            let mut passed = false;
            let mut failed = None;
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        if opts.continue_build {
                            if opts.continue_build {
                                error!("couldn't find file {}", file.display());
                                failed = Some(file);
                                continue;
                            } else {
                                anyhow::bail!("couldn't find file {}", file.display());
                            }
                        } else {
                            anyhow::bail!("couldn't find file {}", file.display());
                        }
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
                passed = true;
            }
            if let Some(file) = failed {
                anyhow::bail!("couldn't find file {}", file.display());
            }
            if let (FileList::Glob(files), false) = (files, passed) {
                warning!("no files matching glob {files}")
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            cc.objs(paths.into_iter().map(|([x, _], _)| x));
            cc.output(&output);
            cc.run()?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
        TargetType::Library => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            let mut passed = false;
            let mut failed = None;
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        if opts.continue_build {
                            if opts.continue_build {
                                error!("couldn't find file {}", file.display());
                                failed = Some(file);
                                continue;
                            } else {
                                anyhow::bail!("couldn't find file {}", file.display());
                            }
                        } else {
                            anyhow::bail!("couldn't find file {}", file.display());
                        }
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
                passed = true;
            }
            if let Some(file) = failed {
                anyhow::bail!("couldn't find file {}", file.display());
            }
            if let (FileList::Glob(files), false) = (files, passed) {
                warning!("no files matching glob {files}")
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(libs::format_lib(name, &opts.triple, true));
            cc.lib(true);
            cc.objs(paths.into_iter().flat_map(|x| x.0));
            cc.output(&output);
            cc.run()?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
        TargetType::Archive => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            let mut passed = false;
            let mut failed = None;
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        if opts.continue_build {
                            if opts.continue_build {
                                error!("couldn't find file {}", file.display());
                                failed = Some(file);
                                continue;
                            } else {
                                anyhow::bail!("couldn't find file {}", file.display());
                            }
                        } else {
                            anyhow::bail!("couldn't find file {}", file.display());
                        }
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
                passed = true;
            }
            if let Some(file) = failed {
                anyhow::bail!("couldn't find file {}", file.display());
            }
            if let (FileList::Glob(files), false) = (files, passed) {
                warning!("no files matching glob {files}")
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            let mut builder = ar::Builder::new(output.create_file_anyhow()?);
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                    builder.append_path(p2)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            Ok((changed, output))
        }
        TargetType::Objects => {
            let mut asts = vec![];
            let mut paths = vec![];
            let mut num_errs = 0;
            let files = t
                .files
                .as_ref()
                .ok_or(anyhow::anyhow!("target must have files"))?;
            let required = files.is_list();
            let mut passed = false;
            let mut failed = None;
            for file in files.iter(source_dir).filter_map(Result::ok) {
                if std::fs::metadata(&file)
                    .map(|m| !m.file_type().is_file())
                    .unwrap_or(true)
                {
                    if required {
                        if opts.continue_build {
                            if opts.continue_build {
                                error!("couldn't find file {}", file.display());
                                failed = Some(file);
                                continue;
                            } else {
                                anyhow::bail!("couldn't find file {}", file.display());
                            }
                        } else {
                            anyhow::bail!("couldn't find file {}", file.display());
                        }
                    }
                    continue;
                }
                let (path, ast) = build_file_1(
                    file.as_path(),
                    &ctx,
                    opts,
                    true,
                    &mut num_errs,
                    source_dir,
                    build_dir,
                )?;
                changed |= ast.is_some();
                paths.push(path);
                asts.push(ast);
                passed = true;
            }
            if let Some(file) = failed {
                anyhow::bail!("couldn't find file {}", file.display());
            }
            if let (FileList::Glob(files), false) = (files, passed) {
                warning!("no files matching glob {files}")
            }
            let mut succ = true;
            for (([p1, p2], fail), ast) in paths.iter().zip(asts) {
                if let Some(ast) = ast {
                    succ &= build_file_2(ast, &ctx, opts, (p1, p2), *fail, &mut num_errs)?;
                }
            }
            if !succ {
                anyhow::bail!(CompileErrors(num_errs))
            }
            let mut output = build_dir.to_path_buf();
            output.push(name);
            Ok((changed, output))
        }
        TargetType::Meta => {
            let output = build_dir.join(name.to_string() + ".meta");
            let mut vec = Vec::new();
            for lib in &cc.abss {
                vec.extend_from_slice(&lib.to_raw_bytes());
                vec.push(0);
            }
            vec.pop();
            output.write_anyhow(vec)?;
            data.set(Some(output.clone()));
            Ok((changed, output))
        }
    }
}
/// Build the project. If to_build is None, all targets are built
pub fn build(
    pkg: &Project,
    to_build: Option<Vec<String>>,
    opts: &BuildOptions,
) -> anyhow::Result<()> {
    if let Some(v) = &pkg.co_version {
        if !v.matches(&env!("CARGO_PKG_VERSION").parse::<Version>().unwrap()) {
            anyhow::bail!(
                r#"project has Cobalt version requirement "{v}", but Cobalt version is {}"#,
                env!("CARGO_PKG_VERSION")
            );
        }
    }
    let targets = pkg
        .targets
        .iter()
        .map(|(n, t)| (&**n, (t, Cell::default())))
        .collect::<HashMap<_, (&Target, Cell<Option<PathBuf>>)>>();
    if let Some(tb) = to_build {
        for target_name in tb {
            let (target, data) = if let Some(t) = targets.get(&*target_name) {
                t
            } else {
                anyhow::bail!("target {target_name:?} is not a target in this project");
            };
            if data.with(|x| x.is_some()) {
                continue;
            }
            build_target(
                target,
                &target_name,
                data,
                &targets,
                opts,
                &pkg.source_dir,
                &pkg.build_dir,
            )?;
        }
    } else {
        for (name, (target, data)) in targets.iter() {
            if data.with(|x| x.is_some()) {
                continue;
            }
            build_target(
                target,
                name,
                data,
                &targets,
                opts,
                &pkg.source_dir,
                &pkg.build_dir,
            )?;
        }
    }
    Ok(())
}
