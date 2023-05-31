use inkwell::targets::*;
use inkwell::execution_engine::FunctionLookupError;
use std::process::{Command, exit};
use std::io::{prelude::*, BufReader};
use std::path::{Path, PathBuf};
use std::fmt;
use anyhow::Context;
use anyhow_std::*;
use const_format::{formatcp, str_index};
use clap::{Parser, Subcommand, ValueEnum};

use cobalt_ast::{CompCtx, AST};
use cobalt_errors::*;
use cobalt_build::*;
use cobalt_utils::Flags;
use cobalt_parser::parse_tl;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum OutputType {
    #[default]
    #[value(name = "exe")]
    Executable,
    #[value(name = "lib")]
    Library,
    #[value(name = "obj")]
    Object,
    #[value(name = "asm")]
    Assembly,
    #[value(name = "llvm")]
    Llvm,
    #[value(name = "bc")]
    Bitcode,
    #[value(name = "header")]
    Header,
    #[value(name = "header-obj")]
    HeaderObj
}
impl fmt::Display for OutputType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Executable => "exe",
            Self::Library => "lib",
            Self::Object => "obj",
            Self::Assembly => "asm",
            Self::Llvm => "llvm",
            Self::Bitcode => "bc",
            Self::Header => "header",
            Self::HeaderObj => "header-obj"
        })
    }
}

const INIT_NEEDED: InitializationConfig = InitializationConfig {
    asm_parser: true,
    asm_printer: true,
    base: true,
    disassembler: false,
    info: true,
    machine_code: true
};
static LONG_VERSION: &'static str = formatcp!("{}\nLLVM version {}{}{}",
    env!("CARGO_PKG_VERSION"), cobalt_ast::LLVM_VERSION,
    if cfg!(has_git) {formatcp!("\nGit commit {} on branch {}", str_index!(env!("GIT_COMMIT"), ..6), env!("GIT_BRANCH"))} else {""},
    if cfg!(debug_assertions) {"\nDebug Build"} else {""}
);
#[derive(Debug, Clone, Parser)]
#[command(name = "co", author, long_version = LONG_VERSION)]
enum Cli {
    /// Print version information
    Version,
    /// Test parser (debug-only)
    #[cfg(debug_assertions)]
    Parse {
        /// input file to parse
        files: Vec<String>,
        /// raw string to parse
        #[arg(short)]
        code: Vec<String>,
        /// print locations of AST nodes
        #[arg(short)]
        locs: bool
    },
    /// Test LLVM generation
    #[cfg(debug_assertions)]
    Llvm {
        /// input file to compile
        input: String
    },
    /// Parse a Cobalt header
    #[cfg(debug_assertions)]
    ParseHeader {
        /// header files to parse
        #[arg(required = true)]
        inputs: Vec<String>
    },
    /// AOT compile a file
    Aot {
        /// input file to compile
        input: String,
        /// output file
        #[arg(short, long)]
        output: Option<String>,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<String>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<String>,
        /// target triple to build
        #[arg(short, long)]
        triple: Option<String>,
        /// type of file to emit
        #[arg(short, long, default_value_t)]
        emit: OutputType,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// continue compilation of errors are encountered
        #[arg(short, long = "continue")]
        continue_if_err: bool,
        /// emit symbols with debug mangling
        #[arg(short, long = "debug")]
        debug_mangle: bool,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool
    },
    /// JIT compile and run a file
    Jit {
        /// input file to compile
        input: String,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<String>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<String>,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// continue compilation of errors are encountered
        #[arg(short, long = "continue")]
        continue_if_err: bool,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        #[arg(last = true)]
        args: Vec<String>
    },
    /// Check a file for errors
    Check {
        /// input file to compile
        input: String,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<String>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<String>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool
    },
    /// project-related utilities
    #[command(subcommand, alias = "proj")]
    Project(ProjSubcommand),
    /// package-related utilities
    #[command(subcommand, alias = "pkg")]
    Package(PkgSubcommand)
}
#[derive(Debug, Clone, Subcommand)]
enum ProjSubcommand {
    /// Track the given projects
    ///
    /// If none are given, the current directory and its parents are searched
    Track {
        projects: Option<Vec<String>>
    },
    /// Untrack the given projects
    ///
    /// If none are given, the current directory and its parents are searched
    Untrack {
        projects: Option<Vec<String>>
    },
    /// List the currently tracked projects
    List {
        #[arg(short, long)]
        machine: bool
    },
    /// Build a project
    Build {
        /// project file or its directory
        project_dir: Option<String>,
        /// directory that source files are relative to
        #[arg(short, long = "source")]
        source_dir: Option<String>,
        /// directory to output build artifacts
        #[arg(short, long = "build")]
        build_dir: Option<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<String>,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// target triple to build for
        #[arg(short, long)]
        triple: Option<String>,
        /// targets to build
        #[arg(short = 'T', long = "target")]
        targets: Vec<String>,
        /// rebuild all files, even those that are up to date
        #[arg(short, long)]
        rebuild: bool
    },
    /// Build and run a target in a project
    #[command(alias = "exec")]
    Run {
        /// project file or its directory
        project_dir: Option<String>,
        /// directory that source files are relative to
        #[arg(short, long = "source")]
        source_dir: Option<String>,
        /// directory to output build artifacts
        #[arg(short, long = "build")]
        build_dir: Option<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<String>,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// targets to build
        #[arg(short = 'T', long)]
        target: Option<String>,
        /// rebuild all files, even those that are up to date
        #[arg(short, long)]
        rebuild: bool,
        #[arg(last = true)]
        args: Vec<String>
    },
}
#[derive(Debug, Clone, Subcommand)]
enum PkgSubcommand {
    /// Update all registries
    Update,
    /// Install packages
    Install {
        /// packages to install
        #[arg(required = true)]
        packages: Vec<String>
    },
}

#[inline(always)]
fn driver() -> anyhow::Result<()> {
    match Cli::parse() {
        Cli::Version => println!("co {LONG_VERSION}"),
        #[cfg(debug_assertions)]
        Cli::Parse {files, code, locs} => {
            for code in code {
                let file = FILES.add_file(0, "<command line>".to_string(), code.clone());
                let (mut ast, errs) = parse_tl(&code);
                ast.file = Some(file);
                for err in errs {eprintln!("{:?}", Report::from(err).with_source_code(file));}
                if locs {print!("{:#}", ast)}
                else {print!("{}", ast)}
            }
            for arg in files {
                let code = Path::new(&arg).read_to_string_anyhow()?;
                let file = FILES.add_file(0, arg.clone(), code.clone());
                let (mut ast, errs) = parse_tl(&code);
                ast.file = Some(file);
                for err in errs {eprintln!("{:?}", Report::from(err).with_source_code(file));}
                if locs {print!("{:#}", ast)}
                else {print!("{}", ast)}
            }
        },
        #[cfg(debug_assertions)]
        Cli::Llvm {input} => {
            let code = if input == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {Path::new(&input).read_to_string_anyhow()?};
            let mut flags = Flags::default();
            flags.dbg_mangle = true;
            let ink_ctx = inkwell::context::Context::create();
            let ctx = CompCtx::with_flags(&ink_ctx, &input, flags);
            let file = FILES.add_file(0, input, code.clone());
            let (mut ast, errs) = parse_tl(&code);
            ast.file = Some(file);
            for err in errs {eprintln!("{:?}", Report::from(err).with_source_code(file));}
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {eprintln!("{:?}", Report::from(err).with_source_code(file));}
            if let Err(msg) = ctx.module.verify() {error!("\n{}", msg.to_string())}
            print!("{}", ctx.module.to_string());
        },
        #[cfg(debug_assertions)]
        Cli::ParseHeader {inputs} => {
            for fname in inputs {
                let ink_ctx = inkwell::context::Context::create();
                let ctx = CompCtx::new(&ink_ctx, "<anon>");
                let mut file = BufReader::new(match std::fs::File::open(&fname) {Ok(f) => f, Err(e) => {eprintln!("error opening {fname}: {e}"); continue}});
                match ctx.load(&mut file) {
                    Ok(_) => ctx.with_vars(|v| v.dump()),
                    Err(e) => eprintln!("error loading {fname}: {e}")
                }
            }
        },
        Cli::Aot {input, output, linked, mut link_dirs, headers, triple, emit, profile, continue_if_err, debug_mangle, no_default_link} => {
            if !no_default_link {
                if let Some(pwd) = std::env::current_dir().ok().and_then(|pwd| pwd.to_str().map(String::from)) {link_dirs.insert(0, pwd);}
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let code = if input == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {Path::new(&input).read_to_string_anyhow()?};
            if triple.is_some() {Target::initialize_all(&INIT_NEEDED)}
            else {Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?}
            let triple = triple.unwrap_or_else(|| TargetMachine::get_default_triple().as_str().to_string_lossy().into_owned());
            let trip = TargetTriple::create(&triple);
            let output = output.map(String::from).unwrap_or_else(|| match emit {
                OutputType::Executable => format!("{}{}", input.rfind('.').map_or(input.as_str(), |i| &input[..i]), if triple.contains("windows") {".exe"} else {""}),
                OutputType::Library => libs::format_lib(input.rfind('.').map_or(input.as_str(), |i| &input[..i]), &trip),
                OutputType::Object => format!("{}.o", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
                OutputType::Assembly => format!("{}.s", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
                OutputType::Llvm => format!("{}.ll", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
                OutputType::Bitcode => format!("{}.bc", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
                OutputType::Header => format!("{}.coh", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
                OutputType::HeaderObj => format!("{}.coh.o", input.rfind('.').map_or(input.as_str(), |i| &input[..i])),
            });
            let output = if output == "-" {None} else {Some(output)};
            let target_machine = Target::from_triple(&trip).unwrap().create_target_machine(
                &trip,
                "",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Small
            ).expect("failed to create target machine");
            let mut flags = Flags::default();
            flags.dbg_mangle = debug_mangle;
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx.ptr_sized_int_type(&target_machine.get_target_data(), None).size_of().get_zero_extended_constant() {flags.word_size = size as u16;}
            let ctx = CompCtx::with_flags(&ink_ctx, &input, flags);
            ctx.module.set_triple(&trip);
            let libs = if !linked.is_empty() {
                let (libs, notfound) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect::<Vec<_>>(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
                libs
            } else {vec![]};
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let mut overall_fail = false;
            let file = FILES.add_file(0, input.to_string(), code.clone());
            let (mut ast, errs) = parse_tl(&code);
            ast.file = Some(file);
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {anyhow::bail!(CompileErrors)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            if fail && !continue_if_err {anyhow::bail!(CompileErrors)}
            if let Err(msg) = ctx.module.verify() {
                error!("\n{}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {anyhow::bail!(CompileErrors)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile.as_deref(), &pm);
            pm.run_on(&ctx.module);
            match emit {
                OutputType::Header =>
                    if let Some(out) = output {
                        let mut file = std::fs::File::create(out)?;
                        ctx.save(&mut file)?;
                    }
                    else {ctx.save(&mut std::io::stdout())?}
                OutputType::HeaderObj => {
                    let mut obj = libs::new_object(&trip);
                    libs::populate_header(&mut obj, &ctx);
                    let vec = obj.write()?;
                    if let Some(out) = output {
                        let mut file = std::fs::File::create(out)?;
                        file.write_all(&vec)?;
                    }
                    else {std::io::stdout().write_all(&vec)?}
                }
                OutputType::Llvm =>
                    if let Some(out) = output {std::fs::write(out, ctx.module.to_string().as_bytes())?}
                    else {println!("{}", ctx.module.to_string())},
                OutputType::Bitcode =>
                    if let Some(out) = output {std::fs::write(out, ctx.module.write_bitcode_to_memory().as_slice())?}
                    else {std::io::stdout().write_all(ctx.module.write_bitcode_to_memory().as_slice())?},
                OutputType::Assembly => {
                    let code = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Assembly).unwrap();
                    if let Some(out) = output {std::fs::write(out, code.as_slice())?}
                    else {std::io::stdout().write_all(code.as_slice())?}
                }
                _ => {
                    let mb = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object).unwrap();
                    match emit {
                        OutputType::Executable => {
                            if output.is_none() {
                                eprintln!("cannot output executable to stdout");
                                exit(4)
                            }
                            let tmp = temp_file::with_contents(mb.as_slice());
                            let mut cc = cc::CompileCommand::new();
                            cc.obj(tmp.path());
                            cc.output(output.unwrap());
                            cc.link_libs(libs.into_iter().map(|(l, _)| l));
                            cc.target(&triple);
                            let res = cc.build()?.status_anyhow()?;
                            std::mem::drop(tmp);
                            exit(res.code().unwrap_or(-1));
                        },
                        OutputType::Library => {
                            if let Some(output) = output {
                                let mut obj = libs::new_object(&trip);
                                libs::populate_header(&mut obj, &ctx);
                                let tmp1 = temp_file::with_contents(&obj.write()?);
                                let tmp2 = temp_file::with_contents(mb.as_slice());
                                let mut cc = cc::CompileCommand::new();
                                cc.lib(true);
                                cc.objs([tmp1.path(), tmp2.path()]);
                                cc.output(&output);
                                cc.link_libs(libs.into_iter().map(|(l, _)| l));
                                cc.target(&triple);
                                let res = cc.build()?.status_anyhow()?;
                                std::mem::drop(tmp1);
                                std::mem::drop(tmp2);
                                exit(res.code().unwrap_or(-1))
                            }
                            else {error!("cannot output library to stdout!"); exit(4)}
                        },
                        OutputType::Object =>
                            if let Some(out) = output {std::fs::write(out, mb.as_slice())?}
                            else {std::io::stdout().write_all(mb.as_slice())?}
                        x => unreachable!("{x:?} has already been handled")
                    }
                }
            }

        },
        Cli::Jit {input, linked, mut link_dirs, headers, profile, continue_if_err, no_default_link, mut args} => {
            if !no_default_link {
                if let Some(pwd) = std::env::current_dir().ok().and_then(|pwd| pwd.to_str().map(String::from)) {link_dirs.insert(0, pwd);}
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let (input, code) = match input.as_str() {
                "-" => {
                    let mut s = String::new();
                    std::io::stdin().read_to_string(&mut s)?;
                    ("<stdin>", s)
                },
                f => {
                    args[0].push_str(f);
                    (f, Path::new(f).read_to_string_anyhow()?)
                }
            };
            let ink_ctx = inkwell::context::Context::create();
            let mut ctx = CompCtx::new(&ink_ctx, input);
            ctx.flags.dbg_mangle = true;
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let libs = if !linked.is_empty() {
                let (libs, notfound) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
                libs
            } else {vec![]};
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let mut overall_fail = false;
            let file = FILES.add_file(0, input.to_string(), code.clone());
            let (mut ast, errs) = parse_tl(&code);
            ast.file = Some(file);
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {anyhow::bail!(CompileErrors)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            if fail && !continue_if_err {anyhow::bail!(CompileErrors)}
            if let Err(msg) = ctx.module.verify() {
                error!("\n{}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {anyhow::bail!(CompileErrors)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile.as_deref(), &pm);
            pm.run_on(&ctx.module);
            let ee = ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).expect("Couldn't create execution engine!");
            for (lib, _) in libs {
                match lib.extension().map(|x| x.to_str().expect("Path should be valid UTF-8!")).unwrap_or("") {
                    "so" | "dylib" | "dll" => {inkwell::support::load_library_permanently(lib.to_str().expect("Path should be valid UTF-8!"));},
                    "a" | "lib" => todo!("JIT cannot handle static libraries!"),
                    _ => {}
                }
            }
            unsafe {
                let main_fn = match ee.get_function_value("main") {
                    Ok(main_fn) => main_fn,
                    Err(FunctionLookupError::JITNotEnabled) => panic!("JIT not enabled here"),
                    Err(FunctionLookupError::FunctionNotFound) => {
                        eprintln!("couldn't find symbol 'main'");
                        exit(255)
                    }
                };
                ee.run_static_constructors();
                let ec = ee.run_function_as_main(main_fn, &args.iter().map(String::as_str).collect::<Vec<_>>());
                ee.run_static_destructors();
                exit(ec);
            }
        },
        Cli::Check {input, linked, mut link_dirs, headers, no_default_link} => {
            if !no_default_link {
                if let Some(pwd) = std::env::current_dir().ok().and_then(|pwd| pwd.to_str().map(String::from)) {link_dirs.insert(0, pwd);}
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let (input, code) = if input != "-" {(input.as_str(), Path::new(&input).read_to_string_anyhow()?)}
            else {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                ("<stdin>", s)
            };
            Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
            let triple = TargetMachine::get_default_triple();
            let target_machine = Target::from_triple(&triple).unwrap().create_target_machine(
                &triple,
                "",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Small
            ).expect("failed to create target machine");
            let mut flags = Flags::default();
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx.ptr_sized_int_type(&target_machine.get_target_data(), None).size_of().get_zero_extended_constant() {flags.word_size = size as u16;}
            let ctx = CompCtx::with_flags(&ink_ctx, input, flags);
            ctx.module.set_triple(&triple);
            if !linked.is_empty() {
                let (_, notfound) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
            }
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let file = FILES.add_file(0, input.to_string(), code.clone());
            let (mut ast, errs) = parse_tl(&code);
            ast.file = Some(file);
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {fail |= err.is_err(); eprintln!("{:?}", Report::from(err).with_source_code(file));}
            if let Err(msg) = ctx.module.verify() {
                error!("\n{}", msg.to_string());
                anyhow::bail!(CompileErrors)
            }
            if fail {anyhow::bail!(CompileErrors)}
        },
        Cli::Project(cmd) => match cmd {
            ProjSubcommand::Track {projects: None} => {
                'found: {
                    for path in std::env::current_dir()?.ancestors() {
                        let cfg_path = path.join("cobalt.toml");
                        if !cfg_path.exists() {continue}
                        if let Some(proj) = std::fs::read_to_string(&cfg_path).ok().and_then(|x| toml::from_str::<build::Project>(&x).ok()) {
                            let mut vecs = load_projects()?;
                            track_project(&proj.name, cfg_path, &mut vecs);
                            save_projects(vecs)?;
                            break 'found
                        }
                    }
                    anyhow::bail!("couldn't find cobalt.toml in currnet or parent directories");
                }
            },
            ProjSubcommand::Track {projects: Some(projs)} => {
                let mut vec = load_projects()?;
                for arg in projs {
                    let mut path: PathBuf = arg.into();
                    if path.is_dir() {path.push("cobalt.toml");}
                    track_project(&toml::from_str::<build::Project>(&Path::new(&path).read_to_string_anyhow()?)?.name, path, &mut vec);
                }
                save_projects(vec)?;
            },
            ProjSubcommand::Untrack {projects: None} => {
                'found: {
                    for path in std::env::current_dir()?.ancestors() {
                        let cfg_path = path.join("cobalt.toml");
                        if !cfg_path.exists() {continue}
                        if std::fs::read_to_string(&cfg_path).ok().and_then(|x| toml::from_str::<build::Project>(&x).ok()).is_some() {
                            let mut vecs = load_projects()?;
                            vecs.retain(|[_, p]| Path::new(p) != cfg_path);
                            save_projects(vecs)?;
                            break 'found
                        }
                    }
                    error!("couldn't find cobalt.toml in currnet or parent directories");
                }
            },
            ProjSubcommand::Untrack {projects: Some(projs)} => {
                let mut vec = load_projects()?;
                for arg in projs {
                    let mut path: PathBuf = arg.into();
                    if path.is_dir() {path.push("cobalt.toml");}
                    vec.retain(|[_, p]| Path::new(p) != path);
                }
                save_projects(vec)?;
            },
            ProjSubcommand::List {machine} => {
                let vecs = load_projects()?;
                if machine {vecs.iter().for_each(|[n, p]| println!("{n}\t{p}"))}
                else {
                    let padding = vecs.iter().map(|[n, _]| n.chars().count()).max().unwrap_or(0);
                    vecs.iter().for_each(|[n, p]| println!("{n}{} => {p}", " ".repeat(padding - n.chars().count())));
                }
            },
            ProjSubcommand::Build {project_dir, source_dir, build_dir, profile, mut link_dirs, no_default_link, triple, targets, rebuild} => {
                let (project_data, project_dir) = match project_dir.as_deref() {
                    Some("-") => {
                        let mut cfg = String::new();
                        std::io::stdin().read_to_string(&mut cfg).context("failed to read project file")?;
                        (toml::from_str::<build::Project>(cfg.as_str()).context("failed to parse project file")?, PathBuf::from("."))
                    },
                    Some(x) => {
                        let mut x = x.to_string();
                        let mut vecs = load_projects()?;
                        if x.as_bytes()[0] == b':' {
                            if let Some(p) = vecs.iter().find_map(|[n, p]| (n == &x[1..]).then_some(p).cloned()) {x = p}
                        }
                        if Path::new(&x).metadata_anyhow()?.file_type().is_dir() {
                            let mut path = std::path::PathBuf::from(&x);
                            path.push("cobalt.toml");
                            if !path.exists() {anyhow::bail!("failed to find cobalt.toml in {x}")}
                            let cfg = path.read_to_string_anyhow()?;
                            let cfg = toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?;
                            track_project(&cfg.name, path, &mut vecs);
                            save_projects(vecs)?;
                            (cfg , PathBuf::from(x))
                        }
                        else {
                            let mut path = std::path::PathBuf::from(&x);
                            let cfg = path.read_to_string_anyhow()?;
                            path.pop();
                            let cfg = toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?;
                            track_project(&cfg.name, x.clone().into(), &mut vecs);
                            save_projects(vecs)?;
                            (cfg, path)
                        }
                    },
                    None => {
                        let mut path = std::env::current_dir()?;
                        loop {
                            path.push("cobalt.toml");
                            if path.exists() {break}
                            path.pop();
                            if !path.pop() {anyhow::bail!("couldn't find cobalt.toml in current directory")}
                        }
                        let cfg = Path::new(&path).read_to_string_anyhow()?;
                        path.pop();
                        (toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?, path)
                    }
                };
                if !no_default_link {
                    if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                    else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
                }
                let source_dir: PathBuf = source_dir.map_or(project_dir.clone(), PathBuf::from);
                let build_dir: PathBuf = build_dir.map_or_else(|| {
                    let mut dir = project_dir.clone();
                    dir.push("build");
                    dir
                }, PathBuf::from);
                if triple.is_some() {Target::initialize_all(&INIT_NEEDED)}
                else {Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?}
                build::build(project_data, if targets.is_empty() {None} else {Some(targets.into_iter().map(String::from).collect())}, &build::BuildOptions {
                    source_dir: &source_dir,
                    build_dir: &build_dir,
                    profile: profile.as_deref().unwrap_or("default"),
                    triple: &triple.map_or_else(TargetMachine::get_default_triple, |x| TargetTriple::create(&x)),
                    continue_build: false,
                    continue_comp: false,
                    rebuild,
                    link_dirs: link_dirs.iter().map(|x| x.as_str()).collect()
                })?;
            },
            ProjSubcommand::Run {project_dir, source_dir, build_dir, profile, mut link_dirs, no_default_link, target, rebuild, args} => {
                let (project_data, project_dir) = match project_dir.as_deref() {
                    Some("-") => {
                        let mut cfg = String::new();
                        std::io::stdin().read_to_string(&mut cfg).context("failed to read project file")?;
                        (toml::from_str::<build::Project>(cfg.as_str()).context("failed to parse project file")?, PathBuf::from("."))
                    },
                    Some(x) => {
                        let mut x = x.to_string();
                        let mut vecs = load_projects()?;
                        if x.as_bytes()[0] == b':' {
                            if let Some(p) = vecs.iter().find_map(|[n, p]| (n == &x[1..]).then_some(p).cloned()) {x = p}
                        }
                        if Path::new(&x).metadata_anyhow()?.file_type().is_dir() {
                            let mut path = std::path::PathBuf::from(&x);
                            path.push("cobalt.toml");
                            if !path.exists() {anyhow::bail!("failed to find cobalt.toml in {x}")}
                            let cfg = path.read_to_string_anyhow()?;
                            let cfg = toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?;
                            track_project(&cfg.name, path, &mut vecs);
                            save_projects(vecs)?;
                            (cfg , PathBuf::from(x))
                        }
                        else {
                            let mut path = std::path::PathBuf::from(&x);
                            let cfg = path.read_to_string_anyhow()?;
                            path.pop();
                            let cfg = toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?;
                            track_project(&cfg.name, x.clone().into(), &mut vecs);
                            save_projects(vecs)?;
                            (cfg, path)
                        }
                    },
                    None => {
                        let mut path = std::env::current_dir()?;
                        loop {
                            path.push("cobalt.toml");
                            if path.exists() {break}
                            path.pop();
                            if !path.pop() {anyhow::bail!("couldn't find cobalt.toml in current directory")}
                        }
                        let cfg = Path::new(&path).read_to_string_anyhow()?;
                        path.pop();
                        (toml::from_str::<build::Project>(&cfg).context("failed to parse project file")?, path)
                    }
                };
                if !no_default_link {
                    if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                    else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
                }
                let source_dir: PathBuf = source_dir.map_or(project_dir.clone(), PathBuf::from);
                let build_dir: PathBuf = build_dir.map_or_else(|| {
                    let mut dir = project_dir.clone();
                    dir.push("build");
                    dir
                }, PathBuf::from);
                Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
                let mut target = target.map_or_else(|| {
                    let exes = project_data.targets.iter().filter_map(|(k, x)| (x.target_type == build::TargetType::Executable).then_some(k.as_str())).collect::<Vec<_>>();
                    match exes.len() {
                        0 => anyhow::bail!("no executable targets available for current project"),
                        1 => Ok(exes[0].to_string()),
                        x => anyhow::bail!("{x} executable targets available, please select one: {exes:?}")
                    }
                }, |t| {
                    if project_data.targets.get(&t).map(|x| x.target_type) != Some(build::TargetType::Executable) {anyhow::bail!("target type must be an executable")}
                    Ok(t.to_string())
                })?;
                let triple = TargetMachine::get_default_triple();
                build::build(project_data, Some(vec![target.clone()]), &build::BuildOptions {
                    source_dir: &source_dir,
                    build_dir: &build_dir,
                    profile: profile.as_deref().unwrap_or("default"),
                    triple: &triple,
                    continue_build: false,
                    continue_comp: false,
                    rebuild,
                    link_dirs: link_dirs.iter().map(|x| x.as_str()).collect()
                })?;
                let mut exe_path = build_dir;
                if triple.as_str().to_str().ok().map_or(false, |t| t.contains("windows")) {target.push_str(".exe");}
                exe_path.push(target);
                exit(Command::new(exe_path).args(args).status()?.code().unwrap_or(-1))
            },
        },
        Cli::Package(cmd) => match cmd {
            PkgSubcommand::Update => pkg::update_packages()?,
            PkgSubcommand::Install {packages} => {pkg::install(packages.into_iter().map(|x| x.parse()).collect::<Result<Vec<_>, _>>()?, &Default::default())?;}
        },
    }
    Ok(())
}
fn main() {
    if let Err(err) = driver() {
        error!("{err:#}");
        exit(100);
    }
}
