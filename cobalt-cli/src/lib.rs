use anyhow::Context;
use anyhow_std::*;
use clap::{Parser, Subcommand, ValueEnum};
use clio::{ClioPath, Input, OutputPath};
use cobalt_ast::{CompCtx, Flags, AST};
use cobalt_build::*;
use cobalt_errors::*;
use cobalt_parser::parse_str;
use const_format::{formatcp, str_index};
use human_repr::*;
use inkwell::execution_engine::FunctionLookupError;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::*;
use os_str_bytes::OsStringBytes;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::io::{prelude::*, BufReader};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};
mod tests;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum OutputType {
    #[default]
    #[value(name = "exe", alias = "bin")]
    Executable,
    #[value(name = "lib", alias = "shared")]
    Library,
    #[value(name = "archive", alias = "ar", alias = "static")]
    Archive,
    #[value(name = "obj", alias = "object")]
    Object,
    #[value(name = "raw-obj", alias = "raw-object")]
    RawObject,
    #[value(name = "asm", alias = "assembly")]
    Assembly,
    #[value(name = "ir", alias = "llvm-ir")]
    Llvm,
    #[value(name = "bc", alias = "llvm-bc")]
    Bitcode,
    #[value(name = "header", alias = "raw-header")]
    Header,
    #[value(name = "header-obj", alias = "header-object")]
    HeaderObj,
}
impl fmt::Display for OutputType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Executable => "exe",
            Self::Library => "lib",
            Self::Archive => "archive",
            Self::Object => "obj",
            Self::RawObject => "raw-obj",
            Self::Assembly => "asm",
            Self::Llvm => "ir",
            Self::Bitcode => "bc",
            Self::Header => "header",
            Self::HeaderObj => "header-obj",
        })
    }
}

const INIT_NEEDED: InitializationConfig = InitializationConfig {
    asm_parser: true,
    asm_printer: true,
    base: true,
    disassembler: false,
    info: true,
    machine_code: true,
};
pub static LONG_VERSION: &str = formatcp!(
    "{}\nLLVM version {}\nGit {}\nDebug {}",
    env!("CARGO_PKG_VERSION"),
    cobalt_llvm::LLVM_VERSION,
    if cfg!(has_git) {
        formatcp!(
            "commit {} on branch {}",
            str_index!(env!("GIT_COMMIT"), ..6),
            env!("GIT_BRANCH")
        )
    } else {
        "not found"
    },
    if cfg!(debug_assertions) {
        "enabled"
    } else {
        "disabled"
    }
);
#[derive(Debug, Clone, Parser)]
#[command(name = "co", author, long_version = LONG_VERSION)]
pub enum Cli {
    /// Print version information
    #[command(version)]
    Version,
    /// Debug compiler stuff
    #[cfg(debug_assertions)]
    #[command(alias = "dbg", subcommand)]
    Debug(DbgSubcommand),
    /// AOT compile a file
    Aot {
        /// input file to compile
        input: Input,
        /// output file
        #[arg(short, long)]
        output: Option<OutputPath>,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
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
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
    },
    /// JIT compile and run a file
    Jit {
        /// input file to compile
        input: Input,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// continue compilation of errors are encountered
        #[arg(short, long = "continue")]
        continue_if_err: bool,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
        /// argv[0] to pass to program
        #[arg(short, long)]
        this: Option<String>,
        /// arguments to pass to program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a file for errors
    Check {
        /// input file to compile
        input: Input,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
        /// Print header as JSON
        #[arg(long)]
        dump_header: bool,
    },
    /// multi-file utilities
    #[command(subcommand)]
    Multi(MultiSubcommand),
    /// project-related utilities
    #[command(subcommand, alias = "proj")]
    Project(ProjSubcommand),
    /// package-related utilities
    #[command(subcommand, alias = "pkg")]
    Package(PkgSubcommand),
}
#[cfg(debug_assertions)]
#[derive(Debug, Clone, Subcommand)]
pub enum DbgSubcommand {
    /// Test parser
    Parse {
        /// input file to parse
        files: Vec<Input>,
        /// raw string to parse
        #[arg(short)]
        code: Vec<String>,
        /// print locations of AST nodes
        #[arg(short)]
        locs: bool,
    },
    /// Test LLVM generation
    Llvm {
        /// input file to compile
        input: Input,
        /// print timings
        #[arg(long)]
        timings: bool,
    },
}
#[derive(Debug, Clone, Subcommand)]
pub enum MultiSubcommand {
    /// AOT compile a file
    Aot {
        /// input files to compile
        #[arg(required = true)]
        inputs: Vec<Input>,
        /// output file
        #[arg(short, long)]
        output: Option<ClioPath>,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
        /// target triple to build
        #[arg(short, long)]
        triple: Option<String>,
        /// type of file to emit
        #[arg(short, long, default_value_t)]
        emit: OutputType,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// emit symbols with debug mangling
        #[arg(short, long = "debug")]
        debug_mangle: bool,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
    },
    /// JIT compile and run a file
    Jit {
        /// input files to compile
        #[arg(required = true)]
        inputs: Vec<Input>,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
        /// optimization profile to use
        #[arg(short, long)]
        profile: Option<String>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
        /// argv[0] to pass to program
        #[arg(short, long)]
        this: Option<String>,
        /// arguments to pass to program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Check a file for errors
    Check {
        /// input files to compile
        #[arg(required = true)]
        inputs: Vec<Input>,
        /// libraries to link
        #[arg(short = 'l')]
        linked: Vec<String>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
        /// Cobalt headers to include
        #[arg(short = 'H')]
        headers: Vec<PathBuf>,
        /// don't search default directories for libraries
        #[arg(long)]
        no_default_link: bool,
        /// print timings
        #[arg(long)]
        timings: bool,
        /// Print header as JSON
        #[arg(long)]
        dump_header: bool,
    },
}
#[derive(Debug, Clone, Subcommand)]
pub enum ProjSubcommand {
    /// Track the given projects
    ///
    /// If none are given, the current directory and its parents are searched
    Track { projects: Option<Vec<String>> },
    /// Untrack the given projects
    ///
    /// If none are given, the current directory and its parents are searched
    Untrack { projects: Option<Vec<String>> },
    /// List the currently tracked projects
    List {
        #[arg(short, long)]
        machine: bool,
    },
    /// Build a project
    Build {
        /// project file or its directory
        project_dir: Option<String>,
        /// directory that source files are relative to
        #[arg(short, long = "source")]
        source_dir: Option<PathBuf>,
        /// directory to output build artifacts
        #[arg(short, long = "build")]
        build_dir: Option<PathBuf>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
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
        rebuild: bool,
    },
    /// Build and run a target in a project
    #[command(alias = "exec")]
    Run {
        /// project file or its directory
        project_dir: Option<String>,
        /// directory that source files are relative to
        #[arg(short, long = "source")]
        source_dir: Option<PathBuf>,
        /// directory to output build artifacts
        #[arg(short, long = "build")]
        build_dir: Option<PathBuf>,
        /// link directories to search
        #[arg(short = 'L')]
        link_dirs: Vec<PathBuf>,
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
        args: Vec<String>,
    },
}
#[derive(Debug, Clone, Subcommand)]
pub enum PkgSubcommand {
    /// Update all registries
    Update,
    /// Install packages
    Install {
        /// packages to install
        #[arg(required = true)]
        packages: Vec<String>,
    },
}

/// time the execution of a function
#[inline(always)]
fn timeit<R, F: FnOnce() -> R>(f: F) -> (R, Duration) {
    let start = Instant::now();
    let ret = f();
    (ret, start.elapsed())
}
#[inline(always)]
fn try_timeit<R, E, F: FnOnce() -> Result<R, E>>(f: F) -> Result<(R, Duration), E> {
    let start = Instant::now();
    let ret = f()?;
    Ok((ret, start.elapsed()))
}
/// count instructions in a module
fn insts(m: &Module) -> usize {
    let mut count = 0;
    let mut f = m.get_first_function();
    while let Some(fv) = f {
        let mut b = fv.get_first_basic_block();
        while let Some(bb) = b {
            let mut i = bb.get_first_instruction();
            while let Some(iv) = i {
                count += 1;
                i = iv.get_next_instruction();
            }
            b = bb.get_next_basic_block();
        }
        f = fv.get_next_function();
    }
    count
}
#[derive(Debug, Clone, Copy)]
pub struct Exit(pub i32);
impl std::fmt::Display for Exit {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}
impl std::error::Error for Exit {}

#[inline(always)]
pub fn driver(cli: Cli) -> anyhow::Result<()> {
    match cli {
        Cli::Version => println!("co {LONG_VERSION}"),
        #[cfg(debug_assertions)]
        Cli::Debug(cmd) => match cmd {
            DbgSubcommand::Parse { files, code, locs } => {
                for code in code {
                    let file = FILES.add_file(0, "<command line>".to_string(), code.into());
                    let (ast, errs) = parse_str(file.contents());
                    let mut ast = ast.unwrap_or_default();
                    ast.file = Some(file);
                    for err in errs {
                        eprintln!("{:?}", Report::from(err).with_source_code(file));
                    }
                    if locs {
                        print!("({} nodes)\n{ast:#}", ast.nodes())
                    } else {
                        print!("({} nodes)\n{ast}", ast.nodes())
                    }
                }
                for mut arg in files {
                    let code = std::io::read_to_string(&mut arg)?;
                    let file = FILES.add_file(0, arg.path().to_string(), code.into());
                    let (ast, errs) = parse_str(file.contents());
                    let mut ast = ast.unwrap_or_default();
                    ast.file = Some(file);
                    for err in errs {
                        eprintln!("{:?}", Report::from(err).with_source_code(file));
                    }
                    if locs {
                        print!("({} nodes)\n{ast:#}", ast.nodes())
                    } else {
                        print!("({} nodes)\n{ast}", ast.nodes())
                    }
                }
            }
            DbgSubcommand::Llvm { mut input, timings } => {
                struct Reporter {
                    timings: bool,
                    reported: bool,
                    bottom_line: bool,
                    start: Instant,
                    overall: Option<Duration>,
                    file_time: Option<Duration>,
                    file_len: usize,
                    parse_time: Option<Duration>,
                    ast_nodes: usize,
                    comp_time: Option<Duration>,
                }
                impl Reporter {
                    pub fn new(timings: bool) -> Self {
                        Self {
                            timings,
                            reported: false,
                            bottom_line: true,
                            start: Instant::now(),
                            overall: None,
                            file_time: None,
                            file_len: 0,
                            parse_time: None,
                            ast_nodes: 0,
                            comp_time: None,
                        }
                    }
                    fn print(&mut self) {
                        if !self.timings {
                            return;
                        }
                        if self.reported {
                            return;
                        }
                        self.reported = true;
                        let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                        println!("----------------");
                        println!("overall time: {:>8}", overall.human_duration().to_string());
                        if let Some(file) = self.file_time {
                            println!(
                                "reading file: {:>8} ({:6.3}%) @ {:>13}",
                                file.human_duration().to_string(),
                                file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / file.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(parse) = self.parse_time {
                            println!(
                                "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                                parse.human_duration().to_string(),
                                parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / parse.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(comp) = self.comp_time {
                            println!(
                                "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                                comp.human_duration().to_string(),
                                comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.ast_nodes as f64 / comp.as_secs_f64())
                                    .human_throughput(" node")
                                    .to_string()
                            );
                        }
                        if self.bottom_line {
                            println!("----------------");
                        }
                    }
                    // happy exit
                    pub fn finish(&mut self) {
                        self.bottom_line = false; // don't print a line after the report, everything went ok
                        self.print();
                    }
                }
                // give the report on drops too, in case of errors
                impl Drop for Reporter {
                    fn drop(&mut self) {
                        self.print();
                    }
                }
                let mut reporter = Reporter::new(timings);
                let input_name = input.path().display().to_string();
                let code = {
                    let start = Instant::now();
                    let code = std::io::read_to_string(&mut input)?;
                    reporter.file_time = Some(start.elapsed());
                    reporter.file_len = code.len();
                    code
                };
                let flags = Flags {
                    dbg_mangle: true,
                    all_move_metadata: true,
                    ..Flags::default()
                };
                let ink_ctx = inkwell::context::Context::create();
                let ctx = CompCtx::with_flags(&ink_ctx, &input_name, flags);
                let file = FILES.add_file(0, input_name, code.into());
                let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
                let mut ast = ast.unwrap_or_default();
                reporter.parse_time = Some(parse_time);
                reporter.ast_nodes = ast.nodes();
                ast.file = Some(file);
                for err in errs {
                    eprintln!("{:?}", Report::from(err).with_source_code(file));
                }
                ctx.module.set_triple(&TargetMachine::get_default_triple());
                let mut errs = vec![];
                let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
                reporter.comp_time = Some(comp_time);
                for err in errs {
                    eprintln!("{:?}", Report::from(err).with_source_code(file));
                }
                if let Err(msg) = ctx.module.verify() {
                    error!("\n{}", msg.to_string())
                }
                print!("{}", ctx.module.to_string());
                reporter.finish();
            }
        },
        Cli::Aot {
            mut input,
            output,
            linked,
            link_dirs,
            headers,
            triple,
            emit,
            profile,
            continue_if_err,
            debug_mangle,
            no_default_link,
            timings,
        } => {
            struct Reporter {
                timings: bool,
                reported: bool,
                bottom_line: bool,
                start: Instant,
                overall: Option<Duration>,
                file_time: Option<Duration>,
                file_len: usize,
                parse_time: Option<Duration>,
                ast_nodes: usize,
                comp_time: Option<Duration>,
                insts_before: usize,
                insts_after: usize,
                opt_time: Option<Duration>,
                cg_time: Option<Duration>,
                libs_time: Option<Duration>,
                nlibs: usize,
                cmd_time: Option<Duration>,
            }
            impl Reporter {
                pub fn new(timings: bool) -> Self {
                    Self {
                        timings,
                        reported: false,
                        bottom_line: true,
                        start: Instant::now(),
                        overall: None,
                        file_time: None,
                        file_len: 0,
                        parse_time: None,
                        ast_nodes: 0,
                        comp_time: None,
                        insts_before: 0,
                        insts_after: 0,
                        opt_time: None,
                        cg_time: None,
                        libs_time: None,
                        nlibs: 0,
                        cmd_time: None,
                    }
                }
                fn print(&mut self) {
                    if !self.timings {
                        return;
                    }
                    if self.reported {
                        return;
                    }
                    self.reported = true;
                    let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                    println!("----------------");
                    println!("overall time: {:>8}", overall.human_duration().to_string());
                    if let Some(file) = self.file_time {
                        println!(
                            "reading file: {:>8} ({:6.3}%) @ {:>13}",
                            file.human_duration().to_string(),
                            file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / file.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(parse) = self.parse_time {
                        println!(
                            "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                            parse.human_duration().to_string(),
                            parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / parse.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(comp) = self.comp_time {
                        println!(
                            "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                            comp.human_duration().to_string(),
                            comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.ast_nodes as f64 / comp.as_secs_f64())
                                .human_throughput(" node")
                                .to_string()
                        );
                    }
                    if let Some(opt) = self.opt_time {
                        println!(
                            "optimization: {:>8} ({:6.3}%) @ {:>13}",
                            opt.human_duration().to_string(),
                            opt.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.insts_before as f64 / opt.as_secs_f64())
                                .human_throughput(" inst")
                                .to_string()
                        );
                    }
                    if let Some(cg) = self.cg_time {
                        println!(
                            "output gen:   {:>8} ({:6.3}%) @ {:>13}",
                            cg.human_duration().to_string(),
                            cg.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.insts_after as f64 / cg.as_secs_f64())
                                .human_throughput(" inst")
                                .to_string()
                        );
                    }
                    if self.nlibs != 0 {
                        if let Some(libs) = self.libs_time {
                            println!(
                                "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                libs.human_duration().to_string(),
                                libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.nlibs as f64 / libs.as_secs_f64())
                                    .human_throughput(" libs")
                                    .to_string()
                            );
                        }
                    }
                    if let Some(cmd) = self.cmd_time {
                        println!(
                            "external cmd: {:>8} ({:6.3}%)",
                            cmd.human_duration().to_string(),
                            cmd.as_secs_f64() / overall.as_secs_f64() * 100.0
                        );
                    }
                    if self.bottom_line {
                        println!("----------------");
                    }
                }
                // happy exit
                pub fn finish(&mut self) {
                    self.bottom_line = false; // don't print a line after the report, everything went ok
                    self.print();
                }
            }
            // give the report on drops too, in case of errors
            impl Drop for Reporter {
                fn drop(&mut self) {
                    self.print();
                }
            }
            let mut reporter = Reporter::new(timings);
            let input_name = input.path().display().to_string();
            let code = {
                let start = Instant::now();
                let code = read_file(&mut input, &input_name)?;
                reporter.file_time = Some(start.elapsed());
                reporter.file_len = code.len();
                code
            };
            if triple.is_some() {
                Target::initialize_all(&INIT_NEEDED)
            } else {
                Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?
            }
            let (triple, trip) = triple.map_or_else(
                || {
                    let trip = TargetMachine::get_default_triple();
                    (trip.as_str().to_string_lossy().into_owned(), trip)
                },
                |triple| {
                    let trip = TargetTriple::create(&triple);
                    (triple, trip)
                },
            );
            let target_machine = Target::from_triple(&trip)
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
            let output = output.map(Ok).unwrap_or_else(|| {
                if input.is_std() {
                    Ok(OutputPath::std())
                } else {
                    let mut path = input.path().clone();
                    match emit {
                        OutputType::Executable => {
                            path.set_extension(if triple.contains("windows") {
                                "exe"
                            } else {
                                ""
                            });
                            false
                        }
                        OutputType::Library | OutputType::Archive => {
                            let (prefix, suffix) =
                                libs::lib_format_info(&triple, emit == OutputType::Library);
                            if prefix {
                                let mut name = OsString::from("lib");
                                name.push(path.file_stem().unwrap_or(OsStr::new("")));
                                if !suffix.is_empty() {
                                    name.push(".");
                                    name.push(suffix);
                                }
                                path.set_file_name(name);
                            } else {
                                path.set_extension(suffix);
                            }
                            false
                        }
                        OutputType::Llvm => path.set_extension("ll"),
                        OutputType::Bitcode => path.set_extension("bc"),
                        OutputType::Assembly => path.set_extension("s"),
                        OutputType::Object => path.set_extension("o"),
                        OutputType::RawObject => path.set_extension("raw.o"),
                        OutputType::HeaderObj => path.set_extension("coh.o"),
                        OutputType::Header => path.set_extension("coh"),
                    };
                    OutputPath::new(path)
                }
            })?;
            let mut flags = Flags {
                dbg_mangle: debug_mangle,
                ..Flags::default()
            };
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx
                .ptr_sized_int_type(&target_machine.get_target_data(), None)
                .size_of()
                .get_zero_extended_constant()
            {
                flags.word_size = size as u16;
            }
            let ctx = CompCtx::with_flags(&ink_ctx, &input_name, flags);
            ctx.module.set_triple(&trip);
            let mut cc = cc::CompileCommand::new();
            cc.target(&triple);
            cc.link_dirs(link_dirs);
            cc.no_default_link = no_default_link;
            {
                reporter.nlibs = linked.len();
                let start = Instant::now();
                let notfound = cc.search_libs(linked, Some(&ctx), false)?;
                if !notfound.is_empty() {
                    anyhow::bail!(LibsNotFound(notfound))
                }
                for head in headers {
                    let mut file = BufReader::new(std::fs::File::open(head)?);
                    ctx.load(&mut file)?;
                }
                reporter.libs_time = Some(start.elapsed());
            };
            let mut fail = false;
            let mut overall_fail = false;
            let file = FILES.add_file(0, input.to_string(), code.into());
            let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
            let mut ast = ast.unwrap_or_default();
            reporter.parse_time = Some(parse_time);
            reporter.ast_nodes = ast.nodes();
            ast.file = Some(file);
            let mut ec = 0;
            for err in errs {
                let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                if is_err {
                    fail = true;
                    ec += 1;
                }
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {
                anyhow::bail!(CompileErrors(ec))
            }
            let mut errs = vec![];
            let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
            reporter.comp_time = Some(comp_time);
            overall_fail |= fail;
            fail = false;
            for err in errs {
                fail |= err.is_err();
                ec += err.is_err() as usize;
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            if fail && !continue_if_err {
                anyhow::bail!(CompileErrors(ec))
            }
            ctx.module.verify().map_err(LlvmVerifierError::from)?;
            if fail || overall_fail {
                anyhow::bail!(CompileErrors(ec))
            }
            reporter.insts_before = insts(&ctx.module);
            reporter.opt_time = Some(
                try_timeit(|| {
                    ctx.module
                        .run_passes(
                            &opt::expand_pass_string(profile.as_deref().unwrap_or("@default"))?,
                            &target_machine,
                            PassBuilderOptions::create(),
                        )
                        .map_err(opt::PassError::from_llvm)?;
                    anyhow::Ok(())
                })?
                .1,
            );
            reporter.insts_after = insts(&ctx.module);
            match emit {
                OutputType::Header => {
                    let mut buf = vec![];
                    reporter.cg_time = Some(try_timeit(|| ctx.save(&mut buf))?.1);
                    output.create_with_len(buf.len() as _)?.write_all(&buf)?
                }
                OutputType::HeaderObj => {
                    let mut obj = libs::new_object(&triple);
                    let (vec, cg_time) = try_timeit(|| {
                        libs::populate_header(&mut obj, &ctx);
                        obj.write()
                    })?;
                    reporter.cg_time = Some(cg_time);
                    output.create_with_len(vec.len() as _)?.write_all(&vec)?
                }
                OutputType::Llvm => {
                    let (m, cg_time) = timeit(|| ctx.module.to_string());
                    reporter.cg_time = Some(cg_time);
                    output
                        .create_with_len(m.len() as _)?
                        .write_all(m.as_bytes())?
                }
                OutputType::Bitcode => {
                    let (m, cg_time) = timeit(|| ctx.module.write_bitcode_to_memory());
                    reporter.cg_time = Some(cg_time);
                    output
                        .create_with_len(m.get_size() as _)?
                        .write_all(m.as_slice())?
                }
                OutputType::Assembly => {
                    let (m, cg_time) = timeit(|| {
                        target_machine
                            .write_to_memory_buffer(
                                &ctx.module,
                                inkwell::targets::FileType::Assembly,
                            )
                            .unwrap()
                    });
                    reporter.cg_time = Some(cg_time);
                    output
                        .create_with_len(m.get_size() as _)?
                        .write_all(m.as_slice())?
                }
                _ => {
                    let (mb, cg_time) = timeit(|| {
                        target_machine
                            .write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object)
                            .unwrap()
                    });
                    reporter.cg_time = Some(cg_time);
                    match emit {
                        OutputType::Executable => {
                            if !output.is_local() {
                                eprintln!("cannot output executable to stdout");
                                Err(Exit(4))?;
                            }
                            let tmp = temp_file::with_contents(mb.as_slice());
                            cc.obj(tmp.path());
                            cc.output(&**output.path());
                            let cmd_time = try_timeit(|| cc.run())?.1;
                            reporter.cmd_time = Some(cmd_time);
                        }
                        OutputType::Library => {
                            if !output.is_local() {
                                eprintln!("cannot output executable to stdout");
                                Err(Exit(4))?;
                            }
                            let mut obj = libs::new_object(&triple);
                            libs::populate_header(&mut obj, &ctx);
                            let tmp1 = temp_file::with_contents(&obj.write()?);
                            let tmp2 = temp_file::with_contents(mb.as_slice());
                            cc.lib(true);
                            cc.objs([tmp1.path(), tmp2.path()]);
                            cc.output(&**output.path());
                            let cmd_time = try_timeit(|| cc.run())?.1;
                            reporter.cmd_time = Some(cmd_time);
                        }
                        OutputType::Archive => {
                            let mut builder = ar::Builder::new(output.create()?);
                            let slice = mb.as_slice();

                            builder.append(
                                &ar::Header::new(
                                    input.path().with_extension("o").into_raw_vec(),
                                    slice.len() as _,
                                ),
                                slice,
                            )?;
                            let mut obj = libs::new_object(&triple);
                            libs::populate_header(&mut obj, &ctx);
                            let out = obj.write()?;
                            builder.append(
                                &ar::Header::new(b".colib.o".to_vec(), out.len() as _),
                                out.as_slice(),
                            )?;
                        }
                        OutputType::RawObject => output
                            .create_with_len(mb.get_size() as _)?
                            .write_all(mb.as_slice())?,
                        OutputType::Object => {
                            let parsed_llvm_object = object::read::File::parse(mb.as_slice())?;
                            let mut writeable_object =
                                obj::get_writeable_object_from_file(parsed_llvm_object);
                            libs::populate_header(&mut writeable_object, &ctx);
                            let buf = writeable_object.write()?;
                            output.create_with_len(buf.len() as _)?.write_all(&buf)?
                        }
                        x => unreachable!("{x:?} has already been handled"),
                    };
                }
            }
            reporter.finish();
        }
        Cli::Jit {
            mut input,
            linked,
            link_dirs,
            headers,
            profile,
            continue_if_err,
            no_default_link,
            timings,
            this,
            mut args,
        } => {
            struct Reporter {
                timings: bool,
                reported: bool,
                start: Instant,
                overall: Option<Duration>,
                file_time: Option<Duration>,
                file_len: usize,
                parse_time: Option<Duration>,
                ast_nodes: usize,
                comp_time: Option<Duration>,
                insts_before: usize,
                opt_time: Option<Duration>,
                libs_time: Option<Duration>,
                nlibs: usize,
            }
            impl Reporter {
                pub fn new(timings: bool) -> Self {
                    Self {
                        timings,
                        reported: false,
                        start: Instant::now(),
                        overall: None,
                        file_time: None,
                        file_len: 0,
                        parse_time: None,
                        ast_nodes: 0,
                        comp_time: None,
                        insts_before: 0,
                        opt_time: None,
                        libs_time: None,
                        nlibs: 0,
                    }
                }
                fn print(&mut self) {
                    if !self.timings {
                        return;
                    }
                    if self.reported {
                        return;
                    }
                    self.reported = true;
                    let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                    println!("----------------");
                    println!("overall time: {:>8}", overall.human_duration().to_string());
                    if let Some(file) = self.file_time {
                        println!(
                            "reading file: {:>8} ({:6.3}%) @ {:>13}",
                            file.human_duration().to_string(),
                            file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / file.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(parse) = self.parse_time {
                        println!(
                            "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                            parse.human_duration().to_string(),
                            parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / parse.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(comp) = self.comp_time {
                        println!(
                            "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                            comp.human_duration().to_string(),
                            comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.ast_nodes as f64 / comp.as_secs_f64())
                                .human_throughput(" node")
                                .to_string()
                        );
                    }
                    if let Some(opt) = self.opt_time {
                        println!(
                            "optimization: {:>8} ({:6.3}%) @ {:>13}",
                            opt.human_duration().to_string(),
                            opt.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.insts_before as f64 / opt.as_secs_f64())
                                .human_throughput(" inst")
                                .to_string()
                        );
                    }
                    if self.nlibs != 0 {
                        if let Some(libs) = self.libs_time {
                            println!(
                                "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                libs.human_duration().to_string(),
                                libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.nlibs as f64 / libs.as_secs_f64())
                                    .human_throughput(" libs")
                                    .to_string()
                            );
                        }
                    }
                }
                // happy exit
                pub fn finish(&mut self) {
                    self.print();
                }
            }
            // give the report on drops too, in case of errors
            impl Drop for Reporter {
                fn drop(&mut self) {
                    self.print();
                }
            }
            let mut reporter = Reporter::new(timings);
            let input_name = input.path().display().to_string();
            let code = {
                let start = Instant::now();
                let code = read_file(&mut input, &input_name)?;
                reporter.file_time = Some(start.elapsed());
                reporter.file_len = code.len();
                code
            };
            args.insert(
                0,
                this.unwrap_or_else(|| {
                    std::env::args()
                        .next()
                        .unwrap_or_else(|| "<error>".to_string())
                        + " jit"
                }),
            );
            Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
            let trip = TargetMachine::get_default_triple();
            let target_machine = Target::from_triple(&trip)
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
            let ink_ctx = inkwell::context::Context::create();
            let mut ctx = CompCtx::new(&ink_ctx, &input_name);
            ctx.flags.dbg_mangle = true;
            ctx.module.set_triple(&trip);
            let mut cc = cc::CompileCommand::new();
            cc.link_dirs(link_dirs);
            cc.no_default_link = no_default_link;
            {
                reporter.nlibs = linked.len();
                let start = Instant::now();
                let notfound = cc.search_libs(linked, Some(&ctx), true)?;
                if !notfound.is_empty() {
                    anyhow::bail!(LibsNotFound(notfound))
                }
                for head in headers {
                    let mut file = BufReader::new(std::fs::File::open(head)?);
                    ctx.load(&mut file)?;
                }
                reporter.libs_time = Some(start.elapsed());
            };
            let mut fail = false;
            let mut overall_fail = false;
            let file = FILES.add_file(0, input_name, code.into());
            let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
            let mut ast = ast.unwrap_or_default();
            reporter.parse_time = Some(parse_time);
            reporter.ast_nodes = ast.nodes();
            ast.file = Some(file);
            let mut ec = 0;
            for err in errs {
                let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                if is_err {
                    fail = true;
                    ec += 1;
                }
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {
                anyhow::bail!(CompileErrors(ec))
            }
            let mut errs = vec![];
            let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
            reporter.comp_time = Some(comp_time);
            overall_fail |= fail;
            fail = false;
            for err in errs {
                fail |= err.is_err();
                ec += err.is_err() as usize;
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            if fail && !continue_if_err {
                anyhow::bail!(CompileErrors(ec))
            }
            if let Err(msg) = ctx.module.verify() {
                error!("\n{}", msg.to_string());
                Err(Exit(101))?
            }
            if fail || overall_fail {
                anyhow::bail!(CompileErrors(ec))
            }
            reporter.insts_before = insts(&ctx.module);
            reporter.opt_time = Some(
                try_timeit(|| {
                    ctx.module
                        .run_passes(
                            &opt::expand_pass_string(profile.as_deref().unwrap_or("@default"))?,
                            &target_machine,
                            PassBuilderOptions::create(),
                        )
                        .map_err(opt::PassError::from_llvm)?;
                    anyhow::Ok(())
                })?
                .1,
            );
            let ee = ctx
                .module
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .expect("Couldn't create execution engine!");
            unsafe {
                let main_fn = match ee.get_function_value("main") {
                    Ok(main_fn) => main_fn,
                    Err(FunctionLookupError::JITNotEnabled) => panic!("JIT not enabled here"),
                    Err(FunctionLookupError::FunctionNotFound) => {
                        eprintln!("couldn't find symbol 'main'");
                        Err(Exit(255))?
                    }
                };
                reporter.finish();
                ee.run_static_constructors();
                let ec = ee.run_function_as_main(
                    main_fn,
                    &args.iter().map(String::as_str).collect::<Vec<_>>(),
                );
                ee.run_static_destructors();
                if ec != 0 {
                    Err(Exit(ec))?
                }
            }
        }
        Cli::Check {
            mut input,
            linked,
            link_dirs,
            headers,
            no_default_link,
            timings,
            dump_header,
        } => {
            struct Reporter {
                timings: bool,
                reported: bool,
                bottom_line: bool,
                start: Instant,
                overall: Option<Duration>,
                file_time: Option<Duration>,
                file_len: usize,
                parse_time: Option<Duration>,
                ast_nodes: usize,
                comp_time: Option<Duration>,
                libs_time: Option<Duration>,
                nlibs: usize,
            }
            impl Reporter {
                pub fn new(timings: bool) -> Self {
                    Self {
                        timings,
                        reported: false,
                        bottom_line: true,
                        start: Instant::now(),
                        overall: None,
                        file_time: None,
                        file_len: 0,
                        parse_time: None,
                        ast_nodes: 0,
                        comp_time: None,
                        libs_time: None,
                        nlibs: 0,
                    }
                }
                fn print(&mut self) {
                    if !self.timings {
                        return;
                    }
                    if self.reported {
                        return;
                    }
                    self.reported = true;
                    let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                    println!("----------------");
                    println!("overall time: {:>8}", overall.human_duration().to_string());
                    if let Some(file) = self.file_time {
                        println!(
                            "reading file: {:>8} ({:6.3}%) @ {:>13}",
                            file.human_duration().to_string(),
                            file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / file.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(parse) = self.parse_time {
                        println!(
                            "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                            parse.human_duration().to_string(),
                            parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.file_len as f64 / parse.as_secs_f64())
                                .human_throughput_bytes()
                                .to_string()
                        );
                    }
                    if let Some(comp) = self.comp_time {
                        println!(
                            "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                            comp.human_duration().to_string(),
                            comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                            (self.ast_nodes as f64 / comp.as_secs_f64())
                                .human_throughput(" node")
                                .to_string()
                        );
                    }
                    if self.nlibs != 0 {
                        if let Some(libs) = self.libs_time {
                            println!(
                                "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                libs.human_duration().to_string(),
                                libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.nlibs as f64 / libs.as_secs_f64())
                                    .human_throughput(" libs")
                                    .to_string()
                            );
                        }
                    }
                    if self.bottom_line {
                        println!("----------------");
                    }
                }
                // happy exit
                pub fn finish(&mut self) {
                    self.bottom_line = false; // don't print a line after the report, everything went ok
                    self.print();
                }
            }
            // give the report on drops too, in case of errors
            impl Drop for Reporter {
                fn drop(&mut self) {
                    self.print();
                }
            }
            let mut reporter = Reporter::new(timings);
            let input_name = input.path().display().to_string();
            let code = {
                let start = Instant::now();
                let code = read_file(&mut input, &input_name)?;
                reporter.file_time = Some(start.elapsed());
                reporter.file_len = code.len();
                code
            };
            Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
            let trip = TargetMachine::get_default_triple();
            let triple = trip.as_str().to_string_lossy();
            let target_machine = Target::from_triple(&trip)
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
            let mut flags = Flags {
                dbg_mangle: true,
                ..Flags::default()
            };
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx
                .ptr_sized_int_type(&target_machine.get_target_data(), None)
                .size_of()
                .get_zero_extended_constant()
            {
                flags.word_size = size as u16;
            }
            flags.add_type_map = dump_header;
            let ctx = CompCtx::with_flags(&ink_ctx, &input_name, flags);
            ctx.module.set_triple(&trip);
            let mut cc = cc::CompileCommand::new();
            cc.target(&*triple);
            cc.link_dirs(link_dirs);
            cc.no_default_link = no_default_link;
            {
                reporter.nlibs = linked.len();
                let start = Instant::now();
                let notfound = cc.search_libs(linked, Some(&ctx), false)?;
                if !notfound.is_empty() {
                    anyhow::bail!(LibsNotFound(notfound))
                }
                for head in headers {
                    let mut file = BufReader::new(std::fs::File::open(head)?);
                    ctx.load(&mut file)?;
                }
                reporter.libs_time = Some(start.elapsed());
            };
            let mut fail = false;
            let file = FILES.add_file(0, input_name, code.into());
            let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
            let mut ast = ast.unwrap_or_default();
            reporter.parse_time = Some(parse_time);
            reporter.ast_nodes = ast.nodes();
            ast.file = Some(file);
            let mut ec = 0;
            for err in errs {
                let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                if is_err {
                    ec += 1;
                    fail = true;
                }
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            let mut errs = vec![];
            let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
            reporter.comp_time = Some(comp_time);
            for err in errs {
                fail |= err.is_err();
                ec += err.is_err() as usize;
                eprintln!("{:?}", Report::from(err).with_source_code(file));
            }
            ctx.module.verify().map_err(LlvmVerifierError::from)?;
            if fail {
                anyhow::bail!(CompileErrors(ec))
            }
            if dump_header {
                serde_json::to_writer_pretty(std::io::stdout(), &ctx)?;
            }
            reporter.finish();
        }
        Cli::Multi(cmd) => match cmd {
            MultiSubcommand::Aot {
                mut inputs,
                output,
                linked,
                link_dirs,
                headers,
                triple,
                emit,
                profile,
                debug_mangle,
                no_default_link,
                timings,
            } => {
                struct Reporter {
                    timings: bool,
                    reported: bool,
                    bottom_line: bool,
                    start: Instant,
                    overall: Option<Duration>,
                    file_time: Option<Duration>,
                    file_len: usize,
                    parse_time: Option<Duration>,
                    ast_nodes: usize,
                    comp_time: Option<Duration>,
                    insts_before: usize,
                    insts_after: usize,
                    opt_time: Option<Duration>,
                    cg_time: Option<Duration>,
                    libs_time: Option<Duration>,
                    nlibs: usize,
                    cmd_time: Option<Duration>,
                }
                impl Reporter {
                    pub fn new(timings: bool) -> Self {
                        Self {
                            timings,
                            reported: false,
                            bottom_line: true,
                            start: Instant::now(),
                            overall: None,
                            file_time: None,
                            file_len: 0,
                            parse_time: None,
                            ast_nodes: 0,
                            comp_time: None,
                            insts_before: 0,
                            insts_after: 0,
                            opt_time: None,
                            cg_time: None,
                            libs_time: None,
                            nlibs: 0,
                            cmd_time: None,
                        }
                    }
                    fn print(&mut self) {
                        if !self.timings {
                            return;
                        }
                        if self.reported {
                            return;
                        }
                        self.reported = true;
                        let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                        println!("----------------");
                        println!("overall time: {:>8}", overall.human_duration().to_string());
                        if let Some(file) = self.file_time {
                            println!(
                                "reading file: {:>8} ({:6.3}%) @ {:>13}",
                                file.human_duration().to_string(),
                                file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / file.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(parse) = self.parse_time {
                            println!(
                                "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                                parse.human_duration().to_string(),
                                parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / parse.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(comp) = self.comp_time {
                            println!(
                                "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                                comp.human_duration().to_string(),
                                comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.ast_nodes as f64 / comp.as_secs_f64())
                                    .human_throughput(" node")
                                    .to_string()
                            );
                        }
                        if let Some(opt) = self.opt_time {
                            println!(
                                "optimization: {:>8} ({:6.3}%) @ {:>13}",
                                opt.human_duration().to_string(),
                                opt.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_before as f64 / opt.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if let Some(cg) = self.cg_time {
                            println!(
                                "output gen:   {:>8} ({:6.3}%) @ {:>13}",
                                cg.human_duration().to_string(),
                                cg.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_after as f64 / cg.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if self.nlibs != 0 {
                            if let Some(libs) = self.libs_time {
                                println!(
                                    "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                    libs.human_duration().to_string(),
                                    libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                    (self.nlibs as f64 / libs.as_secs_f64())
                                        .human_throughput(" libs")
                                        .to_string()
                                );
                            }
                        }
                        if let Some(cmd) = self.cmd_time {
                            println!(
                                "external cmd: {:>8} ({:6.3}%)",
                                cmd.human_duration().to_string(),
                                cmd.as_secs_f64() / overall.as_secs_f64() * 100.0
                            );
                        }
                        if self.bottom_line {
                            println!("----------------");
                        }
                    }
                    // happy exit
                    pub fn finish(&mut self) {
                        self.bottom_line = false; // don't print a line after the report, everything went ok
                        self.print();
                    }
                }
                // give the report on drops too, in case of errors
                impl Drop for Reporter {
                    fn drop(&mut self) {
                        self.print();
                    }
                }
                let mut reporter = Reporter::new(timings);
                let codes = inputs
                    .iter_mut()
                    .map(|input| {
                        let start = Instant::now();
                        let code = read_file(input, &input.path().display().to_string())?;
                        reporter.file_time = Some(start.elapsed());
                        reporter.file_len = code.len();
                        anyhow::Ok(code)
                    })
                    .collect::<anyhow::Result<Vec<String>>>()?;
                if triple.is_some() {
                    Target::initialize_all(&INIT_NEEDED)
                } else {
                    Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?
                }
                let (triple, trip) = triple.map_or_else(
                    || {
                        let trip = TargetMachine::get_default_triple();
                        (trip.as_str().to_string_lossy().into_owned(), trip)
                    },
                    |triple| {
                        let trip = TargetTriple::create(&triple);
                        (triple, trip)
                    },
                );
                let target_machine = Target::from_triple(&trip)
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
                let mut flags = Flags {
                    dbg_mangle: debug_mangle,
                    prepass: false,
                    private_syms: false,
                    ..Flags::default()
                };
                let ink_ctx = inkwell::context::Context::create();
                if let Some(size) = ink_ctx
                    .ptr_sized_int_type(&target_machine.get_target_data(), None)
                    .size_of()
                    .get_zero_extended_constant()
                {
                    flags.word_size = size as u16;
                }
                let ctx = CompCtx::with_flags(&ink_ctx, "base-module", flags);
                ctx.module.set_triple(&trip);
                let mut cc = cc::CompileCommand::new();
                cc.target(&triple);
                cc.link_dirs(link_dirs);
                cc.no_default_link = no_default_link;
                {
                    reporter.nlibs = linked.len();
                    let start = Instant::now();
                    let notfound = cc.search_libs(linked, Some(&ctx), false)?;
                    if !notfound.is_empty() {
                        anyhow::bail!(LibsNotFound(notfound))
                    }
                    for head in headers {
                        let mut file = BufReader::new(std::fs::File::open(head)?);
                        ctx.load(&mut file)?;
                    }
                    reporter.libs_time = Some(start.elapsed());
                };
                let mut fail = false;
                let mut ec = 0;
                let asts = inputs
                    .iter()
                    .zip(&codes)
                    .map(|(input, code)| {
                        let file = FILES.add_file(
                            0,
                            input.path().display().to_string(),
                            code.clone().into(),
                        );
                        let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
                        let mut ast = ast.unwrap_or_default();
                        *reporter.parse_time.get_or_insert(Duration::ZERO) += parse_time;
                        reporter.ast_nodes += ast.nodes();
                        ast.file = Some(file);
                        for err in errs {
                            let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                            if is_err {
                                ec += 1;
                                fail = true;
                            }
                            eprintln!("{:?}", Report::from(err).with_source_code(file));
                        }
                        ast.run_passes(&ctx);
                        anyhow::Ok(ast)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                #[derive(Debug, Clone)]
                enum TmpFile {
                    Zero,
                    One(temp_file::TempFile),
                    Two(temp_file::TempFile, temp_file::TempFile),
                }
                let mut archive = (emit == OutputType::Archive)
                    .then(|| output.clone().map(|o| o.create().map(ar::Builder::new)))
                    .flatten()
                    .transpose()?;
                let pass_str = opt::expand_pass_string(profile.as_deref().unwrap_or("@default"))?;
                let _tmps = asts
                    .iter()
                    .map(|ast| {
                        let file = ast.file.unwrap();
                        ctx.module.set_name(file.name());
                        ctx.module.set_source_file_name(file.name());
                        let mut errs = vec![];
                        let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
                        *reporter.comp_time.get_or_insert(Duration::ZERO) += comp_time;
                        for err in errs {
                            fail |= err.is_err();
                            eprintln!(
                                "{:?}",
                                Report::from(err).with_source_code(ast.file.unwrap())
                            );
                        }
                        ctx.module.verify().map_err(LlvmVerifierError::from)?;
                        if fail {
                            anyhow::bail!(CompileErrors(ec))
                        }
                        reporter.insts_before += insts(&ctx.module);
                        *reporter.opt_time.get_or_insert(Duration::ZERO) += try_timeit(|| {
                            ctx.module
                                .run_passes(
                                    &pass_str,
                                    &target_machine,
                                    PassBuilderOptions::create(),
                                )
                                .map_err(opt::PassError::from_llvm)?;
                            anyhow::Ok(())
                        })?
                        .1;
                        reporter.insts_after += insts(&ctx.module);
                        let input = PathBuf::from(file.name());
                        let mut out = match &output {
                            None => ClioPath::new(input)?,
                            Some(p) => {
                                let mut p = p.clone();
                                p.push(input);
                                p
                            }
                        };
                        if !matches!(
                            emit,
                            OutputType::Executable | OutputType::Library | OutputType::Archive
                        ) {
                            let parent = out.parent().unwrap().parent().unwrap(); // why
                            if !parent.exists() {
                                parent.create_dir_all_anyhow()?
                            }
                        }
                        use TmpFile::*;
                        let tmps = match emit {
                            OutputType::Header => {
                                out.set_extension("coh");
                                let mut buf = vec![];
                                *reporter.cg_time.get_or_insert(Duration::ZERO) +=
                                    try_timeit(|| ctx.save(&mut buf))?.1;
                                out.create_with_len(buf.len() as _)?.write_all(&buf)?;
                                Zero
                            }
                            OutputType::HeaderObj => {
                                out.set_extension("coh.o");
                                let mut obj = libs::new_object(&triple);
                                let (vec, cg_time) = try_timeit(|| {
                                    libs::populate_header(&mut obj, &ctx);
                                    obj.write()
                                })?;
                                *reporter.cg_time.get_or_insert(Duration::ZERO) += cg_time;
                                out.create_with_len(vec.len() as _)?.write_all(&vec)?;
                                Zero
                            }
                            OutputType::Llvm => {
                                out.set_extension("ll");
                                let (m, cg_time) = timeit(|| ctx.module.to_string());
                                *reporter.cg_time.get_or_insert(Duration::ZERO) += cg_time;
                                out.create_with_len(m.len() as _)?.write_all(m.as_bytes())?;
                                Zero
                            }
                            OutputType::Bitcode => {
                                out.set_extension("bc");
                                let (m, cg_time) = timeit(|| ctx.module.write_bitcode_to_memory());
                                *reporter.cg_time.get_or_insert(Duration::ZERO) += cg_time;
                                out.create_with_len(m.get_size() as _)?
                                    .write_all(m.as_slice())?;
                                Zero
                            }
                            OutputType::Assembly => {
                                out.set_extension("s");
                                let (m, cg_time) = timeit(|| {
                                    target_machine
                                        .write_to_memory_buffer(
                                            &ctx.module,
                                            inkwell::targets::FileType::Assembly,
                                        )
                                        .unwrap()
                                });
                                *reporter.cg_time.get_or_insert(Duration::ZERO) += cg_time;
                                out.create_with_len(m.get_size() as _)?
                                    .write_all(m.as_slice())?;
                                Zero
                            }
                            _ => {
                                let (mb, cg_time) = timeit(|| {
                                    target_machine
                                        .write_to_memory_buffer(
                                            &ctx.module,
                                            inkwell::targets::FileType::Object,
                                        )
                                        .unwrap()
                                });
                                *reporter.cg_time.get_or_insert(Duration::ZERO) += cg_time;
                                match emit {
                                    OutputType::Executable => {
                                        if output.is_none() {
                                            eprintln!("cannot output executable to stdout");
                                            Err(Exit(4))?;
                                        }
                                        let tmp = temp_file::with_contents(mb.as_slice());
                                        cc.obj(tmp.path());
                                        One(tmp)
                                    }
                                    OutputType::Library => {
                                        let mut obj = libs::new_object(&triple);
                                        libs::populate_header(&mut obj, &ctx);
                                        let tmp1 = temp_file::with_contents(&obj.write()?);
                                        let tmp2 = temp_file::with_contents(mb.as_slice());
                                        cc.lib(true);
                                        cc.objs([tmp1.path(), tmp2.path()]);
                                        Two(tmp1, tmp2)
                                    }
                                    OutputType::Archive => {
                                        out.set_extension("o");
                                        archive.as_mut().unwrap().append(
                                            &ar::Header::new(
                                                out.to_os_string().into_raw_vec(),
                                                mb.get_size() as _,
                                            ),
                                            mb.as_slice(),
                                        )?;
                                        Zero
                                    }
                                    OutputType::RawObject => {
                                        out.set_extension("raw.o");
                                        out.create_with_len(mb.get_size() as _)?
                                            .write_all(mb.as_slice())?;
                                        Zero
                                    }
                                    OutputType::Object => {
                                        out.set_extension("o");
                                        let parsed_llvm_object =
                                            object::read::File::parse(mb.as_slice())?;
                                        let mut writeable_object =
                                            obj::get_writeable_object_from_file(parsed_llvm_object);
                                        libs::populate_header(&mut writeable_object, &ctx);
                                        let buf = writeable_object.write()?;
                                        out.create_with_len(buf.len() as _)?.write_all(&buf)?;
                                        Zero
                                    }
                                    x => unreachable!("{x:?} has already been handled"),
                                }
                            }
                        };
                        ctx.with_vars(|v| clear_mod(&mut v.symbols));
                        anyhow::Ok(tmps)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                if emit == OutputType::Archive {
                    let mut obj = libs::new_object(&triple);
                    libs::populate_header(&mut obj, &ctx);
                    let buf = obj.write()?;
                    archive.as_mut().unwrap().append(
                        &ar::Header::new(b".colib.o".to_vec(), buf.len() as _),
                        &buf[..],
                    )?;
                } else if matches!(emit, OutputType::Executable | OutputType::Library) {
                    let is_lib = emit == OutputType::Library;
                    cc.output(
                        output
                            .ok_or(anyhow::anyhow!(
                                "output file must be specified for multi-{}s",
                                if is_lib { "lib" } else { "exe" }
                            ))?
                            .path(),
                    );
                    cc.lib(is_lib);
                    let cmd_time = try_timeit(|| cc.run())?.1;
                    reporter.cmd_time = Some(cmd_time);
                }
                reporter.finish();
            }
            MultiSubcommand::Jit {
                mut inputs,
                linked,
                link_dirs,
                headers,
                profile,
                no_default_link,
                timings,
                this,
                mut args,
            } => {
                struct Reporter {
                    timings: bool,
                    reported: bool,
                    bottom_line: bool,
                    start: Instant,
                    overall: Option<Duration>,
                    file_time: Option<Duration>,
                    file_len: usize,
                    parse_time: Option<Duration>,
                    ast_nodes: usize,
                    comp_time: Option<Duration>,
                    insts_before: usize,
                    insts_after: usize,
                    opt_time: Option<Duration>,
                    cg_time: Option<Duration>,
                    libs_time: Option<Duration>,
                    nlibs: usize,
                }
                impl Reporter {
                    pub fn new(timings: bool) -> Self {
                        Self {
                            timings,
                            reported: false,
                            bottom_line: true,
                            start: Instant::now(),
                            overall: None,
                            file_time: None,
                            file_len: 0,
                            parse_time: None,
                            ast_nodes: 0,
                            comp_time: None,
                            insts_before: 0,
                            insts_after: 0,
                            opt_time: None,
                            cg_time: None,
                            libs_time: None,
                            nlibs: 0,
                        }
                    }
                    fn print(&mut self) {
                        if !self.timings {
                            return;
                        }
                        if self.reported {
                            return;
                        }
                        self.reported = true;
                        let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                        println!("----------------");
                        println!("overall time: {:>8}", overall.human_duration().to_string());
                        if let Some(file) = self.file_time {
                            println!(
                                "reading file: {:>8} ({:6.3}%) @ {:>13}",
                                file.human_duration().to_string(),
                                file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / file.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(parse) = self.parse_time {
                            println!(
                                "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                                parse.human_duration().to_string(),
                                parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / parse.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(comp) = self.comp_time {
                            println!(
                                "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                                comp.human_duration().to_string(),
                                comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.ast_nodes as f64 / comp.as_secs_f64())
                                    .human_throughput(" node")
                                    .to_string()
                            );
                        }
                        if let Some(opt) = self.opt_time {
                            println!(
                                "optimization: {:>8} ({:6.3}%) @ {:>13}",
                                opt.human_duration().to_string(),
                                opt.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_before as f64 / opt.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if let Some(cg) = self.cg_time {
                            println!(
                                "output gen:   {:>8} ({:6.3}%) @ {:>13}",
                                cg.human_duration().to_string(),
                                cg.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_after as f64 / cg.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if self.nlibs != 0 {
                            if let Some(libs) = self.libs_time {
                                println!(
                                    "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                    libs.human_duration().to_string(),
                                    libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                    (self.nlibs as f64 / libs.as_secs_f64())
                                        .human_throughput(" libs")
                                        .to_string()
                                );
                            }
                        }
                        if self.bottom_line {
                            println!("----------------");
                        }
                    }
                    // happy exit
                    pub fn finish(&mut self) {
                        self.bottom_line = false; // don't print a line after the report, everything went ok
                        self.print();
                    }
                }
                // give the report on drops too, in case of errors
                impl Drop for Reporter {
                    fn drop(&mut self) {
                        self.print();
                    }
                }
                let mut reporter = Reporter::new(timings);
                let codes = inputs
                    .iter_mut()
                    .map(|input| {
                        let start = Instant::now();
                        let code = read_file(input, &input.path().display().to_string())?;
                        reporter.file_time = Some(start.elapsed());
                        reporter.file_len = code.len();
                        anyhow::Ok(code)
                    })
                    .collect::<anyhow::Result<Vec<String>>>()?;
                args.insert(
                    0,
                    this.unwrap_or_else(|| {
                        std::env::args()
                            .next()
                            .unwrap_or_else(|| "<error>".to_string())
                            + " jit"
                    }),
                );
                Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
                let trip = TargetMachine::get_default_triple();
                let triple = trip.as_str().to_string_lossy().into_owned();
                let target_machine = Target::from_triple(&trip)
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
                let mut flags = Flags {
                    dbg_mangle: true,
                    prepass: false,
                    ..Flags::default()
                };
                let ink_ctx = inkwell::context::Context::create();
                if let Some(size) = ink_ctx
                    .ptr_sized_int_type(&target_machine.get_target_data(), None)
                    .size_of()
                    .get_zero_extended_constant()
                {
                    flags.word_size = size as u16;
                }
                let ctx = CompCtx::with_flags(&ink_ctx, "multi-jit", flags);
                ctx.module.set_triple(&trip);
                let mut cc = cc::CompileCommand::new();
                cc.target(&triple);
                cc.link_dirs(link_dirs);
                cc.no_default_link = no_default_link;
                {
                    reporter.nlibs = linked.len();
                    let start = Instant::now();
                    let notfound = cc.search_libs(linked, Some(&ctx), true)?;
                    if !notfound.is_empty() {
                        anyhow::bail!(LibsNotFound(notfound))
                    }
                    for head in headers {
                        let mut file = BufReader::new(std::fs::File::open(head)?);
                        ctx.load(&mut file)?;
                    }
                    reporter.libs_time = Some(start.elapsed());
                };
                let mut fail = false;
                let mut ec = 0;
                let asts = inputs
                    .iter()
                    .zip(&codes)
                    .map(|(input, code)| {
                        let file = FILES.add_file(
                            0,
                            input.path().display().to_string(),
                            code.clone().into(),
                        );
                        let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
                        let mut ast = ast.unwrap_or_default();
                        *reporter.parse_time.get_or_insert(Duration::ZERO) += parse_time;
                        reporter.ast_nodes += ast.nodes();
                        ast.file = Some(file);
                        for err in errs {
                            let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                            if is_err {
                                ec += 1;
                                fail = true;
                            }
                            eprintln!("{:?}", Report::from(err).with_source_code(file));
                        }
                        ast.run_passes(&ctx);
                        anyhow::Ok(ast)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let mut errs = vec![];
                asts.iter().for_each(|ast| {
                    let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
                    *reporter.comp_time.get_or_insert(Duration::ZERO) += comp_time;
                    for err in errs.drain(..) {
                        fail |= err.is_err();
                        ec += err.is_err() as usize;
                        eprintln!(
                            "{:?}",
                            Report::from(err).with_source_code(ast.file.unwrap())
                        );
                    }
                });
                ctx.module.verify().map_err(LlvmVerifierError::from)?;
                reporter.insts_before = insts(&ctx.module);
                *reporter.opt_time.get_or_insert(Duration::ZERO) += try_timeit(|| {
                    reporter.insts_before += insts(&ctx.module);
                    ctx.module
                        .run_passes(
                            &opt::expand_pass_string(profile.as_deref().unwrap_or("@default"))?,
                            &target_machine,
                            PassBuilderOptions::create(),
                        )
                        .map_err(opt::PassError::from_llvm)?;
                    anyhow::Ok(())
                })?
                .1;
                let ee = ctx
                    .module
                    .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                    .map_err(|m| anyhow::Error::msg(m.to_string()))?;
                if fail {
                    anyhow::bail!(CompileErrors(ec))
                }
                reporter.finish();
                unsafe {
                    let main_fn = match ee.get_function_value("main") {
                        Ok(main_fn) => main_fn,
                        Err(FunctionLookupError::JITNotEnabled) => panic!("JIT not enabled here"),
                        Err(FunctionLookupError::FunctionNotFound) => {
                            eprintln!("couldn't find symbol 'main'");
                            Err(Exit(255))?
                        }
                    };
                    reporter.finish();
                    ee.run_static_constructors();
                    let ec = ee.run_function_as_main(
                        main_fn,
                        &args.iter().map(String::as_str).collect::<Vec<_>>(),
                    );
                    ee.run_static_destructors();
                    if ec != 0 {
                        Err(Exit(ec))?
                    }
                }
            }
            MultiSubcommand::Check {
                mut inputs,
                linked,
                link_dirs,
                headers,
                no_default_link,
                timings,
                dump_header,
            } => {
                struct Reporter {
                    timings: bool,
                    reported: bool,
                    bottom_line: bool,
                    start: Instant,
                    overall: Option<Duration>,
                    file_time: Option<Duration>,
                    file_len: usize,
                    parse_time: Option<Duration>,
                    ast_nodes: usize,
                    comp_time: Option<Duration>,
                    insts_before: usize,
                    insts_after: usize,
                    opt_time: Option<Duration>,
                    cg_time: Option<Duration>,
                    libs_time: Option<Duration>,
                    nlibs: usize,
                    cmd_time: Option<Duration>,
                }
                impl Reporter {
                    pub fn new(timings: bool) -> Self {
                        Self {
                            timings,
                            reported: false,
                            bottom_line: true,
                            start: Instant::now(),
                            overall: None,
                            file_time: None,
                            file_len: 0,
                            parse_time: None,
                            ast_nodes: 0,
                            comp_time: None,
                            insts_before: 0,
                            insts_after: 0,
                            opt_time: None,
                            cg_time: None,
                            libs_time: None,
                            nlibs: 0,
                            cmd_time: None,
                        }
                    }
                    fn print(&mut self) {
                        if !self.timings {
                            return;
                        }
                        if self.reported {
                            return;
                        }
                        self.reported = true;
                        let overall = self.overall.get_or_insert_with(|| self.start.elapsed());
                        println!("----------------");
                        println!("overall time: {:>8}", overall.human_duration().to_string());
                        if let Some(file) = self.file_time {
                            println!(
                                "reading file: {:>8} ({:6.3}%) @ {:>13}",
                                file.human_duration().to_string(),
                                file.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / file.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(parse) = self.parse_time {
                            println!(
                                "parsing code: {:>8} ({:6.3}%) @ {:>13}",
                                parse.human_duration().to_string(),
                                parse.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.file_len as f64 / parse.as_secs_f64())
                                    .human_throughput_bytes()
                                    .to_string()
                            );
                        }
                        if let Some(comp) = self.comp_time {
                            println!(
                                "LLVM codegen: {:>8} ({:6.3}%) @ {:>13}",
                                comp.human_duration().to_string(),
                                comp.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.ast_nodes as f64 / comp.as_secs_f64())
                                    .human_throughput(" node")
                                    .to_string()
                            );
                        }
                        if let Some(opt) = self.opt_time {
                            println!(
                                "optimization: {:>8} ({:6.3}%) @ {:>13}",
                                opt.human_duration().to_string(),
                                opt.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_before as f64 / opt.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if let Some(cg) = self.cg_time {
                            println!(
                                "output gen:   {:>8} ({:6.3}%) @ {:>13}",
                                cg.human_duration().to_string(),
                                cg.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                (self.insts_after as f64 / cg.as_secs_f64())
                                    .human_throughput(" inst")
                                    .to_string()
                            );
                        }
                        if self.nlibs != 0 {
                            if let Some(libs) = self.libs_time {
                                println!(
                                    "lib lookup:   {:>8} ({:6.3}%) @ {:>13}",
                                    libs.human_duration().to_string(),
                                    libs.as_secs_f64() / overall.as_secs_f64() * 100.0,
                                    (self.nlibs as f64 / libs.as_secs_f64())
                                        .human_throughput(" libs")
                                        .to_string()
                                );
                            }
                        }
                        if let Some(cmd) = self.cmd_time {
                            println!(
                                "external cmd: {:>8} ({:6.3}%)",
                                cmd.human_duration().to_string(),
                                cmd.as_secs_f64() / overall.as_secs_f64() * 100.0
                            );
                        }
                        if self.bottom_line {
                            println!("----------------");
                        }
                    }
                    // happy exit
                    pub fn finish(&mut self) {
                        self.bottom_line = false; // don't print a line after the report, everything went ok
                        self.print();
                    }
                }
                // give the report on drops too, in case of errors
                impl Drop for Reporter {
                    fn drop(&mut self) {
                        self.print();
                    }
                }
                let mut reporter = Reporter::new(timings);
                let codes = inputs
                    .iter_mut()
                    .map(|input| {
                        let start = Instant::now();
                        let code = read_file(input, &input.path().display().to_string())?;
                        reporter.file_time = Some(start.elapsed());
                        reporter.file_len = code.len();
                        anyhow::Ok(code)
                    })
                    .collect::<anyhow::Result<Vec<String>>>()?;
                Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
                let trip = TargetMachine::get_default_triple();
                let triple = trip.as_str().to_string_lossy().into_owned();
                let target_machine = Target::from_triple(&trip)
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
                let mut flags = Flags {
                    dbg_mangle: true,
                    prepass: false,
                    ..Flags::default()
                };
                let ink_ctx = inkwell::context::Context::create();
                if let Some(size) = ink_ctx
                    .ptr_sized_int_type(&target_machine.get_target_data(), None)
                    .size_of()
                    .get_zero_extended_constant()
                {
                    flags.word_size = size as u16;
                }
                flags.add_type_map = dump_header;
                let ctx = CompCtx::with_flags(&ink_ctx, "multi-check", flags);
                ctx.module.set_triple(&trip);
                let mut cc = cc::CompileCommand::new();
                cc.target(&triple);
                cc.link_dirs(link_dirs);
                cc.no_default_link = no_default_link;
                {
                    reporter.nlibs = linked.len();
                    let start = Instant::now();
                    let notfound = cc.search_libs(linked, Some(&ctx), false)?;
                    if !notfound.is_empty() {
                        anyhow::bail!(LibsNotFound(notfound))
                    }
                    for head in headers {
                        let mut file = BufReader::new(std::fs::File::open(head)?);
                        ctx.load(&mut file)?;
                    }
                    reporter.libs_time = Some(start.elapsed());
                };
                let mut fail = false;
                let mut ec = 0;
                let asts = inputs
                    .iter()
                    .zip(&codes)
                    .map(|(input, code)| {
                        let file = FILES.add_file(
                            0,
                            input.path().display().to_string(),
                            code.clone().into(),
                        );
                        let ((ast, errs), parse_time) = timeit(|| parse_str(file.contents()));
                        let mut ast = ast.unwrap_or_default();
                        *reporter.parse_time.get_or_insert(Duration::ZERO) += parse_time;
                        reporter.ast_nodes += ast.nodes();
                        ast.file = Some(file);
                        for err in errs {
                            let is_err = true; // err.severity.map_or(true, |e| e > Severity::Warning);
                            if is_err {
                                ec += 1;
                                fail = true;
                            }
                            eprintln!("{:?}", Report::from(err).with_source_code(file));
                        }
                        ast.run_passes(&ctx);
                        anyhow::Ok(ast)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let mut errs = vec![];
                asts.iter().for_each(|ast| {
                    let (_, comp_time) = timeit(|| ast.codegen(&ctx, &mut errs));
                    *reporter.comp_time.get_or_insert(Duration::ZERO) += comp_time;
                    for err in errs.drain(..) {
                        fail |= err.is_err();
                        ec += err.is_err() as usize;
                        eprintln!(
                            "{:?}",
                            Report::from(err).with_source_code(ast.file.unwrap())
                        );
                    }
                });
                ctx.module.verify().map_err(LlvmVerifierError::from)?;
                if fail {
                    anyhow::bail!(CompileErrors(ec))
                }
                if dump_header {
                    serde_json::to_writer_pretty(std::io::stdout(), &ctx)?;
                }
                reporter.finish();
            }
        },
        Cli::Project(cmd) => match cmd {
            ProjSubcommand::Track { projects: None } => 'found: {
                for path in std::env::current_dir()?.ancestors() {
                    let cfg_path = path.join("cobalt.toml");
                    if !cfg_path.exists() {
                        continue;
                    }
                    if let Some(proj) = std::fs::read_to_string(&cfg_path)
                        .ok()
                        .and_then(|x| build::ProjectFragment::from_toml_static(&x).ok())
                    {
                        let mut vecs = load_projects()?;
                        track_project(proj.get_name()?, cfg_path, &mut vecs);
                        save_projects(vecs)?;
                        break 'found;
                    }
                }
                anyhow::bail!("couldn't find cobalt.toml in currnet or parent directories");
            }
            ProjSubcommand::Track {
                projects: Some(projs),
            } => {
                let mut vec = load_projects()?;
                for arg in projs {
                    let mut path: PathBuf = arg.into();
                    if path.is_dir() {
                        path.push("cobalt.toml");
                    }
                    track_project(
                        build::ProjectFragment::from_toml_static(
                            &Path::new(&path).read_to_string_anyhow()?,
                        )?
                        .get_name()?,
                        path,
                        &mut vec,
                    );
                }
                save_projects(vec)?;
            }
            ProjSubcommand::Untrack { projects: None } => 'found: {
                for path in std::env::current_dir()?.ancestors() {
                    let cfg_path = path.join("cobalt.toml");
                    if !cfg_path.exists() {
                        continue;
                    }
                    if std::fs::read_to_string(&cfg_path)
                        .ok()
                        .and_then(|x| build::ProjectFragment::from_toml_static(&x).ok())
                        .is_some()
                    {
                        let mut vecs = load_projects()?;
                        vecs.retain(|[_, p]| Path::new(p) != cfg_path);
                        save_projects(vecs)?;
                        break 'found;
                    }
                }
                error!("couldn't find cobalt.toml in currnet or parent directories");
            }
            ProjSubcommand::Untrack {
                projects: Some(projs),
            } => {
                let mut vec = load_projects()?;
                for arg in projs {
                    let mut path: PathBuf = arg.into();
                    if path.is_dir() {
                        path.push("cobalt.toml");
                    }
                    vec.retain(|[_, p]| Path::new(p) != path);
                }
                save_projects(vec)?;
            }
            ProjSubcommand::List { machine } => {
                let vecs = load_projects()?;
                if machine {
                    vecs.iter().for_each(|[n, p]| println!("{n}\t{p}"))
                } else {
                    let padding = vecs
                        .iter()
                        .map(|[n, _]| n.chars().count())
                        .max()
                        .unwrap_or(0);
                    vecs.iter().for_each(|[n, p]| {
                        println!("{n}{} => {p}", " ".repeat(padding - n.chars().count()))
                    });
                }
            }
            ProjSubcommand::Build {
                project_dir,
                source_dir,
                build_dir,
                profile,
                link_dirs,
                no_default_link,
                triple,
                targets,
                rebuild,
            } => {
                let set_src = source_dir.is_none();
                let set_build = build_dir.is_none();
                let frag = build::ProjectFragment {
                    source_dir: source_dir.as_deref().map(From::from),
                    build_dir: build_dir.as_deref().map(From::from),
                    ..Default::default()
                };
                let (project, _project_dir) = match project_dir.as_deref() {
                    Some("-") => {
                        let mut cfg = String::new();
                        std::io::stdin()
                            .read_to_string(&mut cfg)
                            .context("failed to read project file")?;
                        let mut project = build::ProjectFragment::default();
                        if_config_json! {
                            if cfg.trim_start().starts_with('{') {
                                project = build::ProjectFragment::from_json_static(cfg.as_str())
                                    .context("failed to parse project file")?;
                            }
                        }
                        if_config_toml! {
                            if !(CONFIG_JSON && cfg.trim_start().starts_with('{')) {
                                project = build::ProjectFragment::from_toml_static(cfg.as_str())
                                    .context("failed to parse project file")?;
                            }
                        }
                        project.set_dirs(".");
                        (project.try_into()?, PathBuf::from("."))
                    }
                    Some(path) => {
                        let mut path = path.to_string();
                        let mut vecs = load_projects()?;
                        if path.starts_with(':') {
                            if let Some(p) = vecs
                                .iter()
                                .find_map(|[n, p]| (n == &path[1..]).then_some(p).cloned())
                            {
                                path = p
                            }
                        }
                        let (proj, path) = build::Project::load(path, set_src, set_build, frag)?;
                        track_project(&proj.name, path.clone(), &mut vecs);
                        save_projects(vecs)?;
                        (proj, path)
                    }
                    None => {
                        let (proj, path) = build::Project::load(
                            std::env::current_dir()?,
                            set_src,
                            set_build,
                            frag,
                        )?;
                        let mut vecs = load_projects()?;
                        track_project(&proj.name, path.clone(), &mut vecs);
                        save_projects(vecs)?;
                        (proj, path)
                    }
                };
                if triple.is_some() {
                    Target::initialize_all(&INIT_NEEDED)
                } else {
                    Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?
                }
                let triple = triple.unwrap_or_else(|| {
                    TargetMachine::get_default_triple()
                        .as_str()
                        .to_string_lossy()
                        .into_owned()
                });
                build::build(
                    &project,
                    if targets.is_empty() {
                        None
                    } else {
                        Some(targets.into_iter().map(String::from).collect())
                    },
                    &build::BuildOptions {
                        profile: profile.as_deref().unwrap_or("@default").into(),
                        triple: triple.as_str().into(),
                        continue_build: false,
                        continue_comp: false,
                        rebuild,
                        no_default_link,
                        link_dirs: link_dirs.iter().map(From::from).collect::<Vec<_>>(),
                    },
                )?;
            }
            ProjSubcommand::Run {
                project_dir,
                source_dir,
                build_dir,
                profile,
                link_dirs,
                no_default_link,
                target,
                rebuild,
                args,
            } => {
                let set_src = source_dir.is_none();
                let set_build = build_dir.is_none();
                let frag = build::ProjectFragment {
                    source_dir: source_dir.as_deref().map(From::from),
                    build_dir: build_dir.as_deref().map(From::from),
                    ..Default::default()
                };
                let (project, _project_dir) = match project_dir.as_deref() {
                    Some("-") => {
                        let mut cfg = String::new();
                        std::io::stdin()
                            .read_to_string(&mut cfg)
                            .context("failed to read project file")?;
                        let mut project = build::ProjectFragment::default();
                        if_config_json! {
                            if cfg.trim_start().starts_with('{') {
                                project = build::ProjectFragment::from_json_static(cfg.as_str())
                                    .context("failed to parse project file")?;
                            }
                        }
                        if_config_toml! {
                            if !(CONFIG_JSON && cfg.trim_start().starts_with('{')) {
                                project = build::ProjectFragment::from_toml_static(cfg.as_str())
                                    .context("failed to parse project file")?;
                            }
                        }
                        project.set_dirs(".");
                        (project.try_into()?, PathBuf::from("."))
                    }
                    Some(path) => {
                        let mut path = path.to_string();
                        let mut vecs = load_projects()?;
                        if path.starts_with(':') {
                            if let Some(p) = vecs
                                .iter()
                                .find_map(|[n, p]| (n == &path[1..]).then_some(p).cloned())
                            {
                                path = p
                            }
                        }
                        let (proj, path) = build::Project::load(path, set_src, set_build, frag)?;
                        track_project(&proj.name, path.clone(), &mut vecs);
                        save_projects(vecs)?;
                        (proj, path)
                    }
                    None => {
                        let (proj, path) = build::Project::load(
                            std::env::current_dir()?,
                            set_src,
                            set_build,
                            frag,
                        )?;
                        let mut vecs = load_projects()?;
                        track_project(&proj.name, path.clone(), &mut vecs);
                        save_projects(vecs)?;
                        (proj, path)
                    }
                };
                Target::initialize_native(&INIT_NEEDED).map_err(anyhow::Error::msg)?;
                let mut target = target.map_or_else(
                    || {
                        let exes = project
                            .targets
                            .iter()
                            .filter_map(|(k, x)| {
                                (x.target_type == build::TargetType::Executable).then_some(&**k)
                            })
                            .collect::<Vec<_>>();
                        match exes.len() {
                            0 => {
                                anyhow::bail!("no executable targets available for current project")
                            }
                            1 => Ok(exes[0].to_string()),
                            x => anyhow::bail!(
                                "{x} executable targets available, please select one: {exes:?}"
                            ),
                        }
                    },
                    |t| {
                        if project.targets.get(&*t).map(|x| x.target_type)
                            != Some(build::TargetType::Executable)
                        {
                            anyhow::bail!("target type must be an executable")
                        }
                        Ok(t)
                    },
                )?;
                let triple = TargetMachine::get_default_triple()
                    .as_str()
                    .to_string_lossy()
                    .into_owned();
                build::build(
                    &project,
                    Some(vec![target.clone()]),
                    &build::BuildOptions {
                        profile: profile.as_deref().unwrap_or("@default").into(),
                        triple: triple.as_str().into(),
                        continue_build: false,
                        continue_comp: false,
                        rebuild,
                        no_default_link,
                        link_dirs: link_dirs.iter().map(From::from).collect::<Vec<_>>(),
                    },
                )?;
                let mut exe_path = project.build_dir.into_owned();
                if triple.contains("windows") {
                    target.push_str(".exe");
                }
                exe_path.push(target);
                let code = Command::new(exe_path)
                    .args(args)
                    .status()?
                    .code()
                    .unwrap_or(-1);
                if code != 0 {
                    Err(Exit(code))?
                }
            }
        },
        Cli::Package(cmd) => match cmd {
            PkgSubcommand::Update => pkg::update_packages()?,
            PkgSubcommand::Install { packages } => {
                pkg::install(
                    packages
                        .into_iter()
                        .map(|x| x.parse())
                        .collect::<Result<Vec<_>, _>>()?,
                    &Default::default(),
                )?;
            }
        },
    }
    Ok(())
}

pub mod prelude {
    pub use super::{driver, Cli, Exit};
    pub use clap::Parser as _;
}
