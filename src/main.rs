use colored::Colorize;
use inkwell::targets::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{self, termcolor::{ColorChoice, StandardStream}};
use std::process::{Command, exit};
use std::io::{Read, Write, BufReader};
use std::ffi::OsString;
use path_dedot::ParseDot;
use std::path::{Path, PathBuf};
mod libs;
mod opt;
mod jit;
mod build;
mod package;
const HELP: &str = "co- Cobalt compiler and build system
A program can be compiled using the `co aot' subcommand, or JIT compiled using the `co jit' subcommand";
#[derive(Debug, PartialEq, Eq)]
enum OutputType {
    Executable,
    Library,
    Object,
    Assembly,
    LLVM,
    Bitcode
}
const INIT_NEEDED: InitializationConfig = InitializationConfig {
    asm_parser: true,
    asm_printer: true,
    base: true,
    disassembler: false,
    info: true,
    machine_code: true
};
type MainFn = unsafe extern "C" fn(i32, *const *const i8, *const *const i8) -> i32;
#[allow(non_snake_case)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ERROR = &"error".bright_red().bold();
    let WARNING = &"warning".bright_yellow().bold();
    let MODULE = &"module".blue().bold();
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        println!("{}", HELP);
        return Ok(());
    }
    match args[1].as_str() {
        "help" | "--help" | "-h" => {
            match args.get(2) {
                None => println!("{HELP}"),
                Some(x) if x.len() == 5 && (x.as_bytes()[0] == 'E' as u8 || x.as_bytes()[0] == 'W' as u8) && x.bytes().skip(1).all(|x| x >= '0' as u8 && x <= '9' as u8) => {
                    match cobalt::errors::info::lookup(x[1..].parse().unwrap()).map(|x| x.help) {
                        None | Some("") => eprintln!("no help message available for {x}"),
                        Some(x) => println!("{x}")
                    }
                },
                Some(x) => {
                    eprintln!("unknown help category {x:?}");
                    exit(1)
                }
            }
        },
        "version" | "--version" | "-v" | "-V" => {
            println!(
                "Cobalt version {}\n\
                LLVM version {}\n\
                Git commit {} on branch {}{}", env!("CARGO_PKG_VERSION"), "14.0.1.6", env!("GIT_COMMIT"), env!("GIT_BRANCH"), if cfg!(debug_assertions) {"\nDebug Build"} else {""});
        }
        "lex" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            for arg in args.into_iter().skip(2) {
                if arg.len() == 0 {continue;}
                if arg.as_bytes()[0] == ('-' as u8) {
                    for c in arg.chars().skip(1) {
                        match c {
                            'c' => {
                                if nfcl {
                                    eprintln!("{WARNING}: reuse of -c flag");
                                }
                                nfcl = true;
                            }
                            x => eprintln!("{WARNING}: unknown flag -{x}")
                        }
                    }
                }
                else if nfcl {
                    nfcl = false;
                    let file = cobalt::errors::files::add_file("<command line>".to_string(), arg.clone());
                    let files = &*cobalt::errors::files::FILES.read().unwrap();
                    let (toks, errs) = cobalt::parser::lex(arg.as_str(), (file, 0), &flags);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
                    for tok in toks {term::emit(&mut stdout, &config, files, &Diagnostic::note().with_message(format!("{tok}")).with_labels(vec![Label::primary(tok.loc.0, tok.loc.1)])).unwrap();}
                }
                else {
                    let code = std::fs::read_to_string(arg.as_str())?;
                    let file = cobalt::errors::files::add_file(arg.clone(), code.clone());
                    let files = &*cobalt::errors::files::FILES.read().unwrap();
                    let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
                    for tok in toks {term::emit(&mut stdout, &config, files, &Diagnostic::note().with_message(format!("{tok}")).with_labels(vec![Label::primary(tok.loc.0, tok.loc.1)])).unwrap();}
                }
            }
            if nfcl {
                eprintln!("{ERROR}: -c flag must be followed by code");
            }
        },
        "parse" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut loc = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            for arg in args.into_iter().skip(2) {
                if arg.len() == 0 {continue;}
                if arg.as_bytes()[0] == ('-' as u8) {
                    for c in arg.chars().skip(1) {
                        match c {
                            'c' => {
                                if nfcl {
                                    eprintln!("{WARNING}: reuse of -c flag");
                                }
                                nfcl = true;
                            },
                            'l' => {
                                if loc {
                                    eprintln!("{WARNING}: reuse of -l flag");
                                }
                                loc = true;
                            },
                            x => eprintln!("{WARNING}: unknown flag -{}", x)
                        }
                    }
                }
                else if nfcl {
                    nfcl = false;
                    let file = cobalt::errors::files::add_file("<command line>".to_string(), arg.clone());
                    let files = &*cobalt::errors::files::FILES.read().unwrap();
                    let (toks, mut errs) = cobalt::parser::lex(arg.as_str(), (file, 0), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
                    if loc {print!("{:#}", ast)}
                    else {print!("{}", ast)}
                }
                else {
                    let code = std::fs::read_to_string(arg.as_str())?;
                    let file = cobalt::errors::files::add_file(arg.clone(), code.clone());
                    let files = &*cobalt::errors::files::FILES.read().unwrap();
                    let (toks, mut errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
                    if loc {print!("{:#}", ast)}
                    else {print!("{}", ast)}
                }
            }
            if nfcl {
                eprintln!("{ERROR}: -c flag must be followed by code");
            }
        },
        "llvm" if cfg!(debug_assertions) => {
            let mut in_file: Option<&str> = None;
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.len() == 0 {continue;}
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if in_file.is_none() {
                eprintln!("{ERROR}: no input file given");
                exit(1)
            }
            let in_file = in_file.unwrap();
            let code = if in_file == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {std::fs::read_to_string(in_file)?};
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            let mut fail = false;
            let file = cobalt::errors::files::add_file(in_file.to_string(), code.clone());
            let files = &*cobalt::errors::files::FILES.read().unwrap();
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, in_file);
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                fail = true;
            }
            print!("{}", ctx.module.to_string());
            exit(if fail {101} else {0})
        },
        "lib-header" if cfg!(debug_assertions) => {
            let mut in_file: Option<&str> = None;
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.len() == 0 {continue;}
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if in_file.is_none() {
                eprintln!("{ERROR}: no input file given");
                exit(1)
            }
            let in_file = in_file.unwrap();
            let code = if in_file == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {std::fs::read_to_string(in_file)?};
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            let mut fail = false;
            let file = cobalt::errors::files::add_file(in_file.to_string(), code.clone());
            let files = &*cobalt::errors::files::FILES.read().unwrap();
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, in_file);
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap();}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                fail = true;
            }
            if let Err(e) = ctx.with_vars(|v| v.save(&mut std::io::stdout())) {
                eprintln!("error saving {in_file}: {e}");
                exit(102)
            }
            exit(if fail {101} else {0});
        },
        "read-lib" if cfg!(debug_assertions) => {
            for fname in std::env::args().skip(2) {
                let ink_ctx = inkwell::context::Context::create();
                let ctx = cobalt::CompCtx::new(&ink_ctx, "<anon>");
                let mut file = BufReader::new(match std::fs::File::open(&fname) {Ok(f) => f, Err(e) => {eprintln!("error opening {fname}: {e}"); continue}});
                match cobalt::varmap::VarMap::load_new(&mut file, &ctx) {
                    Ok(v) => v.dump(),
                    Err(e) => eprintln!("error loading {fname}: {e}")
                }
            }
        },
        "aot" => {
            let mut output_type: Option<OutputType> = None;
            let mut in_file: Option<&str> = None;
            let mut out_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<String> = vec![];
            let mut triple: Option<TargetTriple> = None;
            let mut continue_if_err = false;
            let mut no_default_link = false;
            let mut profile: Option<&str> = None;
            let mut linker_args: Vec<&str> = vec![];
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.len() == 0 {continue;}
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                "continue" => {
                                    if continue_if_err {
                                        eprintln!("{WARNING}: reuse of --continue flag");
                                    }
                                    continue_if_err = true;
                                },
                                "emit-asm" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Assembly);
                                },
                                "emit-obj" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Object);
                                },
                                "emit-llvm" | "emit-ir" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::LLVM);
                                },
                                "emit-bc" | "emit-bitcode" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Bitcode);
                                },
                                "lib" | "emit-lib" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Library);
                                },
                                "exe" | "executable" | "emit-exe" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Executable);
                                },
                                "no-default-link" => {
                                    if no_default_link {
                                        eprintln!("{WARNING}: reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'p' => {
                                        if profile.is_some() {
                                            eprintln!("{WARNING}: respecification of optimization profile");
                                        }
                                        if let Some(x) = it.next() {
                                            profile = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected profile after -p flag");
                                            exit(1)
                                        }
                                    },
                                    'c' => {
                                        if continue_if_err {
                                            eprintln!("{WARNING}: reuse of -c flag");
                                        }
                                        continue_if_err = true;
                                    },
                                    'o' => {
                                        if out_file.is_some() {
                                            eprintln!("{ERROR}: respecification of input file");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next() {
                                            out_file = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected file after -o flag");
                                            exit(1)
                                        }
                                    },
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected library after -l flag");
                                            exit(1)
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x.clone());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected directory after -L flag");
                                            exit(1)
                                        }
                                    },
                                    't' => {
                                        if triple.is_some() {
                                            eprintln!("{ERROR}: respecification of target triple");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next().map(|x| TargetTriple::create(x)) {
                                            triple = Some(x);
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected target triple after -t flag");
                                            exit(1)
                                        }
                                    },
                                    'X' => {
                                        linker_args.extend(it.next().map(|x| x.as_str()).unwrap_or("").split(","));
                                    },
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if !no_default_link {
                if let Some(pwd) = std::env::current_dir().ok().and_then(|pwd| pwd.to_str().map(String::from)) {link_dirs.insert(0, pwd);}
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            if in_file.is_none() {
                eprintln!("{ERROR}: no input file given");
                exit(1)
            }
            let in_file = in_file.unwrap();
            let code = if in_file == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {std::fs::read_to_string(in_file)?};
            let output_type = output_type.unwrap_or(OutputType::Executable);
            if triple.is_some() {Target::initialize_all(&INIT_NEEDED)}
            else {Target::initialize_native(&INIT_NEEDED)?}
            let triple = triple.unwrap_or_else(TargetMachine::get_default_triple);
            let out_file = out_file.map(String::from).unwrap_or_else(|| match output_type {
                OutputType::Executable => format!("{}{}", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file), if triple.as_str().to_str().unwrap_or("").contains("windows") {".exe"} else {""}),
                OutputType::Library => format!("lib{}.so", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Object => format!("{}.o", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Assembly => format!("{}.s", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::LLVM => format!("{}.ll", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Bitcode => format!("{}.bc", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file))
            });
            let out_file = if out_file == "-" {None} else {Some(out_file)};
            let target_machine = Target::from_triple(&triple).unwrap().create_target_machine(
                &triple,
                "",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Small
            ).expect("failed to create target machine");
            let mut flags = cobalt::Flags::default();
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx.ptr_sized_int_type(&target_machine.get_target_data(), None).size_of().get_zero_extended_constant() {flags.word_size = size;}
            let ctx = cobalt::context::CompCtx::with_flags(&ink_ctx, in_file, flags);
            ctx.module.set_triple(&triple);
            let libs = if linked.len() > 0 {
                let (libs, notfound) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect(), Some(&ctx))?;
                notfound.iter().for_each(|nf| eprintln!("{ERROR}: couldn't find library {nf}"));
                if notfound.len() > 0 {exit(102)}
                libs
            } else {vec![]};
            let mut fail = false;
            let mut overall_fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let file = cobalt::errors::files::add_file(in_file.to_string(), code.clone());
            let files = &*cobalt::errors::files::FILES.read().unwrap();
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {exit(101)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile, &pm);
            pm.run_on(&ctx.module);
            match output_type {
                OutputType::LLVM =>
                    if let Some(out) = out_file {std::fs::write(out, ctx.module.to_string().as_bytes())?}
                    else {println!("{}", ctx.module.to_string())},
                OutputType::Bitcode =>
                    if let Some(out) = out_file {std::fs::write(out, ctx.module.write_bitcode_to_memory().as_slice())?}
                    else {std::io::stdout().write_all(ctx.module.write_bitcode_to_memory().as_slice())?},
                _ => {
                    if output_type == OutputType::Assembly {
                        let code = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Assembly).unwrap();
                        if let Some(out) = out_file {std::fs::write(out, code.as_slice())?}
                        else {std::io::stdout().write_all(code.as_slice())?}
                        return Ok(())
                    }
                    let mb = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object).unwrap();
                    match output_type {
                        OutputType::Executable => {
                            if out_file.is_none() {
                                eprintln!("cannot output executable to stdout");
                                exit(4)
                            }
                            let tmp = temp_file::with_contents(mb.as_slice());
                            let mut args = vec![OsString::from(tmp.path()), OsString::from("-o"), OsString::from(out_file.unwrap())];
                            for (lib, _) in libs {
                                let lib = lib.parse_dot()?;
                                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                                args.push(OsString::from("-L"));
                                args.push(parent.clone());
                                args.push(OsString::from("-rpath"));
                                args.push(parent);
                                args.push(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
                            }
                            args.extend(linker_args.into_iter().map(OsString::from));
                            exit( // search for cc, then, clang, and finally gcc
                                Command::new("cc").args(args.iter()).status()
                                .or_else(|_| Command::new("clang").args(args.iter()).status())
                                .or_else(|_| Command::new("gcc").args(args.iter()).status())
                                .ok().and_then(|x| x.code()).unwrap_or(0))
                        },
                        OutputType::Library => {
                            if let Some(out_file) = out_file {
                                let mut tmp = temp_file::with_contents(mb.as_slice());
                                let mut cmd = Command::new("ld");
                                cmd
                                    .arg("--shared")
                                    .arg(tmp.path())
                                    .arg("-o")
                                    .arg(&out_file);
                                let mut args = vec![];
                                for (lib, _) in libs {
                                    let parent = lib.parent().unwrap().as_os_str().to_os_string();
                                    args.push(OsString::from("-L"));
                                    args.push(parent.clone());
                                    args.push(OsString::from("-rpath"));
                                    args.push(parent);
                                    args.push(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
                                }
                                let code = cmd.args(args).status().ok().and_then(|x| x.code()).unwrap_or(-1);
                                if code != 0 {exit(code)}
                                let mut buf = Vec::<u8>::new();
                                if let Err(e) = ctx.with_vars(|v| v.save(&mut buf)) {
                                    eprintln!("{ERROR}: {e}");
                                    exit(4)
                                }
                                tmp = temp_file::with_contents(&buf);
                                cmd = Command::new("objcopy");
                                cmd
                                    .arg(&out_file)
                                    .arg("--add-section")
                                    .arg(format!(".colib={}", tmp.path().as_os_str().to_str().expect("temporary file should be valid Unicode")))
                                    .arg("--set-section-flags")
                                    .arg(format!(".colib=readonly,data"));
                                exit(cmd.status().ok().and_then(|x| x.code()).unwrap_or(-1))
                            }
                            else {eprintln!("{ERROR}: cannot output library to stdout!"); exit(4)}
                        },
                        OutputType::Object =>
                            if let Some(out) = out_file {std::fs::write(out, mb.as_slice())?}
                            else {std::io::stdout().write_all(mb.as_slice())?}
                        x => panic!("{x:?} has already been handled")
                    }
                }
            }
        },
        "jit" => {
            let mut in_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<String> = vec![];
            let mut continue_if_err = false;
            let mut no_default_link = false;
            let mut profile: Option<&str> = None;
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.len() == 0 {continue;}
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                "continue" => {
                                    if continue_if_err {
                                        eprintln!("{WARNING}: reuse of --continue flag");
                                    }
                                    continue_if_err = true;
                                },
                                "no-default-link" => {
                                    if no_default_link {
                                        eprintln!("{WARNING}: reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'p' => {
                                        if profile.is_some() {
                                            eprintln!("{WARNING}: respecification of optimization profile");
                                        }
                                        if let Some(x) = it.next() {
                                            profile = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected profile after -p flag");
                                            exit(1)
                                        }
                                    },
                                    'c' => {
                                        if continue_if_err {
                                            eprintln!("{WARNING}: reuse of -c flag");
                                        }
                                        continue_if_err = true;
                                    },
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected library after -l flag");
                                            exit(1)
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x.clone());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected directory after -L flag");
                                            exit(1)
                                        }
                                    },
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if !no_default_link {
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let (in_file, code) = if in_file.is_none() {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                ("<stdin>", s)
            }
            else {
                let f = in_file.unwrap();
                (f, std::fs::read_to_string(f)?)
            };
            let ink_ctx = inkwell::context::Context::create();
            let mut ctx = cobalt::context::CompCtx::new(&ink_ctx, in_file);
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let libs = if linked.len() > 0 {
                let (libs, notfound) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect(), Some(&ctx))?;
                notfound.iter().for_each(|nf| eprintln!("{ERROR}: couldn't find library {nf}"));
                if notfound.len() > 0 {exit(102)}
                libs
            } else {vec![]};
            let mut fail = false;
            let mut overall_fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            let file = cobalt::errors::files::add_file(in_file.to_string(), code.clone());
            let files = &*cobalt::errors::files::FILES.read().unwrap();
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {exit(101)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile, &pm);
            pm.run_on(&ctx.module);
            let jit = jit::LLJIT::new();
            {
                let mut m = ink_ctx.create_module("");
                std::mem::swap(&mut m, &mut ctx.module);
                jit.add_module(jit.main(), m);
            }
            std::mem::drop(libs);
            unsafe {
                exit(jit.lookup_main::<MainFn>(&std::ffi::CString::new("_start").unwrap()).expect("couldn't find 'main'")(1, [format!("co jit {}", if in_file == "<stdin>" {"-"} else {in_file}).as_ptr() as *const i8].as_ptr(), [0 as *const i8].as_ptr()));
            }
        },
        "check" => {
            let mut in_file: Option<&str> = None;
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            let (in_file, code) = if in_file.is_none() {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                ("<stdin>", s)
            }
            else {
                let f = in_file.unwrap();
                (f, std::fs::read_to_string(f)?)
            };
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, in_file);
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let mut fail = false;
            let mut overall_fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Always);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            let file = cobalt::errors::files::add_file(in_file.to_string(), code.clone());
            let files = &*cobalt::errors::files::FILES.read().unwrap();
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            if fail {eprintln!("{ERROR}: lexing failed; the following errors may not be accurate")}
            fail = false;
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            overall_fail |= fail;
            if fail {eprintln!("{ERROR}: parsing failed; the following errors may not be accurate")}
            fail = false;
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {term::emit(&mut stdout, &config, files, &err.0).unwrap(); fail |= err.is_err();}
            if fail {eprintln!("{ERROR}: code generation failed; the following errors may not be accurate")}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {exit(101)}
        },
        "build" => {
            let mut project_dir: Option<&str> = None;
            let mut source_dir: Option<&str> = None;
            let mut build_dir: Option<&str> = None;
            let mut profile: Option<&str> = None;
            let mut link_dirs: Vec<String> = vec![];
            let mut no_default_link = false;
            let mut triple: Option<TargetTriple> = None;
            let mut targets: Vec<&str> = vec![];
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if project_dir.is_some() {
                                eprintln!("{ERROR}: respecification of project directory");
                                exit(1)
                            }
                            project_dir = Some("-");
                        }
                        else if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                "no-default-link" => {
                                    if no_default_link {
                                        eprintln!("{WARNING}: reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'p' => {
                                        if profile.is_some() {
                                            eprintln!("{WARNING}: respecification of optimization profile");
                                        }
                                        if let Some(x) = it.next() {
                                            profile = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected profile after -p flag");
                                            exit(1)
                                        }
                                    },
                                    's' => {
                                        if profile.is_some() {
                                            eprintln!("{ERROR}: respecification of source directory");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next() {
                                            source_dir = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected source directory after -s flag");
                                            exit(1)
                                        }
                                    },
                                    'b' => {
                                        if profile.is_some() {
                                            eprintln!("{WARNING}: respecification of build directory");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next() {
                                            build_dir = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected build directory after -b flag");
                                            exit(1)
                                        }
                                    },
                                    't' => {
                                        if profile.is_some() {
                                            eprintln!("{WARNING}: respecification of target triple");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next() {
                                            triple = Some(TargetTriple::create(x.as_str()));
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected target triple after -t flag");
                                            exit(1)
                                        }
                                    },
                                    'T' => {
                                        if let Some(x) = it.next() {
                                            targets.push(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected build target after -T flag");
                                            exit(1)
                                        }
                                    },
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if project_dir.is_some() {
                            eprintln!("{ERROR}: respecification of project directory");
                            exit(1)
                        }
                        project_dir = Some(arg.as_str());
                    }
                }
            }
            let (project_data, project_dir) = match project_dir {
                Some("-") => {
                    let mut cfg = String::new();
                    if let Err(e) = std::io::stdin().read_to_string(&mut cfg) {
                        eprintln!("error when reading project file from stdin: {e}");
                        exit(100)
                    }
                    (match toml::from_str::<build::Project>(cfg.as_str()) {
                        Ok(proj) => proj,
                        Err(e) => {
                            eprintln!("error when parsing project file: {e}");
                            exit(100)
                        }
                    }, PathBuf::from("."))
                },
                Some(x) => {
                    if !Path::new(x).exists() {
                        eprintln!("{ERROR}: {x} does not exist");
                        exit(100)
                    }
                    match std::fs::metadata(x).map(|x| x.file_type().is_dir()) {
                        Ok(true) => {
                            let mut path = std::path::PathBuf::from(x);
                            path.push("cobalt.toml");
                            if !path.exists() {
                                eprintln!("{ERROR}: cannot find cobalt.toml in {x}");
                                exit(100)
                            }
                            let cfg;
                            match std::fs::read_to_string(path) {
                                Ok(c) => cfg = c,
                                Err(e) => {
                                    eprintln!("error when reading project file: {e}");
                                    exit(100)
                                }
                            }
                            (match toml::from_str::<build::Project>(cfg.as_str()) {
                                Ok(proj) => proj,
                                Err(e) => {
                                    eprintln!("error when parsing project file: {e}");
                                    exit(100)
                                }
                            }, PathBuf::from(x))
                        },
                        Ok(false) => {
                            let mut path = std::path::PathBuf::from(x);
                            path.pop();
                            let cfg;
                            match std::fs::read_to_string(x) {
                                Ok(c) => cfg = c,
                                Err(e) => {
                                    eprintln!("error when reading project file: {e}");
                                    exit(100)
                                }
                            }
                            (match toml::from_str::<build::Project>(cfg.as_str()) {
                                Ok(proj) => proj,
                                Err(e) => {
                                    eprintln!("error when parsing project file: {e}");
                                    exit(100)
                                }
                            }, path)
                        },
                        Err(e) => {
                            eprintln!("error when determining type of {x}: {e}");
                            exit(100)
                        }
                    }
                },
                None => {
                    let cfg;
                    if !Path::new("cobalt.toml").exists() {
                        eprintln!("{ERROR}: couldn't find cobalt.toml in current directory");
                        exit(100)
                    }
                    match std::fs::read_to_string("cobalt.toml") {
                        Ok(c) => cfg = c,
                        Err(e) => {
                            eprintln!("error when reading project file: {e}");
                            exit(100)
                        }
                    }
                    (match toml::from_str::<build::Project>(cfg.as_str()) {
                        Ok(proj) => proj,
                        Err(e) => {
                            eprintln!("error when parsing project file: {e}");
                            exit(100)
                        }
                    }, PathBuf::from("."))
                }
            };
            if !no_default_link {
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let source_dir: &Path = source_dir.map_or(project_dir.as_path(), Path::new);
            let build_dir: PathBuf = build_dir.map_or_else(|| {
                let mut dir = project_dir.clone();
                dir.push("build");
                dir
            }, PathBuf::from);
            if triple.is_some() {Target::initialize_all(&INIT_NEEDED)}
            else {Target::initialize_native(&INIT_NEEDED)?}
            exit(build::build(project_data, if targets.len() == 0 {None} else {Some(targets.into_iter().map(String::from).collect())}, &build::BuildOptions {
                source_dir,
                build_dir: build_dir.as_path(),
                profile: profile.unwrap_or("default"),
                triple: &triple.unwrap_or_else(TargetMachine::get_default_triple),
                continue_build: false,
                continue_comp: false,
                link_dirs
            }));
        },
        "install" => {
            match package::Package::init_registry() {
                Ok(()) => {},
                Err(package::PackageUpdateError::NoInstallDirectory) => {
                    eprintln!("{ERROR}: could not find or infer Cobalt directory");
                    exit(1)
                },
                Err(package::PackageUpdateError::GitError(e)) => {
                    eprintln!("{ERROR}: {e}");
                    exit(2)
                }
                Err(package::PackageUpdateError::StdIoError(e)) => {
                    eprintln!("{ERROR}: {e}");
                    exit(3)
                }
            };
            let mut good = 0;
            let registry = package::Package::registry();
            for pkg in args.iter().skip(2).skip_while(|x| x.len() == 0) {
                if let Some(p) = registry.get(pkg) {
                    match p.install(TargetMachine::get_default_triple().as_str().to_str().unwrap(), None, package::InstallOptions::default()) {
                        Err(package::InstallError::NoInstallDirectory) => panic!("This would only be reachable if $HOME was deleted in a data race, which may or may not even be possible"),
                        Err(package::InstallError::DownloadError(e)) => {
                            eprintln!("{ERROR}: {e}");
                            good = 4;
                        },
                        Err(package::InstallError::StdIoError(e)) => {
                            eprintln!("{ERROR}: {e}");
                            good = 3;
                        },
                        Err(package::InstallError::GitCloneError(e)) => {
                            eprintln!("{ERROR}: {e}");
                            good = 2;
                        },
                        Err(package::InstallError::ZipExtractError(e)) => {
                            eprintln!("{ERROR}: {e}");
                            good = 5;
                        },
                        Err(package::InstallError::BuildFailed(e)) => {
                            eprintln!("failed to build package {pkg}");
                            good = e;
                        },
                        Err(package::InstallError::NoMatchesError) => {
                            eprintln!("package {p:?} has no releases");
                            good = 7;
                        },
                        Err(package::InstallError::CfgFileError(e)) => {
                            eprintln!("{ERROR} in {pkg}'s config file: {e}");
                            good = 8;
                        },
                        Err(package::InstallError::InvalidVersionSpec(_, v)) => {
                            eprintln!("{ERROR} in {pkg}'s dependencies: invalid version spec {v}");
                            good = 9;
                        },
                        Err(package::InstallError::PkgNotFound(p)) => {
                            eprintln!("{ERROR} in {pkg}'s dependencies: can't find package {p}");
                            good = 10;
                        },
                        _ => {}
                    }
                }
                else {
                    eprintln!("{ERROR}: couldn't find package {pkg:?}");
                    good = 6;
                }
            }
            exit(good)
        },
        x => {
            eprintln!("unknown subcommand '{}'", x);
        }
    };
    Ok(())
}
