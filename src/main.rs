use inkwell::targets::*;
use inkwell::execution_engine::FunctionLookupError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{self, termcolor::{ColorChoice, StandardStream}};
use std::process::{Command, exit};
use std::io::{self, Read, Write, BufReader, Seek};
use std::ffi::OsString;
use path_calculate::*;
use std::path::{Path, PathBuf};
use cobalt::{AST, errors::FILES};
mod libs;
mod opt;
mod build;
mod package;
mod color;
const HELP: &str = "co- Cobalt compiler and build system
A program can be compiled using the `co aot' subcommand, or JIT compiled using the `co jit' subcommand";
fn load_projects() -> io::Result<Vec<[String; 2]>> {
    let mut cobalt_dir = PathBuf::from(if let Ok(path) = std::env::var("COBALT_DIR") {path}
        else if let Ok(path) = std::env::var("HOME") {format!("{path}/.cobalt")}
        else {error!("couldn't determine Cobalt directory"); exit(100)});
    if !cobalt_dir.exists() {std::fs::create_dir_all(&cobalt_dir)?;}
    cobalt_dir.push("tracked.txt");
    let mut file = std::fs::OpenOptions::new().read(true).write(true).create(true).open(cobalt_dir)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    let mut it = buf.split('\0');
    let len = it.size_hint();
    let mut vec = Vec::<[String; 2]>::with_capacity(len.1.unwrap_or(len.0));
    while let (Some(name), Some(path)) = (it.next(), it.next()) {if Path::new(path).exists() {vec.push([name.to_string(), path.to_string()])}}
    Ok(vec)
}
fn track_project(name: &str, path: PathBuf, vec: &mut Vec<[String; 2]>) {
    if let Some(entry) = vec.iter_mut().find(|[_, p]| if let Ok(path) = path.as_absolute_path() {path == Path::new(&p)} else {false}) {entry[0] = name.to_string()}
    else {vec.push([name.to_string(), path.as_absolute_path().unwrap().to_string_lossy().to_string()])}
}
fn save_projects(vec: Vec<[String; 2]>) -> io::Result<()> {
    let mut cobalt_dir = PathBuf::from(if let Ok(path) = std::env::var("COBALT_DIR") {path}
        else if let Ok(path) = std::env::var("HOME") {format!("{path}/.cobalt")}
        else {error!("couldn't determine Cobalt directory"); exit(100)});
    if !cobalt_dir.exists() {std::fs::create_dir_all(&cobalt_dir)?;}
    cobalt_dir.push("tracked.txt");
    let mut file = std::fs::OpenOptions::new().write(true).create(true).open(cobalt_dir)?;
    vec.into_iter().flatten().try_for_each(|s| {
        file.write_all(s.as_bytes())?;
        file.write_all(&[0])
    })?;
    let pos = file.stream_position()?;
    file.set_len(pos)
}
#[derive(Debug, PartialEq, Eq)]
enum OutputType {
    Executable,
    Library,
    Object,
    Assembly,
    Llvm,
    Bitcode,
    Header,
    HeaderObj
}
const INIT_NEEDED: InitializationConfig = InitializationConfig {
    asm_parser: true,
    asm_printer: true,
    base: true,
    disassembler: false,
    info: true,
    machine_code: true
};
fn driver() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        println!("{}", HELP);
        return Ok(());
    }
    match args[1].as_str() {
        "help" | "--help" | "-h" => {
            match args.get(2) {
                None => println!("{HELP}"),
                Some(x) if x.len() == 5 && (x.as_bytes()[0] == b'E' || x.as_bytes()[0] == b'W') && x.bytes().skip(1).all(|x| (b'0'..=b'9').contains(&x)) => {
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
            println!("Cobalt version {}", env!("CARGO_PKG_VERSION"));
            println!("LLVM version {}", env!("LLVM_VERSION"));
            #[cfg(has_git)]
            println!("Git commit {} on branch {}", &env!("GIT_COMMIT")[..6], env!("GIT_BRANCH"));
            #[cfg(debug_assertions)]
            println!("Debug Build");
        }
        "lex" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            for arg in args.into_iter().skip(2) {
                if arg.is_empty() {continue;}
                if arg.as_bytes()[0] == b'-' {
                    for c in arg.chars().skip(1) {
                        match c {
                            'c' => {
                                if nfcl {
                                    warning!("reuse of -c flag");
                                }
                                nfcl = true;
                            }
                            x => warning!("unknown flag -{x}")
                        }
                    }
                }
                else if nfcl {
                    nfcl = false;
                    let files = &mut *FILES.write().unwrap();
                    let file = files.add_file(0, "<command line>".to_string(), arg.clone());
                    let (toks, errs) = cobalt::parser::lex(arg.as_str(), (file, 0), &flags);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
                    for tok in toks {term::emit(&mut stdout, &config, files, &Diagnostic::note().with_message(format!("{tok}")).with_labels(vec![Label::primary(tok.loc.0, tok.loc.1)]))?;}
                }
                else {
                    let code = std::fs::read_to_string(arg.as_str())?;
                    let files = &mut *FILES.write().unwrap();
                    let file = files.add_file(0, arg.clone(), code.clone());
                    let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
                    for tok in toks {term::emit(&mut stdout, &config, files, &Diagnostic::note().with_message(format!("{tok}")).with_labels(vec![Label::primary(tok.loc.0, tok.loc.1)]))?;}
                }
            }
            if nfcl {
                error!("-c flag must be followed by code");
            }
        },
        "parse" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut loc = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            for arg in args.into_iter().skip(2) {
                if arg.is_empty() {continue;}
                if arg.as_bytes()[0] == b'-' {
                    for c in arg.chars().skip(1) {
                        match c {
                            'c' => {
                                if nfcl {
                                    warning!("reuse of -c flag");
                                }
                                nfcl = true;
                            },
                            'l' => {
                                if loc {
                                    warning!("reuse of -l flag");
                                }
                                loc = true;
                            },
                            x => warning!("unknown flag -{}", x)
                        }
                    }
                }
                else if nfcl {
                    nfcl = false;
                    let files = &mut *FILES.write().unwrap();
                    let file = files.add_file(0, "<command line>".to_string(), arg.clone());
                    let (toks, mut errs) = cobalt::parser::lex(arg.as_str(), (file, 0), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
                    if loc {print!("{:#}", ast)}
                    else {print!("{}", ast)}
                }
                else {
                    let code = std::fs::read_to_string(arg.as_str())?;
                    let files = &mut *FILES.write().unwrap();
                    let file = files.add_file(0, arg.clone(), code.clone());
                    let (toks, mut errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
                    if loc {print!("{:#}", ast)}
                    else {print!("{}", ast)}
                }
            }
            if nfcl {
                error!("-c flag must be followed by code");
            }
        },
        "llvm" if cfg!(debug_assertions) => {
            let mut in_file: Option<&str> = None;
            {
                let it = args.iter().skip(2).filter(|x| !x.is_empty());
                for arg in it {
                    if arg.is_empty() {continue;}
                    if arg.as_bytes()[0] == b'-' {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                error!("respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == b'-' {
                            error!("unknown flag --{}", &arg[2..]);
                            exit(1)
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                error!("unknown flag -{c}");
                                exit(1)
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            error!("respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if in_file.is_none() {
                error!("no input file given");
                exit(1)
            }
            let in_file = in_file.unwrap();
            let code = if in_file == "-" {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                s
            } else {std::fs::read_to_string(in_file)?};
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let mut flags = cobalt::Flags::default();
            flags.dbg_mangle = true;
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::with_flags(&ink_ctx, in_file, flags);
            let mut fail = false;
            let files = &mut *FILES.write().unwrap();
            let file = files.add_file(0, in_file.to_string(), code.clone());
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?;}
            if let Err(msg) = ctx.module.verify() {
                error!("\n{}", msg.to_string());
                fail = true;
            }
            print!("{}", ctx.module.to_string());
            exit(if fail {101} else {0})
        },
        "parse-header" if cfg!(debug_assertions) => {
            for fname in std::env::args().skip(2) {
                let ink_ctx = inkwell::context::Context::create();
                let ctx = cobalt::CompCtx::new(&ink_ctx, "<anon>");
                let mut file = BufReader::new(match std::fs::File::open(&fname) {Ok(f) => f, Err(e) => {eprintln!("error opening {fname}: {e}"); continue}});
                match ctx.load(&mut file) {
                    Ok(_) => ctx.with_vars(|v| v.dump()),
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
            let mut debug_mangle = false;
            let mut profile: Option<&str> = None;
            let mut headers: Vec<&str> = vec![];
            let mut linker_args: Vec<&str> = vec![];
            {
                let mut it = args.iter().skip(2).filter(|x| !x.is_empty());
                while let Some(arg) = it.next() {
                    if arg.is_empty() {continue;}
                    if arg.as_bytes()[0] == b'-' {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                error!("respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == b'-' {
                            match &arg[2..] {
                                "continue" => {
                                    if continue_if_err {
                                        warning!("reuse of --continue flag");
                                    }
                                    continue_if_err = true;
                                },
                                "emit-asm" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Assembly);
                                },
                                "emit-obj" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Object);
                                },
                                "emit-llvm" | "emit-ir" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Llvm);
                                },
                                "emit-bc" | "emit-bitcode" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Bitcode);
                                },
                                "lib" | "emit-lib" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Library);
                                },
                                "exe" | "executable" | "emit-exe" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Executable);
                                },
                                "header" | "emit-header" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::Header);
                                },
                                "header-obj" | "emit-header-obj" => {
                                    if output_type.is_some() {
                                        error!("respecification of output type");
                                        exit(1)
                                    }
                                    output_type = Some(OutputType::HeaderObj);
                                },
                                "no-default-link" => {
                                    if no_default_link {
                                        warning!("reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                "debug-mangle" => {
                                    if debug_mangle {
                                        warning!("reuse of --debug-mangle flag");
                                    }
                                    debug_mangle = true;
                                },
                                x => {
                                    error!("unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'p' => {
                                        if profile.is_some() {
                                            warning!("respecification of optimization profile");
                                        }
                                        if let Some(x) = it.next() {
                                            profile = Some(x.as_str());
                                        }
                                        else {
                                            error!("expected profile after -p flag");
                                            exit(1)
                                        }
                                    },
                                    'c' => {
                                        if continue_if_err {
                                            warning!("reuse of -c flag");
                                        }
                                        continue_if_err = true;
                                    },
                                    'o' => {
                                        if out_file.is_some() {
                                            error!("respecification of input file");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next() {
                                            out_file = Some(x.as_str());
                                        }
                                        else {
                                            error!("expected file after -o flag");
                                            exit(1)
                                        }
                                    },
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x.as_str());
                                        }
                                        else {
                                            error!("expected library after -l flag");
                                            exit(1)
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x.clone());
                                        }
                                        else {
                                            error!("expected directory after -L flag");
                                            exit(1)
                                        }
                                    },
                                    't' => {
                                        if triple.is_some() {
                                            error!("respecification of target triple");
                                            exit(1)
                                        }
                                        if let Some(x) = it.next().map(|x| TargetTriple::create(x)) {
                                            triple = Some(x);
                                        }
                                        else {
                                            error!("expected target triple after -t flag");
                                            exit(1)
                                        }
                                    },
                                    'X' => {
                                        linker_args.extend(it.next().map(|x| x.as_str()).unwrap_or("").split(','));
                                    },
                                    'h' => {
                                        if let Some(x) = it.next() {
                                            headers.push(x.as_str());
                                        }
                                        else {
                                            error!("expected header file after -h flag");
                                            exit(1)
                                        }
                                    },
                                    x => {
                                        error!("unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            error!("respecification of input file");
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
                error!("no input file given");
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
                OutputType::Library => libs::format_lib(in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file), &triple),
                OutputType::Object => format!("{}.o", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Assembly => format!("{}.s", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Llvm => format!("{}.ll", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Bitcode => format!("{}.bc", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Header => format!("{}.coh", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::HeaderObj => format!("{}.coh.o", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
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
            flags.dbg_mangle = debug_mangle;
            let ink_ctx = inkwell::context::Context::create();
            if let Some(size) = ink_ctx.ptr_sized_int_type(&target_machine.get_target_data(), None).size_of().get_zero_extended_constant() {flags.word_size = size as u16;}
            let ctx = cobalt::context::CompCtx::with_flags(&ink_ctx, in_file, flags);
            ctx.module.set_triple(&triple);
            let libs = if !linked.is_empty() {
                let (libs, notfound, failed) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect::<Vec<_>>(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
                if failed {exit(99)}
                libs
            } else {vec![]};
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let mut overall_fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let files = &mut *FILES.write().unwrap();
            let file = files.add_file(0, in_file.to_string(), code.clone());
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                error!(" {}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {exit(101)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile, &pm);
            pm.run_on(&ctx.module);
            match output_type {
                OutputType::Header =>
                    if let Some(out) = out_file {
                        let mut file = std::fs::File::create(out)?;
                        ctx.save(&mut file)?;
                    }
                    else {ctx.save(&mut std::io::stdout())?}
                OutputType::HeaderObj => {
                    let mut obj = libs::new_object(&triple);
                    libs::populate_header(&mut obj, &ctx);
                    if let Some(out) = out_file {
                        let file = std::fs::File::create(out)?;
                        obj.write_stream(file)?;
                    }
                    else {obj.write_stream(std::io::stdout())?}
                }
                OutputType::Llvm =>
                    if let Some(out) = out_file {std::fs::write(out, ctx.module.to_string().as_bytes())?}
                    else {println!("{}", ctx.module.to_string())},
                OutputType::Bitcode =>
                    if let Some(out) = out_file {std::fs::write(out, ctx.module.write_bitcode_to_memory().as_slice())?}
                    else {std::io::stdout().write_all(ctx.module.write_bitcode_to_memory().as_slice())?},
                OutputType::Assembly => {
                    let code = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Assembly).unwrap();
                    if let Some(out) = out_file {std::fs::write(out, code.as_slice())?}
                    else {std::io::stdout().write_all(code.as_slice())?}
                }
                _ => {
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
                                let lib = lib.as_absolute_path()?;
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
                                let mut obj = libs::new_object(&triple);
                                libs::populate_header(&mut obj, &ctx);
                                let tmp1 = match obj.write() {
                                    Ok(data) => temp_file::with_contents(&data),
                                    Err(err) => {
                                        error!("{err}");
                                        exit(4)
                                    }
                                };
                                let tmp2 = temp_file::with_contents(mb.as_slice());
                                let mut cmd = Command::new("ld");
                                cmd
                                    .arg("--shared")
                                    .arg(tmp1.path())
                                    .arg(tmp2.path())
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
                                std::mem::drop(tmp1);
                                std::mem::drop(tmp2);
                                exit(code)
                            }
                            else {error!("cannot output library to stdout!"); exit(4)}
                        },
                        OutputType::Object =>
                            if let Some(out) = out_file {std::fs::write(out, mb.as_slice())?}
                            else {std::io::stdout().write_all(mb.as_slice())?}
                        x => unreachable!("{x:?} has already been handled")
                    }
                }
            }
        },
        "jit" => {
            let mut in_file: Option<String> = None;
            let mut linked: Vec<String> = vec![];
            let mut link_dirs: Vec<String> = vec![];
            let mut continue_if_err = false;
            let mut no_default_link = false;
            let mut headers: Vec<String> = vec![];
            let mut profile: Option<String> = None;
            let mut args = Vec::<String>::new();
            {
                let mut it = std::env::args().filter(|x| !x.is_empty());
                args.push(it.by_ref().take(2).map(|x| format!("{x} ")).collect::<String>());
                while let Some(arg) = it.next() {
                    if arg.is_empty() {continue;}
                    if arg.as_bytes()[0] == b'-' {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                error!("respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-".to_string());
                        }
                        else if arg.as_bytes()[1] == b'-' {
                            match &arg[2..] {
                                "" => args.extend(it.by_ref()),
                                "continue" => {
                                    if continue_if_err {
                                        warning!("reuse of --continue flag");
                                    }
                                    continue_if_err = true;
                                },
                                "no-default-link" => {
                                    if no_default_link {
                                        warning!("reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                x => {
                                    error!("unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'p' => {
                                        if profile.is_some() {
                                            warning!("respecification of optimization profile");
                                        }
                                        if let Some(x) = it.next() {
                                            profile = Some(x);
                                        }
                                        else {
                                            error!("expected profile after -p flag");
                                            exit(1)
                                        }
                                    },
                                    'c' => {
                                        if continue_if_err {
                                            warning!("reuse of -c flag");
                                        }
                                        continue_if_err = true;
                                    },
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x);
                                        }
                                        else {
                                            error!("expected library after -l flag");
                                            exit(1)
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x);
                                        }
                                        else {
                                            error!("expected directory after -L flag");
                                            exit(1)
                                        }
                                    },
                                    'h' => {
                                        if let Some(x) = it.next() {
                                            headers.push(x);
                                        }
                                        else {
                                            error!("expected header file after -h flag");
                                            exit(1)
                                        }
                                    },
                                    x => {
                                        error!("unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            error!("respecification of input file");
                            exit(1)
                        }
                        in_file = Some(arg);
                    }
                }
            }
            if !no_default_link {
                if let Some(pwd) = std::env::current_dir().ok().and_then(|pwd| pwd.to_str().map(String::from)) {link_dirs.insert(0, pwd);}
                if let Ok(home) = std::env::var("HOME") {link_dirs.extend_from_slice(&[format!("{home}/.cobalt/packages"), format!("{home}/.local/lib/cobalt"), "/usr/local/lib/cobalt/packages".to_string(), "/usr/lib/cobalt/packages".to_string(), "/lib/cobalt/packages".to_string(), "/usr/local/lib".to_string(), "/usr/lib".to_string(), "/lib".to_string()]);}
                else {link_dirs.extend(["/usr/local/lib/cobalt/packages", "/usr/lib/cobalt/packages", "/lib/cobalt/packages", "/usr/local/lib", "/usr/lib", "/lib"].into_iter().map(String::from));}
            }
            let (in_file, code) = match in_file.as_deref().unwrap_or("-") {
                "-" => {
                    let mut s = String::new();
                    std::io::stdin().read_to_string(&mut s)?;
                    ("<stdin>", s)
                },
                f => {
                    args[0].push_str(f);
                    (f, std::fs::read_to_string(f)?)
                }
            };
            let ink_ctx = inkwell::context::Context::create();
            let mut ctx = cobalt::context::CompCtx::new(&ink_ctx, in_file);
            ctx.flags.dbg_mangle = true;
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let libs = if !linked.is_empty() {
                let (libs, notfound, failed) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
                if failed {exit(99)}
                libs
            } else {vec![]};
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let mut overall_fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let flags = cobalt::Flags::default();
            let files = &mut *FILES.write().unwrap();
            let file = files.add_file(0, in_file.to_string(), code.clone());
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            overall_fail |= fail;
            fail = false;
            if fail && !continue_if_err {exit(101)}
            let (_, errs) = ast.codegen(&ctx);
            overall_fail |= fail;
            fail = false;
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                error!(" {}", msg.to_string());
                exit(101)
            }
            if fail || overall_fail {exit(101)}
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
        "check" => {
            let mut in_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<String> = vec![];
            let mut headers: Vec<&str> = vec![];
            let mut no_default_link = false;
            {
                let mut it = args.iter().skip(2).filter(|x| !x.is_empty());
                while let Some(arg) = it.next() {
                    if arg.as_bytes()[0] == b'-' {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                error!("respecification of input file");
                                exit(1)
                            }
                            in_file = Some("-");
                        }
                        else if arg.as_bytes()[1] == b'-' {
                            match &arg[2..] {
                                "no-default-link" => {
                                    if no_default_link {
                                        warning!("reuse of --no-default-link flag");
                                    }
                                    no_default_link = true;
                                },
                                x => {
                                    error!("unknown flag --{x}");
                                    exit(1)
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x.as_str());
                                        }
                                        else {
                                            error!("expected library after -l flag");
                                            exit(1)
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x.clone());
                                        }
                                        else {
                                            error!("expected directory after -L flag");
                                            exit(1)
                                        }
                                    },
                                    'h' => {
                                        if let Some(x) = it.next() {
                                            headers.push(x.as_str());
                                        }
                                        else {
                                            error!("expected header file after -h flag");
                                            exit(1)
                                        }
                                    },
                                    x => {
                                        error!("unknown flag -{x}");
                                        exit(1)
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            error!("respecification of input file");
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
            let (in_file, code) = if let Some(f) = in_file {(f, std::fs::read_to_string(f)?)}
            else {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                ("<stdin>", s)
            };
            Target::initialize_native(&INIT_NEEDED)?;
            let triple = TargetMachine::get_default_triple();
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
            if let Some(size) = ink_ctx.ptr_sized_int_type(&target_machine.get_target_data(), None).size_of().get_zero_extended_constant() {flags.word_size = size as u16;}
            let ctx = cobalt::context::CompCtx::with_flags(&ink_ctx, in_file, flags);
            ctx.module.set_triple(&triple);
            if !linked.is_empty() {
                let (_, notfound, failed) = libs::find_libs(linked.iter().map(|x| x.to_string()).collect(), &link_dirs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), Some(&ctx))?;
                notfound.iter().for_each(|nf| error!("couldn't find library {nf}"));
                if !notfound.is_empty() {exit(102)}
                if failed {exit(99)}
            }
            for head in headers {
                let mut file = BufReader::new(std::fs::File::open(head)?);
                ctx.load(&mut file)?;
            }
            let mut fail = false;
            let mut stdout = &mut StandardStream::stdout(ColorChoice::Auto);
            let config = term::Config::default();
            let files = &mut *FILES.write().unwrap();
            let file = files.add_file(0, in_file.to_string(), code.clone());
            let (toks, errs) = cobalt::parser::lex(code.as_str(), (file, 0), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &ctx.flags);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {term::emit(&mut stdout, &config, files, &err.0)?; fail |= err.is_err();}
            if let Err(msg) = ctx.module.verify() {
                error!(" {}", msg.to_string());
                exit(101)
            }
            if fail {exit(101)}
        },
        "install" => {
            match package::Package::init_registry() {
                Ok(()) => {},
                Err(package::PackageUpdateError::NoInstallDirectory) => {
                    error!("could not find or infer Cobalt directory");
                    exit(1)
                },
                Err(package::PackageUpdateError::GitError(e)) => {
                    error!("{e}");
                    exit(2)
                }
                Err(package::PackageUpdateError::StdIoError(e)) => {
                    error!("{e}");
                    exit(3)
                }
            };
            let mut good = 0;
            let registry = package::Package::registry();
            for pkg in args.iter().skip(2).filter(|x| !x.is_empty()) {
                if let Some(p) = registry.get(pkg) {
                    match p.install(TargetMachine::get_default_triple().as_str().to_str().unwrap(), None, package::InstallOptions::default()) {
                        Err(package::InstallError::NoInstallDirectory) => panic!("This would only be reachable if $HOME was deleted in a data race, which may or may not even be possible"),
                        Err(package::InstallError::DownloadError(e)) => {
                            error!("{e}");
                            good = 4;
                        },
                        Err(package::InstallError::StdIoError(e)) => {
                            error!("{e}");
                            good = 3;
                        },
                        Err(package::InstallError::GitCloneError(e)) => {
                            error!("{e}");
                            good = 2;
                        },
                        Err(package::InstallError::ZipExtractError(e)) => {
                            error!("{e}");
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
                            error!("could not parse {pkg}'s config file: {e}");
                            good = 8;
                        },
                        Err(package::InstallError::InvalidVersionSpec(_, v)) => {
                            error!("could not parse {pkg}'s dependencies: invalid version spec {v}");
                            good = 9;
                        },
                        Err(package::InstallError::PkgNotFound(p)) => {
                            error!("could not parse {pkg}'s dependencies: can't find package {p}");
                            good = 10;
                        },
                        _ => {}
                    }
                }
                else {
                    error!("couldn't find package {pkg:?}");
                    good = 6;
                }
            }
            exit(good)
        },
        "proj" | "project" => match args[2].as_str() {
            "track" => {
                if args.len() == 3 {
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
                        error!("couldn't find cobalt.toml in currnet or parent directories");
                    }
                }
                else {
                    let mut vec = load_projects()?;
                    for arg in args.into_iter().skip(3).filter(|x| !x.is_empty()) {
                        if arg.as_bytes()[0] == b'-' {
                            error!("'track' subcommand does not accept flags");
                            exit(1);
                        }
                        let mut path: PathBuf = arg.into();
                        if path.is_dir() {path.push("cobalt.toml");}
                        track_project(&toml::from_str::<build::Project>(&std::fs::read_to_string(&path)?)?.name, path, &mut vec);
                    }
                    save_projects(vec)?;
                }
            },
            "untrack" => {
                if args.len() == 3 {
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
                }
                else {
                    let mut vec = load_projects()?;
                    for arg in args.into_iter().skip(3).filter(|x| !x.is_empty()) {
                        if arg.as_bytes()[0] == b'-' {
                            error!("'track' subcommand does not accept flags");
                            exit(1);
                        }
                        let mut path: PathBuf = arg.into();
                        if path.is_dir() {path.push("cobalt.toml");}
                        vec.retain(|[_, p]| Path::new(p) != path);
                    }
                    save_projects(vec)?;
                }
            },
            "list" => {
                let mut machine = false;
                for arg in args.into_iter().skip(3).filter(|x| !x.is_empty()) {
                    if arg.as_bytes()[0] == b'-' && arg.len() > 1 {
                        if arg.as_bytes()[1] == b'-' {
                            if matches!(&arg[2..], "machine" | "machine-readable") {
                                if machine {warning!("reuse of --machine flag")}
                                machine = true;
                            }
                            else {
                                error!("unknown flag {arg}");
                                exit(1);
                            }
                        }
                        else {
                            for c in arg[1..].chars() {
                                if c == 'm' {
                                    if machine {warning!("reuse of --machine flag")}
                                    machine = true;
                                }
                                else {
                                    error!("unknown flag -{c}");
                                    exit(1);
                                }
                            }
                        }
                    }
                    else {
                        error!("positional arguments are not allowed for this command");
                        exit(1);
                    }
                }
                let vecs = load_projects()?;
                if machine {vecs.iter().for_each(|[n, p]| println!("{n}\t{p}"))}
                else {
                    let padding = vecs.iter().map(|[n, _]| n.chars().count()).max().unwrap_or(0);
                    vecs.iter().for_each(|[n, p]| println!("{n}{} => {p}", " ".repeat(padding - n.chars().count())));
                }
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
                    let mut it = args.iter().skip(3).filter(|x| !x.is_empty());
                    while let Some(arg) = it.next() {
                        if arg.as_bytes()[0] == b'-' {
                            if arg.as_bytes().len() == 1 {
                                if project_dir.is_some() {
                                    error!("respecification of project directory");
                                    exit(1)
                                }
                                project_dir = Some("-");
                            }
                            else if arg.as_bytes()[1] == b'-' {
                                match &arg[2..] {
                                    "no-default-link" => {
                                        if no_default_link {
                                            warning!("reuse of --no-default-link flag");
                                        }
                                        no_default_link = true;
                                    },
                                    x => {
                                        error!("unknown flag --{x}");
                                        exit(1)
                                    }
                                }
                            }
                            else {
                                for c in arg.chars().skip(1) {
                                    match c {
                                        'p' => {
                                            if profile.is_some() {
                                                warning!("respecification of optimization profile");
                                            }
                                            if let Some(x) = it.next() {
                                                profile = Some(x.as_str());
                                            }
                                            else {
                                                error!("expected profile after -p flag");
                                                exit(1)
                                            }
                                        },
                                        's' => {
                                            if profile.is_some() {
                                                error!("respecification of source directory");
                                                exit(1)
                                            }
                                            if let Some(x) = it.next() {
                                                source_dir = Some(x.as_str());
                                            }
                                            else {
                                                error!("expected source directory after -s flag");
                                                exit(1)
                                            }
                                        },
                                        'b' => {
                                            if profile.is_some() {
                                                warning!("respecification of build directory");
                                                exit(1)
                                            }
                                            if let Some(x) = it.next() {
                                                build_dir = Some(x.as_str());
                                            }
                                            else {
                                                error!("expected build directory after -b flag");
                                                exit(1)
                                            }
                                        },
                                        't' => {
                                            if profile.is_some() {
                                                warning!("respecification of target triple");
                                                exit(1)
                                            }
                                            if let Some(x) = it.next() {
                                                triple = Some(TargetTriple::create(x.as_str()));
                                            }
                                            else {
                                                error!("expected target triple after -t flag");
                                                exit(1)
                                            }
                                        },
                                        'T' => {
                                            if let Some(x) = it.next() {
                                                targets.push(x.as_str());
                                            }
                                            else {
                                                error!("expected build target after -T flag");
                                                exit(1)
                                            }
                                        },
                                        x => {
                                            error!("unknown flag -{x}");
                                            exit(1)
                                        }
                                    }
                                }
                            }
                        }
                        else {
                            if project_dir.is_some() {
                                error!("respecification of project directory");
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
                            error!("{x} does not exist");
                            exit(100)
                        }
                        match std::fs::metadata(x).map(|x| x.file_type().is_dir()) {
                            Ok(true) => {
                                let mut path = std::path::PathBuf::from(x);
                                path.push("cobalt.toml");
                                if !path.exists() {
                                    error!("cannot find cobalt.toml in {x}");
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
                                let cfg = match toml::from_str::<build::Project>(&cfg) {
                                    Ok(proj) => proj,
                                    Err(e) => {
                                        eprintln!("error when parsing project file: {e}");
                                        exit(100)
                                    }
                                };
                                let mut vecs = load_projects()?;
                                track_project(&cfg.name, x.into(), &mut vecs);
                                save_projects(vecs)?;
                                (cfg , PathBuf::from(x))
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
                                let cfg = match toml::from_str::<build::Project>(&cfg) {
                                    Ok(proj) => proj,
                                    Err(e) => {
                                        eprintln!("error when parsing project file: {e}");
                                        exit(100)
                                    }
                                };
                                let mut vecs = load_projects()?;
                                track_project(&cfg.name, path.clone(), &mut vecs);
                                save_projects(vecs)?;
                                (cfg, path)
                            },
                            Err(e) => {
                                eprintln!("error when determining type of {x}: {e}");
                                exit(100)
                            }
                        }
                    },
                    None => {
                        let cfg;
                        let mut path = std::env::current_dir()?;
                        loop {
                            path.push("cobalt.toml");
                            if path.exists() {break}
                            path.pop();
                            if !path.pop() {
                                error!("couldn't find cobalt.toml in current directory");
                                exit(100)
                            }
                        }
                        match std::fs::read_to_string(&path) {
                            Ok(c) => cfg = c,
                            Err(e) => {
                                eprintln!("error when reading project file: {e}");
                                exit(100)
                            }
                        }
                        path.pop();
                        (match toml::from_str::<build::Project>(cfg.as_str()) {
                            Ok(proj) => proj,
                            Err(e) => {
                                eprintln!("error when parsing project file: {e}");
                                exit(100)
                            }
                        }, path)
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
                exit(build::build(project_data, if targets.is_empty() {None} else {Some(targets.into_iter().map(String::from).collect())}, &build::BuildOptions {
                    source_dir,
                    build_dir: build_dir.as_path(),
                    profile: profile.unwrap_or("default"),
                    triple: &triple.unwrap_or_else(TargetMachine::get_default_triple),
                    continue_build: false,
                    continue_comp: false,
                    link_dirs: link_dirs.iter().map(|x| x.as_str()).collect()
                }));
            },
            x => {
                error!("unknown subcommand '{x}'");
                exit(1);
            }
        },
        x => {
            error!("unknown subcommand '{x}'");
            exit(1);
        }
    };
    Ok(())
}
fn main() {
    if let Err(err) = driver() {
        error!("{err}");
        exit(100);
    }
}
