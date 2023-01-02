use colored::Colorize;
use inkwell::targets::*;
use std::process::{Command, exit};
use std::io::{Read, Write};
use std::ffi::OsString;
mod libs;
#[allow(dead_code)]
mod jit;
mod opt;
const HELP: &str = "co- Cobalt compiler and build system
A program can be compiled using the `co aot' subcommand, or JIT compiled using the `co jit' subcommand";
static mut FILENAME: String = String::new();
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
    asm_parser: false,
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
            println!("{}", HELP);
        },
        "version" | "--version" | "-v" | "-V" => {
            println!("Cobalt version {} using LLVM version {}", env!("CARGO_PKG_VERSION"), "14.0.1.6");
        }
        "lex" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut loc = false;
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
                            'l' => {
                                if loc {
                                    eprintln!("{WARNING}: reuse of -l flag");
                                }
                                loc = true;
                            },
                            x => eprintln!("{WARNING}: unknown flag -{x}")
                        }
                    }
                }
                else if nfcl {
                    let flags = cobalt::Flags::default();
                    nfcl = false;
                    let (toks, errs) = cobalt::parser::lex(arg.as_str(), cobalt::Location::from_name("<command line>"), &flags);
                    for err in errs {
                        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {ERROR}, err.loc, err.message);
                        for note in err.notes {
                            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    for tok in toks {
                        if loc {
                            eprintln!("{:#}", tok)
                        }
                        else {
                            eprintln!("{}", tok)
                        }
                    }
                }
                else {
                    let flags = cobalt::Flags::default();
                    let fname = unsafe {&mut FILENAME};
                    *fname = arg;
                    let (toks, errs) = cobalt::parser::lex(std::fs::read_to_string(fname.clone())?.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
                    for err in errs {
                        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {ERROR}, err.loc, err.message);
                        for note in err.notes {
                            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    for tok in toks {
                        if loc {
                            println!("{:#}", tok)
                        }
                        else {
                            println!("{}", tok)
                        }
                    }
                }
            }
            if nfcl {
                eprintln!("{ERROR}: -c switch must be followed by code");
            }
        },
        "parse" if cfg!(debug_assertions) => {
            let mut nfcl = false;
            let mut loc = false;
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
                    let flags = cobalt::Flags::default();
                    let (toks, mut errs) = cobalt::parser::lex(arg.as_str(), cobalt::Location::from_name("<command line>"), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {
                        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {ERROR}, err.loc, err.message);
                        for note in err.notes {
                            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    if loc {
                        print!("{:#}", ast)
                    }
                    else {
                        print!("{}", ast)
                    }
                }
                else {
                    let flags = cobalt::Flags::default();
                    let fname = unsafe {&mut FILENAME};
                    *fname = arg;
                    let (toks, mut errs) = cobalt::parser::lex(std::fs::read_to_string(fname.clone())?.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
                    let (ast, mut es) = cobalt::parser::parse(toks.as_slice(), &flags);
                    errs.append(&mut es);
                    for err in errs {
                        eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {ERROR}, err.loc, err.message);
                        for note in err.notes {
                            eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    if loc {
                        print!("{:#}", ast)
                    }
                    else {
                        print!("{}", ast)
                    }
                }
            }
            if nfcl {
                eprintln!("{ERROR}: -c switch must be followed by code");
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
            let fname = unsafe {&mut FILENAME};
            *fname = in_file.to_string();
            let flags = cobalt::Flags::default();
            let (toks, errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname), &flags);
            let mut fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, fname.as_str());
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                fail = true;
            }
            print!("{}", ctx.module.to_string());
            exit(if fail {101} else {0})
        },
        "aot" => {
            let mut output_type: Option<OutputType> = None;
            let mut in_file: Option<&str> = None;
            let mut out_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<&str> = vec![];
            let mut triple: Option<TargetTriple> = None;
            let mut continue_if_err = false;
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
                                            link_dirs.push(x.as_str());
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
            let out_file = out_file.map(String::from).unwrap_or_else(|| match output_type {
                OutputType::Executable => "a.out".to_string(),
                OutputType::Library => format!("lib{}.colib", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Object => format!("{}.o", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Assembly => format!("{}.s", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::LLVM => format!("{}.ll", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file)),
                OutputType::Bitcode => format!("{}.bc", in_file.rfind('.').map(|i| &in_file[..i]).unwrap_or(in_file))
            });
            let mut out = if out_file == "-" {Box::new(std::io::stdout()) as Box<dyn Write>} else {Box::new(std::fs::File::create(out_file.as_str())?) as Box<dyn Write>};
            if triple.is_some() {Target::initialize_all(&INIT_NEEDED)}
            else {Target::initialize_native(&INIT_NEEDED)?}
            let triple = triple.unwrap_or_else(TargetMachine::get_default_triple);
            let flags = cobalt::Flags::default();
            let fname = unsafe {&mut FILENAME};
            *fname = in_file.to_string();
            let mut fail = false;
            let mut overall_fail = false;
            let (toks, errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, fname.as_str());
            ctx.module.set_triple(&triple);
            let (_, errs) = ast.codegen(&ctx);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                exit(101)
            }
            if overall_fail {exit(101)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile, &pm);
            pm.run_on(&ctx.module);
            match output_type {
                OutputType::LLVM => write!(out, "{}", ctx.module.to_string())?,
                OutputType::Bitcode => out.write_all(ctx.module.write_bitcode_to_memory().as_slice())?,
                _ => {
                    let target_machine = Target::from_triple(&triple).unwrap().create_target_machine(
                        &triple,
                        "",
                        "",
                        inkwell::OptimizationLevel::None,
                        inkwell::targets::RelocMode::PIC,
                        inkwell::targets::CodeModel::Small
                    ).expect("failed to create target machine");
                    if output_type == OutputType::Assembly {
                        out.write_all(target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Assembly).unwrap().as_slice())?;
                        return Ok(())
                    }
                    let mb = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object).unwrap();
                    match output_type {
                        OutputType::Executable => {
                            out.write_all(mb.as_slice())?;
                            let mut args = vec![OsString::from(out_file.clone()), OsString::from("-o"), OsString::from(out_file)];
                            let (libs, notfound) = libs::find_libs(linked, link_dirs);
                            for nf in notfound.iter() {
                                eprintln!("couldn't find library {nf}");
                            }
                            if notfound.len() > 0 {exit(102)}
                            for lib in libs {
                                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                                args.push(OsString::from("-L"));
                                args.push(parent.clone());
                                args.push(OsString::from("-rpath"));
                                args.push(parent);
                                args.push(OsString::from((std::borrow::Cow::Borrowed("-l:") + lib.file_name().unwrap().to_string_lossy()).into_owned()));
                            }
                            args.extend(linker_args.into_iter().map(OsString::from));
                            exit(Command::new("ld").args(args).status().ok().and_then(|x| x.code()).unwrap_or(0))
                        },
                        OutputType::Library => {
                            writeln!(out, "!<arch>")?;
                            writeln!(out, "file.o          0           0     0     644     {: >10}`", mb.get_size())?;
                            out.write(mb.as_slice())?;
                            writeln!(out)?;
                            let mut buff = format!("{in_file}\00.1.0\0");
                            let (libs, notfound) = libs::find_libs(linked, link_dirs);
                            for nf in notfound.iter() {
                                eprintln!("couldn't find library {nf}");
                            }
                            if notfound.len() > 0 {exit(102)}
                            for lib in libs {
                                buff += lib.to_str().expect("library path must be valid UTF-8");
                                buff.push('\0');
                            }
                            buff.push('\0');
                            writeln!(out, ".colib          0           0     0     644     {: >10}`\n{}", buff.as_bytes().len(), buff)?;
                            exit(Command::new("ranlib").arg(out_file).status().ok().and_then(|x| x.code()).unwrap_or(0));
                        },
                        OutputType::Object => out.write_all(mb.as_slice())?,
                        x => panic!("{x:?} has already been handled")
                    }
                }
            }
        },
        "jit" => {
            let mut in_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<&str> = vec![];
            let mut continue_if_err = false;
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
                                            link_dirs.push(x.as_str());
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
            let (in_file, code) = if in_file.is_none() {
                let mut s = String::new();
                std::io::stdin().read_to_string(&mut s)?;
                ("<stdin>", s)
            }
            else {
                let f = in_file.unwrap();
                (f, std::fs::read_to_string(f)?)
            };
            let flags = cobalt::Flags::default();
            let fname = unsafe {&mut FILENAME};
            *fname = in_file.to_string();
            let mut fail = false;
            let mut overall_fail = false;
            let (toks, errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            let ink_ctx = inkwell::context::Context::create();
            let mut ctx = cobalt::context::CompCtx::new(&ink_ctx, fname.as_str());
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail && !continue_if_err {exit(101)}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
                exit(101)
            }
            if overall_fail {exit(101)}
            let pm = inkwell::passes::PassManager::create(());
            opt::load_profile(profile, &pm);
            pm.run_on(&ctx.module);
            let (libs, notfound) = libs::find_libs(linked, link_dirs);
            for nf in notfound.iter() {
                eprintln!("couldn't find library {nf}");
            }
            if notfound.len() > 0 {exit(102)}
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
            let flags = cobalt::Flags::default();
            let fname = unsafe {&mut FILENAME};
            *fname = in_file.to_string();
            let mut fail = false;
            let mut overall_fail = false;
            let (toks, errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail {eprintln!("lexing failed, the following errors might be incorrect")}
            let (ast, errs) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail {eprintln!("parsing failed, the following errors might be incorrect")}
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, fname.as_str());
            ctx.module.set_triple(&TargetMachine::get_default_triple());
            let (_, errs) = ast.codegen(&ctx);
            fail = false;
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {fail = true; overall_fail = true; ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            if fail {eprintln!("code generation failed, the following errors might be incorrect")}
            if let Err(msg) = ctx.module.verify() {
                eprintln!("{ERROR}: {MODULE}: {}", msg.to_string());
            }
            exit(if overall_fail {101} else {0})
        },
        x => {
            eprintln!("unknown subcommand '{}'", x);
        }
    };
    Ok(())
}
