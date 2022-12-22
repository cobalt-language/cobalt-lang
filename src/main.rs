use colored::Colorize;
use inkwell::targets::*;
use std::process::{Command, exit};
use std::io::Write;
use std::ffi::OsString;
use std::path::PathBuf;
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
fn find_libs<'a>(mut libs: Vec<&'a str>, dirs: Vec<&str>) -> (Vec<PathBuf>, Vec<&'a str>) {
    let mut outs = vec![];
    let it = dirs.into_iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file());
    it.for_each(|x| {
        let path = x.into_path();
        match path.extension().and_then(std::ffi::OsStr::to_str) {
            Some("a") | Some("so") | Some("dylib") | Some("colib") | Some("dll") | Some("lib") => {},
            _ => return
        }
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| x.len() > 0) {
                if lib == &stem || (stem.starts_with("lib") && lib == &&stem[3..]) {
                    outs.push(path.clone());
                    *lib = "";
                }
            }
        }
    });
    (outs, libs.into_iter().filter(|x| x.len() > 0).collect())
}
#[allow(non_snake_case)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ERROR = &"error".bright_red().bold();
    let WARNING = &"warning".bright_yellow().bold();
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
        }
        "aot" => {
            let mut output_type: Option<OutputType> = None;
            let mut in_file: Option<&str> = None;
            let mut out_file: Option<&str> = None;
            let mut linked: Vec<&str> = vec![];
            let mut link_dirs: Vec<&str> = vec![];
            let mut triple: Option<TargetTriple> = None;
            {
                let mut it = args.iter().skip(2).skip_while(|x| x.len() == 0);
                while let Some(arg) = it.next() {
                    if arg.as_bytes()[0] == ('-' as u8) {
                        if arg.as_bytes().len() == 1 {
                            if in_file.is_some() {
                                eprintln!("{ERROR}: respecification of input file");
                                return Ok(())
                            }
                            in_file = Some("-");
                        }
                        if arg.as_bytes()[1] == ('-' as u8) {
                            match &arg[2..] {
                                "emit-asm" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::Assembly);
                                },
                                "emit-obj" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::Object);
                                },
                                "emit-llvm" | "emit-ir" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::LLVM);
                                },
                                "emit-bc" | "emit-bitcode" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::Bitcode);
                                },
                                "lib" | "emit-lib" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::Library);
                                },
                                "exe" | "executable" | "emit-exe" => {
                                    if output_type.is_some() {
                                        eprintln!("{ERROR}: respecification of output type");
                                        return Ok(())
                                    }
                                    output_type = Some(OutputType::Executable);
                                },
                                x => {
                                    eprintln!("{ERROR}: unknown flag --{x}");
                                    return Ok(())
                                }
                            }
                        }
                        else {
                            for c in arg.chars().skip(1) {
                                match c {
                                    'o' => {
                                        if out_file.is_some() {
                                            eprintln!("{ERROR}: respecification of input file");
                                            return Ok(())
                                        }
                                        if let Some(x) = it.next() {
                                            out_file = Some(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected file after -o flag");
                                            return Ok(())
                                        }
                                    },
                                    'l' => {
                                        if let Some(x) = it.next() {
                                            linked.push(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected library after -l flag");
                                            return Ok(())
                                        }
                                    },
                                    'L' => {
                                        if let Some(x) = it.next() {
                                            link_dirs.push(x.as_str());
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected directory after -L flag");
                                            return Ok(())
                                        }
                                    },
                                    't' => {
                                        if triple.is_some() {
                                            eprintln!("{ERROR}: respecification of target triple");
                                            return Ok(())
                                        }
                                        if let Some(x) = it.next().map(|x| TargetTriple::create(x)) {
                                            triple = Some(x);
                                        }
                                        else {
                                            eprintln!("{ERROR}: expected target triple after -t flag");
                                            return Ok(())
                                        }
                                    },
                                    x => {
                                        eprintln!("{ERROR}: unknown flag -{x}");
                                        return Ok(())
                                    }
                                }
                            }
                        }
                    }
                    else {
                        if in_file.is_some() {
                            eprintln!("{ERROR}: respecification of input file");
                            return Ok(())
                        }
                        in_file = Some(arg.as_str());
                    }
                }
            }
            if in_file.is_none() {
                eprintln!("{ERROR}: no input file given");
                return Ok(())
            }
            let in_file = in_file.unwrap();
            let code = std::fs::read_to_string(in_file)?;
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
            let (toks, mut errs) = cobalt::parser::lexer::lex(code.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
            let (ast, mut es) = cobalt::parser::ast::parse(toks.as_slice(), &flags);
            errs.append(&mut es);
            let ink_ctx = inkwell::context::Context::create();
            let ctx = cobalt::context::CompCtx::new(&ink_ctx, fname.as_str());
            let (_, mut es) = ast.codegen(&ctx);
            errs.append(&mut es);
            for err in errs {
                eprintln!("{}: {:#}: {}", if err.code < 100 {WARNING} else {ERROR}, err.loc, err.message);
                for note in err.notes {
                    eprintln!("\t{}: {:#}: {}", "note".bold(), note.loc, note.message);
                }
            }
            match output_type {
                OutputType::LLVM => write!(out, "{}", ctx.module.to_string())?,
                OutputType::Bitcode => out.write_all(ctx.module.write_bitcode_to_memory().as_slice())?,
                _ => {
                    let target_machine = Target::from_triple(&triple).unwrap().create_target_machine(
                        &triple,
                        "",
                        "",
                        inkwell::OptimizationLevel::Default,
                        inkwell::targets::RelocMode::Default,
                        inkwell::targets::CodeModel::Default
                    ).expect("failed to create target machine");
                    if output_type == OutputType::Assembly {
                        out.write_all(target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Assembly).unwrap().as_slice())?;
                        return Ok(())
                    }
                    let mb = target_machine.write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object).unwrap();
                    match output_type {
                        OutputType::Executable => {
                            out.write_all(mb.as_slice())?;
                            let mut args = vec![OsString::from("-o"), OsString::from(out_file)];
                            let (libs, notfound) = find_libs(linked, link_dirs);
                            for nf in notfound.iter() {
                                eprintln!("couldn't find library {nf}");
                            }
                            if notfound.len() > 0 {return Ok(())}
                            for lib in libs {
                                let parent = lib.parent().unwrap().as_os_str().to_os_string();
                                args.push(OsString::from("-L"));
                                args.push(parent.clone());
                                args.push(OsString::from("-rpath"));
                                args.push(parent);
                                args.push(lib.file_name().unwrap().to_os_string());
                            }
                            exit(Command::new("ld").args(args).status().ok().and_then(|x| x.code()).unwrap_or(0))
                        },
                        OutputType::Library => {
                            writeln!(out, "!<arch>")?;
                            writeln!(out, "file.o          0           0     0     644     {: >10}`", mb.get_size())?;
                            out.write(mb.as_slice())?;
                            writeln!(out)?;
                            let mut buff = format!("{in_file}\00.1.0\0");
                            let (libs, notfound) = find_libs(linked, link_dirs);
                            for nf in notfound.iter() {
                                eprintln!("couldn't find library {nf}");
                            }
                            if notfound.len() > 0 {return Ok(())}
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
        }
        x => {
            eprintln!("unknown subcommand '{}'", x);
        }
    };
    Ok(())
}
