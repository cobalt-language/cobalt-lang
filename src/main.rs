use colored::Colorize;
const HELP: &str = "co- Cobalt compiler and build system
A program can be compiled using the `co aot' subcommand, or JIT compiled using the `co jit' subcommand";
static mut FILENAME: String = String::new();
fn main() -> Result<(), Box<dyn std::error::Error>> {
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
                                    println!("{}: reuse of -c flag", "warning".yellow().bold())
                                }
                                nfcl = true;
                            }
                            'l' => {
                                if loc {
                                    println!("{}: reuse of -l flag", "warning".yellow().bold())
                                }
                                loc = true;
                            },
                            x => eprintln!("{}: unknown flag -{}", "warning".yellow().bold(), x)
                        }
                    }
                }
                else if nfcl {
                    let flags = cobalt::Flags::default();
                    nfcl = false;
                    let (toks, errs) = cobalt::parser::lex(arg.as_str(), cobalt::Location::from_name("<command line>"), &flags);
                    for err in errs {
                        println!("{}: {}: {}", if err.code < 100 {"warning".yellow().bold()} else {"error".red().bold()}, err.loc, err.message);
                        for note in err.notes {
                            println!("{}: {}: {}", "note".bold(), note.loc, note.message);
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
                else {
                    let flags = cobalt::Flags::default();
                    let fname = unsafe {&mut FILENAME};
                    *fname = arg;
                    let (toks, errs) = cobalt::parser::lex(std::fs::read_to_string(fname.clone())?.as_str(), cobalt::Location::from_name(fname.as_str()), &flags);
                    for err in errs {
                        println!("{}: {}: {}", if err.code < 100 {"warning".yellow().bold()} else {"error".red().bold()}, err.loc, err.message);
                        for note in err.notes {
                            println!("{}: {}: {}", "note".bold(), note.loc, note.message);
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
                println!("{}: -c switch must be followed by code", "error".red().bold());
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
                                    println!("{}: reuse of -c flag", "warning".yellow().bold())
                                }
                                nfcl = true;
                            }
                            'l' => {
                                if loc {
                                    println!("{}: reuse of -l flag", "warning".yellow().bold())
                                }
                                loc = true;
                            },
                            x => eprintln!("{}: unknown flag -{}", "warning".yellow().bold(), x)
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
                        println!("{}: {}: {}", if err.code < 100 {"warning".yellow().bold()} else {"error".red().bold()}, err.loc, err.message);
                        for note in err.notes {
                            println!("{}: {}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    if loc {
                        println!("{:#}", ast)
                    }
                    else {
                        println!("{}", ast)
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
                        println!("{}: {}: {}", if err.code < 100 {"warning".yellow().bold()} else {"error".red().bold()}, err.loc, err.message);
                        for note in err.notes {
                            println!("{}: {}: {}", "note".bold(), note.loc, note.message);
                        }
                    }
                    if loc {
                        println!("{:#}", ast)
                    }
                    else {
                        println!("{}", ast)
                    }
                }
            }
            if nfcl {
                println!("{}: -c switch must be followed by code", "error".red().bold());
            }
        }
        x @ _ => {
            println!("unknown subcommand '{}'", x);
        }
    };
    Ok(())
}
