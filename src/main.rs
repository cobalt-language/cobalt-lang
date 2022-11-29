use colored::Colorize;
const HELP: &str = "co- Cobalt compiler and build system
A program can be compiled using the `co aot' subcommand, or JIT compiled using the `co jit' subcommand";

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        println!("{}", HELP);
        return;
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
            for arg in args.into_iter().skip(2) {
                if nfcl {
                    nfcl = false;
                    let (outs, errs) = cobalt::parser::lex(arg.as_str(), cobalt::Location::from_name("<command line>"), cobalt::Flags::default());
                    for err in errs {
                        println!("{}: {}:{}:{}: {}", if err.code < 100 {"warning".yellow().bold()} else {"error".red().bold()}, err.loc.file, err.loc.line, err.loc.col, err.message)
                    }
                    for tok in outs {
                        println!("{}:{}:{}: {:?}", tok.loc.file, tok.loc.line, tok.loc.col, tok.data)
                    }
                }
                else if arg == "-c" {nfcl = true;}
            }
        }
        x @ _ => {
            println!("unknown subcommand `{}'", x);
        }
    };
}
