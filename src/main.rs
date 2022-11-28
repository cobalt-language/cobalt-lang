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
        x @ _ => {
            println!("unknown subcommand `{}'", x);
        }
    };
}
