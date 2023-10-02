use cobalt_cli::prelude::*;
fn main() {
    if let Err(err) = driver(Cli::parse()) {
        match err.downcast() {
            Ok(Exit(code)) => std::process::exit(code),
            Err(err) => {
                cobalt_errors::error!("{err:#}");
                std::process::exit(101);
            }
        }
    }
}
