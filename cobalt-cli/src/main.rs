use cobalt_cli::prelude::*;
fn main() {
    if let Err(err) = driver(Cli::parse()) {
        match err.downcast() {
            Ok(Exit(code)) => std::process::exit(code),
            Err(err) => {
                if let Err(e2) = cobalt_errors::try_error!("{err:#}") {
                    eprintln!("error encountered printing error: {e2}");
                    eprintln!("original error (debug): {err:#?}");
                }
                std::process::exit(101);
            }
        }
    }
}
