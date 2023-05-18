#[macro_export]
macro_rules! error {
    ($($toks:tt)*) => {{
        use std::io::Write;
        use termcolor::{StandardStream, ColorChoice, ColorSpec, Color, WriteColor};
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        stderr.set_color(&ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true)).unwrap();
        write!(stderr, "error").unwrap();
        stderr.reset().unwrap();
        write!(stderr, ": ").unwrap();
        writeln!(stderr, $($toks)*).unwrap();
    }}
}
#[macro_export]
macro_rules! warning {
    ($($toks:tt)*) => {{
        use std::io::Write;
        use termcolor::{StandardStream, ColorChoice, ColorSpec, Color, WriteColor};
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        stderr.set_color(&ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true)).unwrap();
        write!(stderr, "warning").unwrap();
        stderr.reset().unwrap();
        write!(stderr, ": ").unwrap();
        writeln!(stderr, $($toks)*).unwrap();
    }}
}
