use std::io::{self, prelude::*};
pub fn save_str(buf: &mut dyn Write, val: &str) -> io::Result<()> {
    buf.write_all(val.as_bytes())?;
    buf.write_all(&[0])
}
pub fn load_str(buf: &mut dyn BufRead) -> io::Result<String> {
    let mut vec = vec![];
    buf.read_until(0, &mut vec)?;
    if vec.last() == Some(&0) {
        vec.pop();
    }
    String::from_utf8(vec).map_err(invalid_data)
}
pub fn invalid_data(e: impl std::error::Error + Send + Sync + 'static) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}
