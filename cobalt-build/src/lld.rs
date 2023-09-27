use cxx::UniquePtr;
use std::ffi::*;
use std::pin::Pin;
mod glue;
pub mod llvm_ostream {
    use super::*;
    pub use glue::ffi::string_ostream as from_string;
    pub fn outs() -> Pin<&'static mut OStream> {
        unsafe { Pin::new_unchecked(&mut *glue::ffi::outs_ptr()) }
    }
    pub fn errs() -> Pin<&'static mut OStream> {
        unsafe { Pin::new_unchecked(&mut *glue::ffi::errs_ptr()) }
    }
    pub fn file_ostream<P: AsRef<[u8]>>(path: P) -> Result<UniquePtr<OStream>, cxx::Exception> {
        glue::ffi::file_ostream(path.as_ref())
    }
}
pub use glue::ffi::raw_ostream as OStream;
pub use glue::ffi::LldFlavor;
pub fn lld_link<I: IntoIterator<Item = T>, T: Into<Vec<u8>>>(
    args: I,
    stdout: Option<Pin<&mut OStream>>,
    stderr: Option<Pin<&mut OStream>>,
    flavor: LldFlavor,
) -> i32 {
    let args = args
        .into_iter()
        .map(CString::new)
        .collect::<Result<Vec<_>, NulError>>()
        .expect("Arguments to lld_link cannot contain null!");
    unsafe {
        #[allow(clippy::redundant_closure)] // lifetime error if a closure is not used
        glue::ffi::lld_entry(
            &mut args.iter().map(|c| c.as_ptr()).collect::<Vec<_>>(),
            stdout.unwrap_or_else(|| llvm_ostream::outs()),
            stderr.unwrap_or_else(|| llvm_ostream::errs()),
            flavor,
        )
    }
}
