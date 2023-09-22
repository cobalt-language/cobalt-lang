use cxx::UniquePtr;
use std::pin::Pin;
use std::sync::Mutex;
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
pub use glue::LldReturn;
pub static LLD_CAN_RUN: Mutex<bool> = Mutex::new(true);
pub fn lld_link<'a, I: IntoIterator<Item = &'a str>>(
    args: I,
    stdout: Option<Pin<&mut OStream>>,
    stderr: Option<Pin<&mut OStream>>,
) -> i32 {
    let mut lock = LLD_CAN_RUN.lock().unwrap();
    if !*lock {
        std::mem::drop(lock);
        panic!("LLD is in an invalid state and cannot run!")
    }
    let mut ret = LldReturn::default();
    unsafe {
        use std::ffi::CStr;
        #[allow(clippy::redundant_closure)]
        glue::ffi::lld_entry(
            &args
                .into_iter()
                .map(|c| CStr::from_bytes_until_nul(c.as_bytes()).unwrap().as_ptr())
                .collect::<Vec<_>>(),
            stdout.unwrap_or_else(|| llvm_ostream::outs()),
            stderr.unwrap_or_else(|| llvm_ostream::errs()),
            Pin::new(&mut ret),
        );
    }
    *lock = ret.again;
    ret.code
}
