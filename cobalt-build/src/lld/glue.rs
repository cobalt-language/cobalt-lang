#[cxx::bridge(namespace = "llvm")]
pub mod ffi {
    extern "Rust" {
        type LldReturn;
        fn set_ret_code(ret: &mut LldReturn, code: i32);
        fn set_ret_gain(ret: &mut LldReturn, again: bool);
    }
    unsafe extern "C++" {
        include!("llvm/Support/raw_ostream.h");
        include!("cobalt-build/cxx/lld.hpp");
        type raw_ostream;
        unsafe fn lld_entry(
            argv: &[*const c_char],
            stdout: Pin<&mut raw_ostream>,
            stderr: Pin<&mut raw_ostream>,
            ret: Pin<&mut LldReturn>,
        );
        fn outs_ptr() -> *mut raw_ostream;
        fn errs_ptr() -> *mut raw_ostream;
        fn string_ostream(buf: Pin<&mut CxxString>) -> UniquePtr<raw_ostream>;
        fn file_ostream(path: &[u8]) -> Result<UniquePtr<raw_ostream>>;
    }
}
#[derive(Default, Debug, Clone, Copy)]
pub struct LldReturn {
    pub code: i32,
    pub again: bool,
}
fn set_ret_code(ret: &mut LldReturn, code: i32) {
    ret.code = code;
}
fn set_ret_gain(ret: &mut LldReturn, again: bool) {
    ret.again = again;
}
