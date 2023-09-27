#[cxx::bridge(namespace = "llvm")]
pub mod ffi {
    enum LldFlavor {
        Elf,
        Coff,
        MachO,
        Wasm,
    }
    unsafe extern "C++" {
        include!("llvm/Support/raw_ostream.h");
        include!("cobalt-build/cxx/lld.hpp");
        type raw_ostream;
        type LldFlavor;
        unsafe fn lld_entry(
            argv: &mut [*const c_char],
            stdout: Pin<&mut raw_ostream>,
            stderr: Pin<&mut raw_ostream>,
            flavor: LldFlavor,
        ) -> i32;
        fn outs_ptr() -> *mut raw_ostream;
        fn errs_ptr() -> *mut raw_ostream;
        fn string_ostream(buf: Pin<&mut CxxString>) -> UniquePtr<raw_ostream>;
        fn file_ostream(path: &[u8]) -> Result<UniquePtr<raw_ostream>>;
    }
}
