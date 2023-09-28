#[cfg(all(feature = "disble-lld", feature = "force-lld"))]
compile_error!("force-lld and disable-lld features are mutually exclusive");
#[cfg(all(feature = "lld", not(feature = "disable-lld")))]
fn main() {
    println!("cargo:rerun-if-changed=cxx");
    println!("cargo:rerun-if-changed=src/lld/glue.rs");
    if cxx_build::bridge("src/lld/glue.rs")
        .file("cxx/lld.cpp")
        .include(cobalt_llvm::LLVM_INCLUDE_DIR)
        .flag_if_supported("-std=c++17")
        .warnings(false)
        .try_compile("lld-rs")
        .is_ok()
    {
        println!("cargo:rustc-cfg=lld_enabled");
        println!("cargo:rustc-link-search={}", cobalt_llvm::LLVM_LIBS_DIR);
        println!("cargo:rustc-link-lib=lldCommon");
        println!("cargo:rustc-link-lib=lldCOFF");
        println!("cargo:rustc-link-lib=lldELF");
        println!("cargo:rustc-link-lib=lldMachO");
        println!("cargo:rustc-link-lib=lldWasm");
    } else {
        #[cfg(feature = "force-lld")]
        panic!("LLD build failed");
        println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
    }
}
#[cfg(any(not(feature = "lld"), feature = "disable-lld"))]
fn main() {
    println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
}
