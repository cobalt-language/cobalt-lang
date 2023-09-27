#[cfg(feature = "lld")]
fn main() {
    println!("cargo:rerun-if-changed=cxx");
    println!("cargo:rerun-if-changed=src/lld.rs");
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
        println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
    }
}
#[cfg(not(feature = "lld"))]
fn main() {
    println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
}
