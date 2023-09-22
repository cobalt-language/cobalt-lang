#[cfg(feature = "lld")]
fn main() {
    println!("carg:rerun-if-changed=cxx");
    println!("carg:rerun-if-changed=src/lld.rs");
    if cxx_build::bridge("src/lld/glue.rs")
        .file("cxx/lld.cpp")
        .include(cobalt_llvm::LLVM_INCLUDE_DIR)
        .flag_if_supported("-std=c++17")
        .try_compile("lld-rs")
        .is_ok()
    {
        println!("cargo:rustc-cfg=lld_enabled");
    } else {
        println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
    }
}
#[cfg(not(feature = "lld"))]
fn main() {
    println!("cargo:rustc-env=HOST={}", std::env::var("TARGET").unwrap());
}
