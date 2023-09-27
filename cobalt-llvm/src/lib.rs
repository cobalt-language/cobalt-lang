#[cfg(feature = "llvm-15")]
pub use llvm_sys_15 as llvm_sys;

#[cfg(feature = "llvm-16")]
pub use llvm_sys_16 as llvm_sys;

pub use inkwell;

pub const LLVM_VERSION: &str = env!("LLVM_VERSION");
pub const LLVM_INCLUDE_DIR: &str = env!("LLVM_INCLUDE_DIR");
pub const LLVM_LIBS_DIR: &str = env!("LLVM_LIBS_DIR");

#[cfg(feature = "llvm-15")]
#[macro_export]
macro_rules! if_llvm_15 {
    ($($tok:tt)*) => {$($tok)*}
}

#[cfg(not(feature = "llvm-15"))]
#[macro_export]
macro_rules! if_llvm_15 {
    ($($tok:tt)*) => {};
}

#[cfg(feature = "llvm-16")]
#[macro_export]
macro_rules! if_llvm_16 {
    ($($tok:tt)*) => {$($tok)*}
}

#[cfg(not(feature = "llvm-16"))]
#[macro_export]
macro_rules! if_llvm_16 {
    ($($tok:tt)*) => {};
}
