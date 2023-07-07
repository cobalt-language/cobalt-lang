#[cfg(feature = "llvm-15")]
pub use llvm_sys_15 as llvm_sys;

#[cfg(feature = "llvm-16")]
pub use llvm_sys_16 as llvm_sys;

pub use inkwell;

#[cfg(not(any(feature = "llvm-15", feature = "llvm-16")))]
compile_error!("Either LLVM 15 or LLVM 16 must be specified!");

#[cfg(feature = "llvm-15")]
#[macro_export]
macro_rules! if_llvm_15 {
    ($($tok:tt)*) => {$($tok)*}
}

#[cfg(not(feature = "llvm-15"))]
#[macro_export]
macro_rules! if_llvm_15 {
    ($($tok:tt)*) => {}
}

#[cfg(feature = "llvm-16")]
#[macro_export]
macro_rules! if_llvm_16 {
    ($($tok:tt)*) => {$($tok)*}
}

#[cfg(not(feature = "llvm-16"))]
#[macro_export]
macro_rules! if_llvm_16 {
    ($($tok:tt)*) => {}
}