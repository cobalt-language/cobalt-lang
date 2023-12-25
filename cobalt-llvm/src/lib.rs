pub use inkwell;
#[cfg(feature = "llvm-15")]
pub use llvm_sys_15 as llvm_sys;
#[cfg(feature = "llvm-16")]
pub use llvm_sys_16 as llvm_sys;

#[cfg(all(feature = "llvm-15", feature = "llvm-16"))]
compile_error!("LLVM 15 and 16 cannot both be enabled!");

pub const LLVM_VERSION: &str = env!("LLVM_VERSION");
