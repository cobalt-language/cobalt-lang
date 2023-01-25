use llvm_sys::orc2::{*, lljit::*};
use llvm_sys::error::*;
use inkwell::LLVMReference;
use std::ffi::CString;
use std::rc::Rc;
pub type JDL = LLVMOrcJITDylibRef;
#[derive(Clone)]
pub struct LLJIT<'ctx>(Rc<LLVMOrcLLJITRef>, std::marker::PhantomData<&'ctx inkwell::context::Context>);
impl LLJIT<'_> {
    pub fn new() -> Self {
        unsafe {
            let mut jit = std::ptr::null_mut();
            let builder = LLVMOrcCreateLLJITBuilder();
            LLVMConsumeError(LLVMOrcCreateLLJIT(&mut jit as *mut LLVMOrcLLJITRef, builder));
            LLVMOrcDisposeLLJITBuilder(builder);
            let this = LLJIT(Rc::new(jit), std::marker::PhantomData);
            this
        }
    }
    pub fn add_module(&self, jdl: JDL, module: inkwell::module::Module) {
        unsafe {
            LLVMConsumeError(LLVMOrcLLJITAddLLVMIRModule(*self.0, jdl, LLVMOrcCreateNewThreadSafeModule(module.get_ref(), LLVMOrcCreateNewThreadSafeContext())));
        }
    }
    pub fn add_object(&self, jdl: JDL, obj: inkwell::memory_buffer::MemoryBuffer) {
        unsafe {
            LLVMConsumeError(LLVMOrcLLJITAddObjectFile(*self.0, jdl, obj.get_ref()));
        }
    }
    pub fn add_static(&self, jdl: JDL, path: &str) {
        unsafe {
            let mut gen = std::ptr::null_mut();
            LLVMConsumeError(LLVMOrcCreateStaticLibrarySearchGeneratorForPath(
                &mut gen as *mut _,
                LLVMOrcLLJITGetObjLinkingLayer(*self.0),
                path.as_ptr(),
                inkwell::targets::TargetMachine::get_default_triple().as_ptr()
            ));
            LLVMOrcJITDylibAddGenerator(jdl, gen);
        }
    }
    pub fn add_shared(&self, jdl: JDL, path: &str) {
        unsafe {
            let mut gen = std::ptr::null_mut();
            LLVMConsumeError(LLVMOrcCreateDynamicLibrarySearchGeneratorForPath(
                &mut gen as *mut _,
                path.as_ptr() ,
                0, None, std::ptr::null_mut()
            ));
            LLVMOrcJITDylibAddGenerator(jdl, gen);
        }
    }
    pub fn main(&self) -> JDL {
        unsafe {
            LLVMOrcLLJITGetMainJITDylib(*self.0)
        }
    }
    pub fn lookup_lib(&self, name: &CString) -> Option<JDL> {
        unsafe {
            const NULL: *mut LLVMOrcOpaqueJITDylib = 0 as *mut _;
            let es = LLVMOrcLLJITGetExecutionSession(*self.0);
            match LLVMOrcExecutionSessionGetJITDylibByName(es, name.as_bytes().as_ptr()) {
                NULL => None,
                x => Some(x)
            }
        }
    }
    pub fn lookup_or_insert_lib(&self, name: &CString) -> JDL {
        unsafe {
            let es = LLVMOrcLLJITGetExecutionSession(*self.0);
            let mut jdl = LLVMOrcExecutionSessionGetJITDylibByName(es, name.as_bytes().as_ptr());
            if jdl.is_null() {
                jdl = LLVMOrcExecutionSessionCreateBareJITDylib(es, name.as_bytes().as_ptr());
            }
            jdl
        }
    }
    pub fn lookup_main<'jit, T>(&'jit self, name: &CString) -> Option<&'jit T> {
        let mut addr = 0u64;
        unsafe {
            LLVMConsumeError(LLVMOrcLLJITLookup(*self.0, &mut addr as *mut u64, name.as_bytes().as_ptr()));
            (addr as *const T).as_ref()
        }
    }
}
impl Drop for LLJIT<'_> {
    fn drop(&mut self) {
        if Rc::strong_count(&self.0) == 1 {
            unsafe {
                LLVMConsumeError(LLVMOrcDisposeLLJIT(*self.0));
            }
        }
    }
}
