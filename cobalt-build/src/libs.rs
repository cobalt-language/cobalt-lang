use std::path::Path;
use std::fmt;
use os_str_bytes::OsStrBytes;
use object::{SectionKind, write::Object};
use cobalt_ast::CompCtx;
/// This is a list of all symbols defined in multiple files
#[derive(Debug)]
pub struct ConflictingDefs(pub Vec<String>);
impl fmt::Display for ConflictingDefs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.iter().try_for_each(|v| writeln!(f, "conflicting defintions for {v}"))
    }
}
impl std::error::Error for ConflictingDefs {}

/// Create a new (write) Object for the given triple
pub fn new_object<'a>(triple: &inkwell::targets::TargetTriple) -> Object<'a> {
    let triple = triple.as_str().to_str().unwrap();
    let components = triple.split('-').collect::<Vec<&str>>();
    use object::Architecture::*;
    use object::Endianness::*;
    use object::BinaryFormat::*;
    let mut wasm = false;
    let arch = match components.first().copied() {
        Some("aarch64") => Aarch64,
        Some(x) if x.starts_with("arm64") => Aarch64,
        Some(x) if x.starts_with("arm") => Arm,
        Some("x86" | "i386" | "i586" | "i686") => I386,
        Some("mips" | "mipsel") => Mips,
        Some("mips64" | "mips64el") => Mips64,
        Some("powerpc") => PowerPc,
        Some("powerpc64") => PowerPc64,
        Some(x) if x.starts_with("riscv32") => Riscv32,
        Some(x) if x.starts_with("riscv64") => Riscv64,
        Some("x86_64") => X86_64,
        Some("wasm" | "wasm32") => {wasm = true; Wasm32}
        _ => Unknown
    };
    let endian = match arch {
        Mips | Mips64 => Big,
        PowerPc | PowerPc64 | Arm | Aarch64 | Wasm32 | X86_64 | I386 => Little,
        _ => if 1i16.to_be_bytes()[0] == 1 {Little} else {Big}
    };
    let format = if wasm {Wasm} else {
        match components.get(1).copied() {
            Some("apple") => MachO,
            Some("linux") => Elf,
            _ => match components.get(2).copied() {
                Some("apple" | "ios" | "darwin") => MachO,
                Some("windows") => Coff,
                _ => Elf
            }
        }
    };
    Object::new(format, arch, endian)
}
/// Format the library name for the given platform
/// - WebAssembly formats to name.wasm
/// - Apple formats to libname.dylib
/// - Windows formats to name.dll
/// - Anything else formats to libname.so
pub fn format_lib(base: &str, triple: &inkwell::targets::TargetTriple) -> String {
    let triple = triple.as_str().to_str().unwrap();
    let mut components = triple.split('-');
    if matches!(components.next(), Some("wasm" | "wasm32")) {format!("{base}.wasm")} else {
        match components.next() {
            Some("apple") => format!("lib{base}.dylib"),
            Some("linux") => format!("lib{base}.so"),
            _ => match components.next() {
                Some("apple" | "ios" | "darwin") => format!("lib{base}.dylib"),
                Some("windows") => format!("{base}.dylib"),
                _ => format!("lib{base}.so")
            }
        }
    }
}
/// Populate the `.colib` header with the data from the context
pub fn populate_header(obj: &mut Object, ctx: &CompCtx) {
    let mut buf = Vec::<u8>::new();
    ctx.save(&mut buf).unwrap();
    let colib = obj.add_section(
        obj.segment_name(object::write::StandardSegment::Text).to_vec(),
        b".colib".to_vec(), 
        SectionKind::Text
    );
    let colib = obj.section_mut(colib);
    colib.set_data(buf, 1);
}
/// Load the data in the `.colib` header from the file at the specified path
pub fn load_lib(path: &Path, ctx: &CompCtx) -> anyhow::Result<Vec<String>> {
    use object::read::{Object, ObjectSection};
    use anyhow_std::PathAnyhow;
    let buf = path.read_anyhow()?;
    let mut conflicts = vec![];
    if buf.len() >= 4 && &buf[..4] == b"META" {
        buf[4..].split(|&c| c == 0).try_for_each(|p| {
            conflicts.append(&mut load_lib(&Path::assert_from_raw_bytes(p), ctx)?);
            anyhow::Ok(())
        })?;
        Ok(conflicts)
    }
    else {
        let obj = object::File::parse(buf.as_slice())?;
        if let Some(colib) = obj.section_by_name(".colib").and_then(|v| v.uncompressed_data().ok()) {
            conflicts.append(&mut ctx.load(&mut &*colib)?);
        }
        Ok(conflicts)
    }
}
