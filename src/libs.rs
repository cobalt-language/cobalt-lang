use std::path::PathBuf;
use std::ffi::OsStr;
use std::io::{self, Read};
use object::{SectionKind, write::Object};
use super::error;
pub fn find_libs(mut libs: Vec<String>, dirs: &[&str], ctx: Option<&cobalt::CompCtx>) -> io::Result<(Vec<(PathBuf, String)>, Vec<String>, bool)> {
    let mut out = vec![];
    let mut failed = false;
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        if let Some(ext) = path.file_name().and_then(OsStr::to_str).map(|x| x.find('.').map(|i| &x[i..]).unwrap_or(x)) {if !(ext.contains(".so") || ext.contains(".dylib") || ext.contains(".dll")) {continue}} else {continue}
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| !x.is_empty()) {
                if lib == stem || (stem.starts_with("lib") && lib == &stem[3..]) {
                    let val = std::mem::take(lib);
                    if let Some(ctx) = ctx {
                        use object::read::{Object, ObjectSection};
                        let mut buf = Vec::new();
                        let mut file = std::fs::File::open(lib)?;
                        file.read_to_end(&mut buf)?;
                        match object::File::parse(buf.as_slice()) {
                            Ok(obj) => if let Some(colib) = obj.section_by_name("colib").and_then(|v| v.uncompressed_data().ok()) {
                                for c in ctx.load(&mut &*colib)? {
                                    error!("conflicting definitions for {c}");
                                    failed = true;
                                }
                            },
                            Err(err) => error!("{err}")
                        }
                    }
                    out.push((path.clone(), val));
                }
            }
        }
    }
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        match path.extension().and_then(OsStr::to_str) {
            Some("a") | Some("lib") => {},
            _ => continue
        }
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| !x.is_empty()) {
                if lib == stem || (stem.starts_with("lib") && lib == &stem[3..]) {
                    let mut val = String::new();
                    std::mem::swap(&mut val, lib);
                    out.push((path.clone(), val));
                }
            }
        }
    }
    Ok((out, libs.into_iter().filter(|x| !x.is_empty()).collect(), failed))
}
pub fn new_object<'a>(triple: &inkwell::targets::TargetTriple) -> Object<'a> {
    let triple = triple.as_str().to_str().unwrap();
    let components = triple.split("-").collect::<Vec<&str>>();
    use object::Architecture::*;
    use object::Endianness::*;
    use object::BinaryFormat::*;
    let mut wasm = false;
    let arch = match components.get(0).copied() {
        Some("aarch64") => Aarch64,
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
                Some("windows") => Pe,
                _ => Elf
            }
        }
    };
    Object::new(format, arch, endian)
}
pub fn populate_header(obj: &mut Object, ctx: &cobalt::CompCtx) {
    let mut buf = Vec::<u8>::new();
    ctx.save(&mut buf).unwrap();
    let colib = obj.add_section(vec![], b"colib".to_vec(), SectionKind::Other);
    let colib = obj.section_mut(colib);
    colib.set_data(buf, 1);
}
