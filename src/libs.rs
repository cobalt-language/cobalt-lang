use std::path::PathBuf;
use std::ffi::OsStr;
use std::io;
use std::process::{Command, Output};
pub fn find_libs(mut libs: Vec<String>, dirs: &[&str], ctx: Option<&cobalt::CompCtx>) -> io::Result<(Vec<(PathBuf, String)>, Vec<String>, bool)> {
    let mut out = vec![];
    let mut failed = false;
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        if let Some(ext) = path.file_name().and_then(OsStr::to_str).map(|x| x.find('.').map(|i| &x[i..]).unwrap_or(x)) {if !(ext.contains(".so") || ext.contains(".dylib") || ext.contains(".dll")) {continue}} else {continue}
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| !x.is_empty()) {
                if lib == stem || (stem.starts_with("lib") && lib == &stem[3..]) {
                    let mut val = String::new();
                    std::mem::swap(&mut val, lib);
                    if let Some(ctx) = ctx {
                        match Command::new("objcopy").arg(&path).args(["--dump-section", ".colib=/dev/stdout"]).output() { // TODO: use ELF parser library
                            Ok(Output {status, stdout, ..}) if status.success() => for conflict in ctx.load(&mut stdout.as_slice())? {
                                eprintln!("redefinition of {conflict} in {}", path.display());
                                failed = true;
                            },
                            _ => {}
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
