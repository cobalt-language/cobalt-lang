use std::path::PathBuf;
use std::ffi::OsStr;
use std::io;
use std::process::{Command, Output};
pub fn find_libs<'a>(mut libs: Vec<String>, dirs: &Vec<&str>, ctx: Option<&cobalt::CompCtx>) -> io::Result<(Vec<(PathBuf, String)>, Vec<String>)> {
    let mut out = vec![];
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        if let Some(ext) = path.file_name().and_then(OsStr::to_str).map(|x| x.find('.').map(|i| &x[i..]).unwrap_or(x)) {if !(ext.contains(".so") || ext.contains(".dylib") || ext.contains(".dll")) {continue}} else {continue}
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| x.len() > 0) {
                if lib == &stem || (stem.starts_with("lib") && lib == &&stem[3..]) {
                    let mut val = String::new();
                    std::mem::swap(&mut val, lib);
                    if let Some(ctx) = ctx {
                        match Command::new("objcopy").arg(&path).args(["--dump-section", ".colib=/dev/stdout"]).output() { // TODO: use ELF parser library
                            Ok(Output {status, stdout, ..}) if status.success() => ctx.with_vars(|v| v.load(&mut stdout.as_slice(), ctx))?,
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
            for lib in libs.iter_mut().filter(|x| x.len() > 0) {
                if lib == &stem || (stem.starts_with("lib") && lib == &&stem[3..]) {
                    let mut val = String::new();
                    std::mem::swap(&mut val, lib);
                    out.push((path.clone(), val));
                }
            }
        }
    }
    Ok((out, libs.into_iter().filter(|x| x.len() > 0).map(|x| x.to_string()).collect()))
}
