use std::path::PathBuf;
use std::ffi::OsStr;
use std::io::{self, BufReader, BufRead};
use std::fs::File;
pub fn find_libs<'a>(mut libs: Vec<String>, dirs: &Vec<&str>, ctx: Option<&cobalt::CompCtx>) -> io::Result<(Vec<(PathBuf, String)>, Vec<String>)> {
    let mut out = vec![];
    let mut nf = vec![];
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        if path.extension().and_then(OsStr::to_str) != Some("colib") {continue}
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| x.len() > 0) {
                if lib == &stem {
                    let mut passed_libs = false;
                    let mut passed_syms = false;
                    let mut archive = ar::Archive::new(File::open(&path)?);
                    while let Some(entry) = archive.next_entry() {
                        let entry = entry?;
                        let id = entry.header().identifier();
                        if id == b".co-syms" {
                            if passed_syms {continue}
                            let mut buf = BufReader::new(entry);
                            if let Some(ctx) = ctx {ctx.with_vars(|v| v.load(&mut buf, &ctx))?;}
                            passed_syms = true;
                            if passed_libs {break}
                        }
                        else if id == b".libs" {
                            if passed_libs {continue}
                            let mut libs = vec![];
                            let mut dirs = vec![];
                            let mut reader = BufReader::new(entry);
                            loop {
                                let mut data = vec![];
                                reader.read_until(0, &mut data)?;
                                if data.last() == Some(&0) {data.pop();}
                                if data.len() == 0 {break}
                                if let Ok(s) = std::str::from_utf8(&data) {libs.push(s.to_string());}
                            }
                            loop {
                                let mut data = vec![];
                                reader.read_until(0, &mut data)?;
                                if data.last() == Some(&0) {data.pop();}
                                if data.len() == 0 {break}
                                if let Ok(s) = std::str::from_utf8(&data) {dirs.push(s.to_string());}
                            }
                            let mut res = find_libs(libs, &dirs.iter().map(|x| x.as_str()).collect(), ctx)?;
                            out.append(&mut res.0);
                            nf.append(&mut res.1);
                            passed_libs = true;
                            if passed_syms {break}
                        }
                    }
                    let mut val = String::new();
                    std::mem::swap(&mut val, lib);
                    out.push((path.clone(), val));
                }
            }
        }
    }
    for x in dirs.iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file()) {
        let path = x.into_path();
        match path.extension().and_then(OsStr::to_str) {
            Some("so") | Some("dylib") | Some("dll") => {},
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
    Ok((out, nf.into_iter().chain(libs.into_iter().filter(|x| x.len() > 0).map(|x| x.to_string())).collect()))
}
