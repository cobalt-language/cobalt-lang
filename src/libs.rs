use std::path::PathBuf;
pub fn find_libs<'a>(mut libs: Vec<&'a str>, dirs: Vec<&str>) -> (Vec<(PathBuf, &'a str)>, Vec<&'a str>) {
    let mut outs = vec![];
    let it = dirs.into_iter().flat_map(|dir| walkdir::WalkDir::new(dir).follow_links(true).into_iter()).filter_map(|x| x.ok()).filter(|x| x.file_type().is_file());
    it.for_each(|x| {
        let path = x.into_path();
        match path.extension().and_then(std::ffi::OsStr::to_str) {
            Some("so") | Some("dylib") | Some("colib") | Some("dll") => {},
            _ => return
        }
        if let Some(stem) = path.file_stem().and_then(|x| x.to_str()) {
            for lib in libs.iter_mut().filter(|x| x.len() > 0) {
                if lib == &stem || (stem.starts_with("lib") && lib == &&stem[3..]) {
                    outs.push((path.clone(), *lib));
                    *lib = "";
                }
            }
        }
    });
    (outs, libs.into_iter().filter(|x| x.len() > 0).collect())
}
