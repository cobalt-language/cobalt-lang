use std::process::{Command, Output};
fn git_info() -> Option<()> {
    let Output { stdout, status, .. } = Command::new("git")
        .args(["rev-parse", "HEAD", "--abbrev-ref", "HEAD"])
        .output()
        .ok()?;
    if status.success() {
        let info = std::str::from_utf8(&stdout).ok()?;
        let (commit, branch) = info.split_once('\n')?;
        println!("cargo:rustc-env=GIT_BRANCH={branch}");
        println!("cargo:rustc-env=GIT_COMMIT={commit}");
        println!("cargo:rustc-cfg=has_git");
    }
    Some(())
}

fn main() {
    println!("cargo:rerun-if-changed=../.git");
    git_info();
}
