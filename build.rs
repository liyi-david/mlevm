use std::env;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let root_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    Command::new("make")
        .args(&[
              "-C",
              &format!("{}/verified_vm", root_dir),
              &format!("TARGET_DIR={}", out_dir)
        ])
        .status()
        .expect("cannot build verified vm as a library");

    println!("cargo:rustc-link-search={}", out_dir);
}
