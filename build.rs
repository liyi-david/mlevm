use std::env;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    Command::new("ocamlfind")
        .args(&[
              "ocamlopt","-output-complete-obj",
              "-o", &format!("{}/libevm.o", out_dir),
              "-I", "src/ocaml",
              "-linkpkg", "-package", "num,ctypes",
              "src/ocaml/interpreter.ml"
        ])
        .status()
        .expect("Couldn't run builder clean. Do you have dune?");

    Command::new("ar")
        .args(&[
            "qs",
            &format!("{}/libevm.a", out_dir),
            &format!("{}/libevm.o", out_dir),
        ])
        .status()
        .expect("ar gave an error");

    println!("cargo:rustc-link-search={}", out_dir);
}
