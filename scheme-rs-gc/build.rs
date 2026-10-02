use cc::Build;

fn main() {
    println!("cargo::rerun-if-changed=src/ctx_capture.c");
    Build::new()
        .file("src/ctx_capture.c")
        .compile("gc_capture_ctx");
}
