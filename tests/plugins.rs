#[cfg(feature = "plugins")]
#[allow(unused_macros, unused_imports)]
mod common;

#[cfg(feature = "plugins")]
fn test_plugin_dylib_name() -> &'static str {
    if cfg!(target_os = "macos") {
        "libtest_plugin.dylib"
    } else if cfg!(target_os = "windows") {
        "test_plugin.dll"
    } else {
        "libtest_plugin.so"
    }
}

/// Builds the test plugin and returns the path to its dylib.
///
/// The plugin must link the exact same scheme-rs image as this test binary,
/// so its dlopen resolves the runtime to the image already loaded in this
/// process; any other scheme-rs build is a second runtime image, which
/// load_plugin rejects. Building via `cargo test` with both packages
/// selected reproduces this suite's build exactly: same workspace, same
/// profile, and same feature unification (dev-dependencies included).
///
/// Note: this assumes the suite itself runs with --all-features.
#[cfg(feature = "plugins")]
fn build_test_plugin() -> std::path::PathBuf {
    use std::path::Path;
    use std::process::Command;

    let status = Command::new("cargo")
        .args([
            "test",
            "-p",
            "scheme-rs",
            "-p",
            "test-plugin",
            "--all-features",
            "--no-run",
            "--quiet",
        ])
        .current_dir(Path::new(env!("CARGO_MANIFEST_DIR")))
        .status()
        .expect("failed to build test plugin");
    assert!(status.success(), "test plugin build failed");

    // <target>/debug/deps/plugins-<hash> -> <target>/debug/deps
    let deps_dir = std::env::current_exe()
        .expect("failed to locate test executable")
        .parent()
        .expect("unexpected test executable location")
        .to_path_buf();

    let dylib = deps_dir.join(test_plugin_dylib_name());
    assert!(dylib.exists(), "test plugin dylib not found at {dylib:?}");
    dylib
}

#[cfg(feature = "plugins")]
#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn load_plugin_and_call_bridges() {
    use scheme_rs::runtime::Runtime;
    use std::path::Path;

    let dylib = build_test_plugin();

    let rt = Runtime::new();
    let lib = unsafe { libloading::Library::new(&dylib) }.expect("failed to dlopen test plugin");
    unsafe { rt.load_plugin(lib) }.expect("failed to load plugin bridges");

    scheme_rs_macros::maybe_await!(rt.run_program(Path::new("tests/plugins.scm")))
        .expect("scheme test failed");
}

#[cfg(feature = "plugins")]
#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn load_same_plugin_twice_is_ok() {
    use scheme_rs::runtime::Runtime;
    use std::path::Path;

    let dylib = build_test_plugin();

    let rt = Runtime::new();
    let lib1 = unsafe { libloading::Library::new(&dylib) }.unwrap();
    unsafe { rt.load_plugin(lib1) }.expect("first load failed");

    let lib2 = unsafe { libloading::Library::new(&dylib) }.unwrap();
    unsafe { rt.load_plugin(lib2) }.expect("second load should succeed");

    scheme_rs_macros::maybe_await!(rt.run_program(Path::new("tests/plugins.scm")))
        .expect("bridges should work after double load");
}

#[cfg(feature = "plugins")]
#[test]
fn version_constant_matches_crate() {
    assert_eq!(
        scheme_rs::registry::SCHEME_RS_VERSION,
        env!("CARGO_PKG_VERSION"),
    );
}
