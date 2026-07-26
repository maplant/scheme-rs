#[cfg(feature = "plugins")]
#[allow(unused_macros, unused_imports)]
mod common;

#[cfg(feature = "plugins")]
fn test_plugin_dylib_name() -> &'static str {
    if cfg!(target_os = "macos") {
        "target/debug/libtest_plugin.dylib"
    } else if cfg!(target_os = "windows") {
        "target/debug/test_plugin.dll"
    } else {
        "target/debug/libtest_plugin.so"
    }
}

#[cfg(feature = "plugins")]
#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn load_plugin_and_call_bridges() {
    use scheme_rs::runtime::Runtime;
    use std::path::Path;
    use std::process::Command;

    let plugin_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test-plugin");
    let status = Command::new("cargo")
        .args(["build", "--quiet"])
        .current_dir(&plugin_dir)
        .status()
        .expect("failed to build test plugin");
    assert!(status.success(), "test plugin build failed");

    let dylib = plugin_dir.join(test_plugin_dylib_name());
    assert!(dylib.exists(), "test plugin dylib not found at {dylib:?}");

    let rt = Runtime::new();
    unsafe { rt.load_plugin(&dylib) }.expect("failed to load plugin");

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
    use std::process::Command;

    let plugin_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test-plugin");
    Command::new("cargo")
        .args(["build", "--quiet"])
        .current_dir(&plugin_dir)
        .status()
        .expect("failed to build test plugin");

    let dylib = plugin_dir.join(test_plugin_dylib_name());

    let rt = Runtime::new();
    unsafe { rt.load_plugin(&dylib) }.expect("first load failed");
    unsafe { rt.load_plugin(&dylib) }.expect("second load should succeed");

    scheme_rs_macros::maybe_await!(rt.run_program(Path::new("tests/plugins.scm")))
        .expect("bridges should work after double load");
}
