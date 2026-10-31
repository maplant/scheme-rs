use std::path::Path;

use scheme_rs::{gc::set_heap_size, runtime::Runtime};

/// Garbage far beyond the heap size must make allocation wait for the
/// collector, not fail.
#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn a_full_heap_waits_for_the_collector() {
    set_heap_size(32 << 20).unwrap();
    let rt = Runtime::handle();
    scheme_rs_macros::maybe_await!(rt.run_program(Path::new("tests/heap_pressure.scm")))
        .expect("allocating garbage beyond the heap size failed");
}
