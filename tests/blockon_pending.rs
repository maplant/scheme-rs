#![cfg(all(feature = "async", feature = "tokio"))]

#[allow(unused)]
mod common;

// multi_thread is required: the park-based block_on parks this worker while
// another drives the timer. On current_thread this would deadlock, which is
// the ceiling documented in threads.rs.
#[tokio::test(flavor = "multi_thread")]
async fn blockon_pending() {
    use scheme_rs::runtime::Runtime;
    use std::path::Path;

    let rt = Runtime::new();
    rt.run_program(Path::new("tests/blockon_pending.scm"))
        .await
        .expect("Test blockon_pending failed");
}
