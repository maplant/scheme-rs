#![cfg(all(feature = "async", feature = "tokio"))]

#[allow(unused)]
mod common;

// multi_thread is required: the main test thread blocks in join() while
// another worker must drive the timer that completes the spawned thunk.
#[tokio::test(flavor = "multi_thread")]
async fn blockon_pending() {
    use scheme_rs::runtime::Runtime;
    use std::path::Path;

    let rt = Runtime::handle();
    rt.run_program(Path::new("tests/blockon_pending.scm"))
        .await
        .expect("Test blockon_pending failed");
}
