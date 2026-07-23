//! Intentionally empty. Integration tests depend on the package's lib unit,
//! which forces `cargo test --no-run` to emit the cdylib artifact. The host
//! test suite builds the plugin through `cargo test` (not `cargo build`) so
//! that feature unification — including dev-dependencies — matches the host
//! build exactly and the plugin links the very same scheme-rs image.
