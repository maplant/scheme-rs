# Rust

The rust library `(import (rust))` provides procedures and syntax for Rust
interoperability.

## `define-rust-type` _syntax_

```scheme
(define-rust-type rtd name constructor predicate)
```

The `define-rust-type` syntax creates the appropriate syntax needed to describe
a `SchemeCompatible` Rust type completely as Scheme type.
