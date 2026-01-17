# <picture><source media="(prefers-color-scheme: dark)" srcset="logo-dark.png"><img align="left" width="150px" src="logo-light.png"></picture> Scheme-rs: Embedded Scheme for the Rust Ecosystem

Scheme-rs is an implementation of the
[R6RS](https://www.r6rs.org/final/r6rs.pdf) specification of the [Scheme programming 
language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) that is 
designed to embedded within sync and async Rust. 

## Features:

- **Fast**: scheme-rs uses JIT compilation to provide performance on par with 
  other modern scheme implementations.
- **Modern**: scheme-rs is a modern scheme implementation of R6RS and includes 
  advanced features such as [delimited continuations](https://en.wikipedia.org/wiki/Delimited_continuation)
- **Easy to use**: scheme-rs makes it trivial to define Rust functions and data
  structures that are accessible from scheme code and vice-versa.
- **Safe**: scheme-rs provides a completely safe API that is impossible to 
  misuse.
- **Supports both async and sync Rust**: by enabling the `async` feature flag 
  it becomes possible (and easy) to define async scheme functions in Rust. 

## Getting started:

For embedding scheme-rs in your Rust project, [take a look at the API documentation](https://docs.rs/scheme-rs/latest/scheme_rs/).

For installing scheme-rs as a standalone executable, cargo can be used:

```console
$ cargo install scheme-rs 
```

## Documentation 

- [API documentation](https://docs.rs/scheme-rs/latest/scheme_rs/)
- [Language reference](https://www.scheme-rs.org)

## Contributing

If you have any questions or comments about the project, feel free to join 
[the scheme-rs discord server](https://discord.gg/sR4TttzGv5).
