# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2026-05-06

### Added

- Added import restrictions system via allowlists (thanks @wizzeh!).
- Added automatically importing  `(rnrs (6))` if no imports are present in a 
  scheme program.
- Added Basic threading library `(threads (1))`.
- Added values parameter to `abort-to-prompt` in order to allow calling prompt
  handlers with values.
- Added method for accessing Rust mutable references in Scheme within a 
  static delimited continuation via `ContBarrier`.
- Added `r6rs` enumeration sets.
- Improved exception printing (thanks @xnacly!).
- Added Shinn-Wright pattern matcher accessible via `(lang (1))`.
- Allow for `_` to be a keyword in `syntax-rules` and `syntax-case` macros.

### Changed

- Renamed `DynamicState` to `ContBarrier` to better reflect its new role.
- Attempting to call an escape procedure outside the current continuation 
  barrier now throws a runtime error.

### Fixed

- Fixed `(define ())` panicking.
- Fixed erroneous header formatting in docs.
- Aligned error messages with Rust conventions (thanks @wizzeh!).
- Fixed non-determinism in Gc tests.
- Fixed re-defining of top level items causing a panic.
- Fixed `Gc` not requiring `Send`.
- Various performance improvements to the runtime system and garbage collector.
- Various fixes to reach higher parity in the R6RS test suite (26 of 2445).
