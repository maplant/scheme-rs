# Scheme-rs

Scheme-rs is a work-in-progress implementation of the [R6RS](https://www.r6rs.org/final/r6rs.pdf) specification
of the scheme programming language that is designed to work with async Rust runtimes like [tokio](https://tokio.rs/).
In essence, it is a embedded scripting language for the async Rust ecosystem.

Scheme-rs is intended to be fully compliant with R6RS, and R7RS large when it is eventually released. To that end
the bones are mostly there but some key issues. See [unimplemented features](#unimplemented-features) for more 
information. One key issue is that no garbage collection ever occurs, so every program leaks memory.

Eventually, I would like scheme-rs to be more opinionated in the extras it provides, and include a package manager.
That is obviously a long way away.

## This is Scheme but it supports async via Tokio?

Exactly. Here is an example:

```scheme
(define (sleep-and-print secs message)
  (await (sleep (* secs 1000)))
  (display message))

(define (sleep-in-parallel)
  (join (spawn (lambda ()
                 (sleep-and-print 3 "first message")))
        (spawn (lambda ()
                 (sleep-and-print 5 "second message")))))
```

Now you can run the code and it will work as expected:

```
>>> (await (sleep-in-parallel))
"first message"
"second message"
$3 = (() ())
```

Not a whole lot of IO operations are supported right now, so it's very limited.

## Usage:

### Running a REPL:

A REPL is the default entry point for scheme-rs at the current moment. You can access it by running `cargo run`
in the repo's root directory:

```
~/scheme-rs> cargo run
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.03s
     Running `target/debug/scheme-rs`
>>> (+ 5 5)
$1 = 10
```

### Creating Builtin Functions:

Scheme-rs provides a `builtin` function attribute macro to allow you to easily define builtins. For example,
here is the definition of the `number?` builtin in the source code:

```rust
#[builtin("number?")]
pub async fn is_number(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(&*arg, Value::Number(_)))))
}
```

## Why not just use Guile?

In the end, it all comes down to [call with current continuation](https://en.wikipedia.org/wiki/Call-with-current-continuation). Guile implements this feature by copying the stack, an action which is obviously problematic when considering 
open locks. Scheme-rs remedies this by providing all functions with the current continuation so that they may handle 
it properly. Additionally, the current continuation only contains references and not locked variables. 

## Unimplemented features

- Currently, the compiler does not include macro expansion as part of the continuation. Therefore, calling `call/cc`
  in syntax transformers will not produce correct results. I had an _amazing_ example of this but I lost it.
  
- A large number of library functions are missing. 

- There is no real easy way to embed scheme-rs. 

- GC never collects, no interning. 

- Most of tokio's file system and networking functions are not implemented.

## Okay, what _is_ implemented?

Quite a bit. Tail calls work totally fine, you'll never blow out the stack via tail recursion.

Macros are implemented to a surprisingly high degree, including allowing you to reference variables in different
scopes and have them work properly. Special forms (i.e. `set!` macros) are also implemented. 

`call/cc` is implemented, except in the scenario outlined in the previous section. 

Most of the base of Scheme is actually here, minus quite a bit of the functions you'd expect, such as let*, letrec,
etc.
