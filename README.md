# <div align="left" width="150px"> <picture> <source media="(prefers-color-scheme: dark)"  srcset="logo-dark.png">  <source media="(prefers-color-scheme: light)" srcset="logo-light.png"> <img src="logo-light.png">  </picture> </div> Scheme-rs: Embedded Scheme for the Async Rust Ecosystem

Scheme-rs is a work-in-progress implementation of the [R6RS](https://www.r6rs.org/final/r6rs.pdf) specification
of the scheme programming language that is designed to work with async Rust runtimes like [tokio](https://tokio.rs/).
In essence, it is a embedded scripting language for the async Rust ecosystem.

Scheme-rs is intended to be fully compliant with R6RS, and R7RS large when it is eventually released. To that end
the bones are mostly there but some key issues remain. 

Eventually, I would like scheme-rs to be more opinionated in the extras it provides, and include a package manager.
That is obviously a long way away.

## Features currently supported by scheme-rs:

- Tail-call optimizations are fully supported 
- Garbage Collected via [Bacon-Rajan Concurrent Cycle Collection](https://pages.cs.wisc.edu/~cymen/misc/interests/Bacon01Concurrent.pdf)
- Most key forms (let/let*/letrec/lambda/define etc)
- Call by current continuation
- Hygienic macros and syntax transformers (`define-syntax`, `syntax-case`, `datum->syntax` and `syntax->datum`) 
- Spawning tasks and awaiting futures
- Exceptions and error handling (`raise`, `raise-continuable`, `with-exception-handler`)
- `dynamic-wind`
- Defining async bridge functions in Rust

## Features currently unsupported by scheme-rs: 

- Records and conditions 
- Ports and IO operations
- Most API functions are not implemented
- A large portion of lexical structures are missing; there's no way to specify recursive data structures
- And many more that I cannot think of off the top of my head

## Implementation details:

`scheme-rs` is JIT compiled, compiling the expanded Scheme code into a [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style) 
mid-level IR, and then converting that into LLVM IR. 

At present the code produced by `scheme-rs` is of pretty poor quality. Very few optimizations are performed, all variables 
are boxed. Focus was spent on making this project as correct as possible, and to that end this is a JIT compiler for 
scheme that fully supports syntax-case, proper tail recursion, and interaction with async Rust. Contributions are more than
welcome if you would like to add optimizations passes to the compiler.

## Usage:

### Running a REPL:

A REPL is the default entry point for scheme-rs at the current moment. You can access it by running `cargo run`
in the repo's root directory (examples taken from wikipedia):

```
~/scheme-rs> cargo run
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.03s
     Running `target/debug/scheme-rs`
> (let loop ((n 1))
   (if (> n 10)
       '()
       (cons n
         (loop (+ n 1)))))
$1 = (1 2 3 4 5 6 7 8 9 10)
> (let* ((yin
            ((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))
         (yang
            ((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))
     (yin yang))
@*@**@***@****@*****@******@*******@********@*********@**********@***********@**********...^C
```

### Creating Builtin Functions:

Scheme-rs provides a `bridge` function attribute macro to allow you to easily define builtins. For example,
here is the definition of the `number?` builtin in the source code. Notice that this function is async:

```rust
#[bridge(name = "number?", lib = "(base)")]
pub async fn is_number(arg: &Gc<Value>) -> Result<Gc<Value>, Exception> {
    let arg = arg.read();
    Ok(Gc::new(Value::Boolean(matches!(&*arg, Value::Number(_)))))
}
```

## Contributing

If you are an intrepid scheme compiler optimizer, this project is for you! Lots of work needs to be done
to bring this project up to snuff. The initial focus was on correctness, so if you would like to take a
stab at improving perf or add features anywhere in this project, feel free!

If you have any questions or comments about the project, feel free to join [the scheme-rs discord server here](https://discord.gg/sR4TttzGv5).
