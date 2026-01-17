scheme-rs is an implementation of the
[R6RS](https://www.r6rs.org/final/r6rs.pdf) specification of the [Scheme programming
language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) that is
designed to embedded within sync and async Rust.

# Feature flags:
- `async`: Enables support for async functions. Requires the `tokio` feature
  flag.
- `tokio`: Enables support for the [tokio](https://tokio.rs/) async
  executor.

# Getting started

To get started using scheme-rs in your project, create a
[`Runtime`](runtime::Runtime):

```rust
# use scheme_rs::runtime::Runtime;
let runtime = Runtime::new();
```

The `Runtime` struct initializes the garbage collector and handles the
memory of JIT compiled functions. The `Runtime` struct is automatically
garbage collected so you only need it for as long as you're creating new
scheme procedures.

 # Running Scheme code from Rust

The simplest way to run scheme code from Rust is to use the
[`TopLevelEnvironment::eval`](env::TopLevelEnvironment::eval) function which evaluates a
string and returns the evaluated scheme values. Before you can call `eval`,
you need to create a [`TopLevelEnvironment`](env::TopLevelEnvironment) which defines the
set of imports provided to the scheme code.

```rust
# use scheme_rs::{runtime::Runtime, env::TopLevelEnvironment};
# let runtime = Runtime::new();
let env = TopLevelEnvironment::new_repl(&runtime);
env.import("(library (rnrs))".parse().unwrap());
```

Now that you have an environment, you can call `eval` on it. The first
argument to eval determines whether or not the code is allowed to import
external packages. If you are running untrusted user code, be sure to pass
false and think careful of the functions you provide.

```rust
# use scheme_rs::{runtime::Runtime, env::TopLevelEnvironment, proc::Procedure};
# let runtime = Runtime::new();
# let env = TopLevelEnvironment::new_repl(&runtime);
# env.import("(library (rnrs))".parse().unwrap());
let vals = env.eval(
    false,
    "
    (define (fact n)
      (if (= n 1)
          1
          (* n (fact (- n 1)))))
    fact
    "
)
.unwrap();
let factorial = vals[0].cast_to_scheme_type::<Procedure>().unwrap();
```

## Procedures

Evaluating the previous code example returns a factorial 
[`Procedure`](proc::Procedure) which can be called from Rust. To do so, use
the [`Procedure::call`](proc::Procedure::call) method. Procedures are 
automatically garbage collected and implement `Send` and `Sync` and are 
`'static` so you can hold on to them for as long as you want and put them
anywhere.

```rust
# use scheme_rs::{runtime::Runtime, env::TopLevelEnvironment, value::Value, proc::Procedure};
# let runtime = Runtime::new();
# let env = TopLevelEnvironment::new_repl(&runtime);
# env.import("(library (rnrs))".parse().unwrap());
# let [factorial] = env.eval(
#     false,
#     "
#     (define (fact n)
#       (if (= n 1)
#           1
#           (* n (fact (- n 1)))))
#     fact
#     "
# )
# .unwrap()
# .try_into()
# .unwrap();
# let factorial = factorial.cast_to_scheme_type::<Procedure>().unwrap();
let [result] = factorial
    .call(&[Value::from(5)])
    .unwrap()
    .try_into()
    .unwrap();
let result: u64 = result.try_into().unwrap();
assert_eq!(result, 120);
```

# Running Rust code from Scheme

The simplest way to create Rust functions that are callable from Scheme
is with the [`bridge`](registry::bridge) procedural macro. The `bridge` proc
allows one to write Scheme functions in a direct style in Rust that are
automatically registered into a given library:

```rust
# use scheme_rs::{
# registry::bridge, value::Value, exceptions::Exception};
#[bridge(name = "add-five", lib = "(add-five-lib)")]
fn add_five(num: &Value) -> Result<Vec<Value>, Exception> {
    let num: usize = num.clone().try_into()?;
    Ok(vec![Value::from(num + 5)])
}
```

Once you've defined a bridge function it can be imported and called from scheme:

```rust
# use scheme_rs::{
# registry::bridge, value::Value, exceptions::Exception, 
# runtime::Runtime, env::TopLevelEnvironment};
# #[bridge(name = "add-five", lib = "(add-five-lib)")]
# fn add_five(num: &Value) -> Result<Vec<Value>, Exception> {
#    let num: usize = num.clone().try_into()?;
#    Ok(vec![Value::from(num + 5)])
# }
# fn main() {
# let runtime = Runtime::new();
# let env = TopLevelEnvironment::new_repl(&runtime);
# env.import("(library (rnrs))".parse().unwrap());
let val = env.eval(
  true,
  "
  (import (add-five-lib))
  (add-five 12)
  "
)
.unwrap();
assert_eq!(val[0].cast_to_scheme_type::<u64>().unwrap(), 17);
# }
```

It is also possible to implement bridge functions in a [continuation-passing 
style](https://en.wikipedia.org/wiki/Continuation-passing_style) for greater 
flexibility and control. See the [`cps_bridge`](registry::cps_bridge) proc macro
for more information.

## Embedding Rust structs in Scheme

# Error handling
 
`TODO`

# Garbage Collection

See the [gc] module for a more detailed explanation on the garbage collector.



