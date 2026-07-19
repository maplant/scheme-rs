scheme-rs is an implementation of the
[R6RS](https://www.r6rs.org/final/r6rs.pdf) specification of the [Scheme programming
language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) that is
designed to embedded within sync and async Rust.

# Feature flags
- `async`: Enables support for async functions. Requires the `tokio` feature
  flag.
- `tokio`: Enables support for the [tokio](https://tokio.rs/) async
  executor.
- `load-libraries-from-fs`: Enables automatically loading libraries from the 
  file system. The library name specifies its location on the filesystem 
  relative to the currently running process. Enabled by default.

# Getting started

To get started using scheme-rs in your project, create a
[`Runtime`](runtime::Runtime):

```rust
# use scheme_rs::runtime::Runtime;
let runtime = Runtime::handle();
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
let env = TopLevelEnvironment::new_repl();
env.import("(library (rnrs))".parse().unwrap());
```

Now that you have an environment, you can call `eval` on it. The first
argument to eval determines whether or not the code is allowed to import
external packages. If you are running untrusted user code, be sure to pass
false and think careful of the functions you provide.

```rust
# use scheme_rs::{env::TopLevelEnvironment, proc::Procedure};
# let env = TopLevelEnvironment::new_repl();
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
let factorial = vals[0].cast::<Procedure>().unwrap();
```

## Procedures

Evaluating the previous code example returns a factorial 
[`Procedure`](proc::Procedure) which can be called from Rust. To do so, use
the [`Procedure::call`](proc::Procedure::call) method. Procedures are 
automatically garbage collected and implement `Send` and `Sync` and are 
`'static` so you can hold on to them for as long as you want and put them
anywhere.

```rust
# use scheme_rs::{
# env::TopLevelEnvironment, value::Value, proc::Procedure,
# };
# let env = TopLevelEnvironment::new_repl();
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
# let factorial = factorial.cast::<Procedure>().unwrap();
let [result] = factorial.call(&[Value::from(5)]).unwrap().try_into().unwrap();
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

Bridge functions can also optionally automatically type-check their arguments at
run-time, so the following definition is also valid:

```rust
# use scheme_rs::{
# registry::bridge, value::Value, exceptions::Exception};
#[bridge(name = "add-five", lib = "(add-five-lib)")]
fn add_five(num: usize) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(num + 5)])
}
```

Once you've defined a bridge function it can be imported and called from scheme:

```rust
# use scheme_rs::{
# registry::bridge, value::Value, exceptions::Exception, 
# env::TopLevelEnvironment};
# #[bridge(name = "add-five", lib = "(add-five-lib)")]
# fn add_five(num: &Value) -> Result<Vec<Value>, Exception> {
#    let num: usize = num.clone().try_into()?;
#    Ok(vec![Value::from(num + 5)])
# }
# fn main() {
# let env = TopLevelEnvironment::new_repl();
# env.import("(library (rnrs))".parse().unwrap());
let val = env.eval(
  true,
  "
  (import (add-five-lib))
  (add-five 12)
  "
)
.unwrap();
assert_eq!(val[0].cast::<u64>().unwrap(), 17);
# }
```

It is also possible to implement bridge functions in a [continuation-passing 
style](https://en.wikipedia.org/wiki/Continuation-passing_style) for greater 
flexibility and control. See the [`cps_bridge`](registry::cps_bridge) proc macro
for more information.

## Values

Scheme [`Values`](value::Value) can be created from most primitives and std
library objects simply by using `From`:

```rust
# use scheme_rs::value::Value;
let pi = Value::from(3.14159268);
let pair = Value::from((Value::from(1), Value::from((Value::from(2), Value::from(())))));
```

Rust objects that implement [`Embeddable`](records::Embeddable) can 
be converted using `Value::from`:

```rust
# use scheme_rs::{value::Value, records::{rtd, Embeddable, RecordTypeDescriptor}, gc::Trace, exceptions::Exception};
# use std::sync::Arc;
#[derive(Debug, Trace)]
struct Vec3 {
    x: f64,
    y: f64
}

unsafe impl Embeddable for Vec3 {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: Vec3,
            name: "vec3",
            fields: ["x", "y"],
            constructor: |x, y| {
                Ok(Vec3 {
                    x: x.try_to()?,
                    y: y.try_to()?,
                })
            }
        )
    }
    
    fn get_field(&self, k: usize) -> Result<Value, Exception> {
        match k {
            0 => Ok(self.x.into()),
            1 => Ok(self.y.into()),
            _ => unreachable!(),
        }
    }
}

let pos = Value::from(Vec3 { x: 1.0, y: 2.0 });
```

`Values` can be converted back to primitive Rust types with the 
- [`cast_to`](value::Value::cast_to)
- and [`try_to`](value::Value::try_to) functions.

The `cast_to` function converts values to `Option<_>`, and `try_to` 
provides more detailed error conditions of the conversion failure. To recover an
embedded Rust type, cast to an [`Embedded<T>`](records::Embedded), e.g.
`value.try_to::<Embedded<Vec3>>()`.

```rust
# use scheme_rs::value::Value;
# let pi = Value::from(3.14159268);
assert_eq!(pi.cast::<f64>(), Some(3.14159268));
```

See [the `value` module for more information](value).


# Error handling

All scheme-rs functions that return an error return an [`Exception`](exceptions)
which adhere to the scheme condition system. See
[the `exceptions` module for more information](exceptions).

# Mutable references

scheme-rs supports accessing mutable variables via the [`ContBarrier`](proc) 
parameter. Mutable variables are called "params" in scheme-rs due to their 
similarity to [dynamic parameters](https://srfi.schemers.org/srfi-39/srfi-39.html).

Creating a new mutable references enforces a new continuation barrier.

```rust
# use scheme_rs::{
# registry::cps_bridge, value::Value, exceptions::Exception, 
# env::TopLevelEnvironment, proc::{Application, ContBarrier, Procedure}}; 
#[cps_bridge(def = "inc", lib = "(example)")]
pub fn inc(
    _env: &[Value],
    _args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let var = {
        let var: &mut u32 = barrier.get_param("var").unwrap().downcast_mut().unwrap();
        *var += 1;
        *var
    };
    Ok(barrier.call_cont(vec![Value::from(var)]))
}

#[cps_bridge(def = "call-with-var thunk", lib = "(example)")]
pub fn call_with_var(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // Set up a barrier with the new param
    let mut var = 0u32;
    let mut new_barrier = ContBarrier::new();
    new_barrier.add_param("var", &mut var);

    // Call the thunk arg with the new barrier:
    let thunk: Procedure = args[0].clone().try_into()?;
    let result = thunk.call_with_barrier(&[], &mut new_barrier)?;

    // Return to the continuation:
    Ok(barrier.call_cont(result))
}
```
