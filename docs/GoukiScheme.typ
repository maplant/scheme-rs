// #import "@preview/charged-ieee:0.1.3": ieee
#import "@preview/bamdone-ieeeconf:0.1.1": ieee
#import "@preview/zebraw:0.5.5": *
#show: zebraw.with(numbering: true)

// #zebraw(numbering: false)

// #import "@preview/codly:1.3.0": *
// #import "@preview/codly-languages:0.1.1": *
// #show: codly-init.with()
// 
// #codly(languages: codly-languages, number-format: none, zebra-fill: none, display-icon: false)

#show: ieee.with(
    title: [Gouki Scheme: An embedded Scheme implementation for Async Rust],
    abstract: [
        Todo
    ],
    /*
    authors: [
        (
            name: "Matthew Plant",
            location: [New York, U.S.A.],
            email: "maplant@protonmail.com"
        )
    ]
    */
)

= Introduction

Over the last decade Rust has become an increasingly popular choice for systems
programming. Since the introduction of async runtimes to the Rust language,
async Rust programs have also become increasingly popular for writing highly
IO bound applications such as web servers. While Rust is extremely performant
for such applications, async Rust applications tend to be difficult to develop
and debug with. Part of the problem is long compilation times; Rust applications
must be stopped, rebuilt, and restarted, extending development time and reducing
the ability for developers to iterate.

Glue code written in a dynamic language has long been a solution for enabling
rapid prototyping and interopability in a language with long build times. Under
this model, the performance critical code that does not often need to be rebuilt
is written in the slower compiled language, while a dynamic perhaps interpreted
language is embedded into the built to allow for gluing components together
without the need for slow rebuilds. Changes to the glue code can be seen just be
re-loading the applications, or perhaps a hot reloading mechanism can bring them
into the application simply by saving to a file. In either case, iteration
speeds are improved dramatically.

Another advantage of embedding a dynamic language in an application is that the
application can provide a Read-Eval-Print-Loop, or REPL. Interactive prompts can
expand the debugging capabilities of a application by allowing for inspection of
the application while it is live. The application can be debugged, inspected, and
orchestrated as a plastic system rather than a rigid daemon that can only be
started, stopped or interacted with in small fixed languages.

Scheme has shown success as a embedded dynamic language, but its use has been
limited to synchronous (but perhaps multithreaded) applications. Scheme
implementations exist for embedding within Rust, but they are limited to running
sync Rust code and cannot be async themselves. Since async is a Rust language
feature that touches all parts of the code base, a new implementation of Scheme
is required that is built with async Rust in mind to take full advantage of the
async runtime and more importantly integrate frictionlessly into the async
application.

GoukiScheme is a Scheme implementation designed to integrate flawlessly with
async Rust; GoukiScheme code can execute asynchronously embedded in an async
Rust application and can in turn execute async Rust functions. Abitrary Rust
objects can be stored in GoukiScheme variables and passed to GoukiScheme
functions with little modification. This is done without sacrificing potential
performance by architecting GoukiScheme as a tree-walking AST; indeed GoukiScheme
is fully JIT compiled and takes advantage of a CPS based mid-level IR to compile
to LLVM SSA. GoukiScheme does this while providing a completely Safe API.

= Memory Management

In order to provide an memory management interface that gets out of the way as
much as possible, the concurrent cycle collection algorithm was chosen. Rust has
strong support for traditional reference counted memory management, so we chose
to extend this support to collecting cycles rather than implement a tracing
garbage collector. Tracing garbage collectors are difficult to implement in Rust
due to no in-built support for determining the root objects. Cycle collection
does not need to know the root objects, and therefore is easier to implement in
the Rust without the runtime cost or API friction that would otherwise be needed
to get around the lack of root information.

The API for garbage collection in GoukiScheme is simple. GoukiScheme provides a
a `Gc` type that implements the same interface as Arc:

```rust
pub struct Gc<T>
where
  T: ?Sized,
{ /* private fields */ }
```

Fundamentally the `Gc` type intends to mimic the behavior of a scheme variable.
It can be read from and written to and arbitrarily passed around as a
reference. Scheme variables can be passed to Rust code as a `Gc<Value>`. 

Allocating a new garbage-collected T is done via the `Gc::new` function, and
creating a new copy of the pointer to that object is done via `Gc::clone`
method.

Additionally, the Gc type provides a `write` and `read` method in order to
provide thread-safe mutations and reads to the allocated data. Since GoukiScheme
can arbitrarily spawn threads that reference shared or global variables, it
is important for all writes and reads to variables to be safe and atomic. This
is acheived by embedded a `RwLock` in the Gc type.

After the `init_gc` function is called once, GoukiScheme spawns a task
dedicated to collecting garbage. This task is shut down automatically when the
main function returns. This provides a smooth and intuitive interface to the
memory manager that is also performant.

```rust
#[tokio::main]
fn main() {
  init_gc();
  let a = Gc::new("hello");
  let b = a.clone();
  {
    *a.write() = "world";
  }
  assert_eq!(b.read(), "world");
}
```

No signal needs to be sent to shutdown the collector thread. This interface is
only possible with async Rust.

Whenever a `Gc` is cloned or dropped, the change in reference count is send to
the collector task over an unbounded channel called the mutation buffer. The
collector task receives those mutations in a loop, in what essentially amounts
to the following code:

```rust
fn init_gc() {
  // Spawn the collector task:
  let _ = spawn(async move {
    let mut mutations = Vec::new();
    loop {
       BUFFER.recv_many(
         &mut mutations
       ).await;
       process_mutation(mutation);
       mutations.clear();
    } 
  });
}
```

The collector can be a _task_ because its loop is _receiving from an
asynchronous channel_. Since it is a _task_, that means that it is subject to
cancellation when it yields at `.await` points. One common cause of cancellation
is when the Tokio runtime is shutdown, i.e. when the main function returns.
Tokio provides the building blocks to create a clean memory management API.

== Tracing and finalizing 

One limitation of the cycle collection scheme and therefore the `Gc` smart
pointer is the need to enumerate every reference from within a single `Gc` in
order to properly determine cycles. Additionally, properly finalizing a cycle is
non-trivial and needs information about the contents of the type. This limits us
to storing types in a Gc that implement the `Trace` trait:

```rust
unsafe trait Trace: 'static {
  unsafe fn visit_children(
    &self,
    visitor: unsafe fn(OpaqueGcPtr)
  );

  unsafe fn finalize(&mut self);
}
```

Because this trace is very difficult to implement correctly, a Rust procedural
macro known as a derives macro is provided to allow users to implement this for
their own types:

```rust
#[derive(Trace)]
struct NamedCell {
  name: &'static str,
  cell: Gc<Value>,
}

let foo = Gc::new(NamedCell { .. });
```

Despite this, some Rust types cannot implement Trace properly. Storing these
types requires an extra level of indirection, via an `Arc`. While this provides
an escape hatch, special care needs to be taken to avoid cycles, as the cycle
collector cannot enumerate references in an Arc. In a later section we discuss
and alternative to this for types that are passive data structures. 

== Enabling back-pressure from garbage collector

Because we are running the garbage collector concurrently and are sending
mutations to the collector over an unbounded channel, the allocating tasks
receive no back pressure from the collector task. As a result should the pace
of new allocations out pace the collectors ability to free garbage, the process
would exhibit what appears to be a memory leak. While rare, it can satill occur
and thus we allow for a way to convert the unbounded channel into backpressure:

```rust
async fn yield_until_gc_cleared() {
  while PENDING_MUTATIONS
    > MAX_ALLOWED
  {
    tokio::task::yield_now().await
  }
}
```

This function is automatically called in the evaluation trampoline to prevent
runaway memory allocations. This means that while the collection scheme is
always concurrent, it is only _parallel_ for some amount of pending mutations.

= Values

The `Value` type represents a Scheme Value. It has the dual responsibility of
story any possible Scheme value while also allowing for the storage of any Rust
values. Additionally, if a Scheme value has a reasonable native equivalent in
Rust (which almost all of them do), conversion should be convenient and
efficient. The Value type should also be convenient to pass to and use in our
JIT compiled functions.

```rust
#[repr(transparent)]
pub struct Value(u64);
```

We use a tagged pointer scheme to achieve these constaints with minimal
overhead. A rather large tag size of four bits allowing for 16 tags in order to
represent the most common Scheme values without an extra level of indirection.

```rust
pub enum ValueType {
    Null = 0,
    Boolean = 1,
    Character = 2,
    Number = 3,
    String = 4,
    Symbol = 5,
    Vector = 6,
    ByteVector = 7,
    Syntax = 8,
    Closure = 9,
    Record = 10,
    RecordType = 11,
    Pair = 12,
    Any = 13,
    HashMap = 14,
}
```

As a small optimization, an extra "Undefined" value, the result of reading a
variable that has not been defined, is a Pair with a null pointer, or a value
of `12`.

This large number of tags allows us to map all of the primitive values in the
serde data model into a single Value without an extra level of indirection. As
a result, any Rust type that implements Serialize can automatically be serialized
into a Value.

We also utilize Rust's dynamic dispatch to allow for the storing of any type that
implements the `SchemeCompatible` and `Trace` traits. Using this trait we can
create new Scheme values that present themselves as sealed records.

```rust
trait SchemeCompatible: Any {
  fn record_type(&self) -> Arc<RecordType>;

  fn eqv(&self, rhs: &Value) -> bool;
}
```

Objects that implement this trait can be stored as a `Gc<dyn SchemeCompatible>`,
which can then be converted into a `Value`. Values, in turn, can be converted
back to a `Gc<dyn SchemeCompatible>`, which can then be downcast to a concrete
type.

= Evaluation

Evaluation of Scheme code in Rust is achieved through two primary objects: the
`Closure` and `Application` struct.

As the name implies, the `Closure` struct represents a Scheme closure, which
includes a captured environment and some code:

```rust
struct Closure {
  env:     Box<[Gc<Value>]>,
  func:    FuncPtr,
  runtime: Gc<Runtime>,
  /* fields omitted */
}
```

The env field points to an array of variables that can be accessed from the
function. It includes all non-local variables that are captured as part of the
reifying a function definition into a closure.

The function pointer type `FuncPtr` is the heart of the bridge between
asynchronous Rust and Scheme code. It is sum type of three different pointer
types:

```rust
pub enum FuncPtr {
    Continuation(ContinuationPtr),
    Closure(ClosurePtr),
    Bridge(BridgePtr),
}

/// A function pointer to a generated continuation.
pub type ContinuationPtr =
  unsafe extern "C" fn(
    runtime: *mut Runtime,
    env: *const *mut Value,
    args: *const Value,
    // args omitted 
  ) -> *mut Result<
              Application,
              Condition
            >;

/// A function pointer to a generated closure function.
pub type ClosurePtr =
  unsafe extern "C" fn(
    runtime: *mut Runtime,
    env: *const *mut Value,
    args: *const Value,
    cont: *const Value,
    // args omitted 
  ) -> *mut Result<
              Application,
              Condition
            >;

/// A function pointer to an async Rust bridge function.
pub type BridgePtr = for<'a> fn(
    args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    // args omitted
) -> BoxFuture<
        'a,
         Result<Application, Value>
     >;
```

The `ContinuationPtr` and `ClosurePtr` types are JIT compiled Scheme functions
and the `BridgePtr` type is a Rust function that has been cast to a function
pointer. Therefore all Scheme code is treated synchronously but can switch over
to pure Rust whenever it needs to await the result of a future. This is acheived
by a trampoline; each call to a function returns an application to antoher
function:

```rust
pub struct Application {
  op:   Option<Gc<Closure>>,
  args: Vec<Value>,
  /* some fields omitted */
}
```

However, in the case of Rust bridge functions - which can be potentially async -
the function returns the _future_ of an application. That means the body of
trampoline loop is async:

```rust
impl Application {
  pub async fn(mut self) -> Result<Vec<Value>, Exception> {
    while let Application {
       op: Some(op),
       args,
    } = self
    {
      self = op.apply(&args).await;
    }
  }
}
```

This allows for the JIT compiled `ContinuationPtr` and `ClosurePtr`s and for the
Rust bridge functions to seamlessly interact with each other. The synchronous
JIT compiled function pass their continuation to the async Rust code to evaulate the
async result after it is awaited in the CPS trampoline.

= Registering bridge functions

Using the `inventory` crate we can create a global registry of Rust functions
which can be imported into Scheme code. Functions with the signature of
`BridgePtr` can be registered manually, but a proc macro is provided to convert
functions of many different signatures:

```rust
#[bridge(
  name = "read-file-to-string",
  lib = "(tokio)"
)]
async fn read_file(file: &Value) ->
  Result<Vec<Value>, Condition>
{
  let file = file.to_string();
  let contents =
    read_to_string(&file)
    .await
    .map_err(|_| Condition::Error)?;
  Ok(vec![Value::from(contents)])
}
```

Every function that registers with this inventory will be available
automatically defined in their respective library in any `Registry` object
created, GoukiScheme's notion of a package registry. 

= JIT Compilation

After expansion, Scheme code is converted to a simple CPS mid level IR, which is then
converted into LLVM SSA for JIT compilation. Interacting with the LLVM JIT
compiler to produce an object that can easily be interacted with in Rust (i.e. a
`Closure` object described earlier) requires some care.

When a Continuation goes out of scope, we need a way to free the memory
allocated for its function. LLVM's JIT compiler requires that we only have one
handle to this memory per thread, which we call the Context. In order to provide as
safe interface that satisfies this gaurantee, we create a handle to a separate
thread that fully owns the JIT executor, called the `Runtime`:

```rust
struct Runtime {
  comp_tasks_tx:
    Sender<CompilationTask>
}
```

Creating a new Runtime spawns a new thread which handles all of our compilation
tasks. When the Runtime goes out of scope, its sender is dropped and the
compilation task exits, freeing all of the JIT compiled functions.

Once we have a context for which we can JIT compile functions, we need to send
those functions back to the caller, via function pointers. In Rust function
pointers are of the static lifetime, which is obviously incorrect in this
instances as lifetime of Runtime is not gauranteed to be static. Therefore, we
need to add a pointer from the Closure back to the Runtime from which it was
created:

```rust
struct Closure {
  ...
  runtime: Gc<Runtime>,
}
```

= Error system

= Putting it all together

Now we have all of the requisite pieces, we can construct a Rust program that
executes async Scheme code at runtime:

```rust
let rt = Runtime::new();
let registry =
  Registry::new(&runtime).await;
let env = Environment::from(
  registry.import("(base)")
);
let value = rt.eval(
  &env,
  "some very interesting scheme code here"
).await.unwrap();
println!("{value}");
```

= Conclusion

GoukiScheme provides a purely safe, easy-to-use interface for interacting with
async Scheme code from within async Rust while also providing advanced
optimizations like concurrent garbage collection and JIT compilation. 

= Notes

- Add citations for how async rust is difficult to debug 
