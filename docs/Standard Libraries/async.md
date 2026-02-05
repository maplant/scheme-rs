# Async

!!! note
    The async library requires the `async` and `tokio` features be enabled.

The async library `(import (async))` provides procedures for dealing with 
asynchronous tasks. 

By default in scheme-rs when `async` is enabled, all procedures are assumed to 
be async and do not need to be awaited. Awaiting is done automatically. This 
allows you to interface with async Rust code seemlessly if you do not care about
concurrency or parallelism. However, scheme-rs also allows you to convert any
procedure into a future, and to await or spawn [tokio tasks](https://docs.rs/tokio/latest/tokio/task/index.html).

## `future` _procedure_

``` scheme
(future thunk)
```

Converts the thunk into a future. Execution will be deferred until 
[await](#await-procedure) is called.

## `await` _procedure_

``` scheme
(await future)
```

Awaits the future and returns its values. If the future has already been 
completed the same values will be returned.

## `sleep` _procedure_

``` scheme
(sleep milliseconds)
```

Sleeps for `milliseconds`, yielding to the tokio executor.


## `spawn` _procedure_

``` scheme
(spawn thunk)
```

Spawns the thunk in a new [tokio task](https://docs.rs/tokio/latest/tokio/task/index.html)
and returns the future associated with completion of the task.

## `bind-tcp` _procedure_

``` scheme
(bind-tcp address)
```

Creates a new `tcp-listener` bound to `address`.

## `accept` _procedure_

``` scheme
(accept tcp-listener)
```

Blocks until a new connection is received by the listener. Returns a new socket
and the address of the connecting client. Sockets are binary input/output
_ports_.
