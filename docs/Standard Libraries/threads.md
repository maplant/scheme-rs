# Threads

The threads library `(import (threads))` provides threading primitives. 

All values in scheme-rs are safe to send across threads and all mutations are 
atomic. 

## `spawn` _procedure_

```scheme
(spawn thunk)
```

Invokes `thunk` in a newly created thread and returns a [join handle](#join-handle-type).

## `join` _procedure_

```scheme
(join join-handle)
```

Takes a [join handle](#join-handle) and blocks until the thread returns, 
returning the values. After the thread has returned, the same values will be 
returned on subsequent calls.

If the thread throws an exception, `join` will re-throw the same exception each
time it is called.

Attempting to `join` a thread's own `join-handle` will throw an exception.

## `sleep` _procedure_

```scheme
(sleep milliseconds)
```

Block the thread until `milliseconds` have elapsed.

## `join-handle?` _procedure_

```scheme
(join-handle? obj)
```

Returns true if the object is a [join handle](#join-handle-type).

## `join-handle` _type_

Join handle represents the state of completion of a thread and contains whatever
values (or errors) were returned.

It can also be used a form of identification for a thread, via `eqv?`.
