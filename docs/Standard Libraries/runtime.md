# Runtime

The runtime library `(import (runtime))` provides procedures for interacting 
with the `scheme-rs` runtime.

## `collect-garbage` _procedure_

```scheme
(collect-garbage)
```

`scheme-rs` use a technique for garbage collection that interacts well with 
the Rust runtime known as concurrent cycle collection[^1]. This means that the 
garbage collector for `scheme-rs` runs completely in parallel with the rest of 
the program. That can mean that ocassionally mean that the collector falls 
behind if the amount of allocations out paces its ability to quickly collect it.
To remedy this, a scheme program can force a pause by calling `collect-garbage`.

Over time the garbage collector should improve and the need for this should 
become less and less.

[^1]: BACON, D. F., AND RAJAN, V. T. Concurrent cycle collection in reference counted systems. In European Conference on Object-Oriented Programming (Budapest, Hungary, June 2001), J. L. Knudsen, Ed., vol. 2072 of Lecture Notes in Computer Science, Springer-Verlag, pp. 207–235
