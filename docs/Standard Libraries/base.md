# Base

The base library `(import (base))` provides the most bare-bones standard scheme
syntax and functions to get started. Base is re-exported by all of the other 
standard libraries to reduce friction. 

## `define` _syntax_

### Defining variables

```scheme
(define variable expression)
```

Defines a variable with the name `variable` and binds it to the result of `expression`

``` scheme title="Example:" linenums="1"
(define pi 3.1415)
```

### Defining funtions

``` scheme
(define (function arg ... [ . rest-args ]) body)
```

Defines a function. A function can have any number of arguments and optionally end 
in `. variable-name` to express variable arity.

``` scheme title="Example: factorial function" linenums="1"
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
```

## `lambda` syntax

The `lambda` keyword is used to define anonymous procedures. It has three key forms:

- `(lambda (arg1 ... argn) body)` Defines an anonymous procedure that takes `n`
  arguments and applies them to body.
- `(lambda (arg1 ... argn . var-args) body)` Defines an anonymous procedure that
  takes at least `n` arguments and applies them to body. Any extra arguments are
  bound to `var-args` as a list.
- `(lambda args body)` Defines an anonymous procedure that takes any number of
  arguments and binds them to `args`.

`lambda` functions can capture their environment. That is to say, variables 
bound outside the scope of the lambda are captured.

``` scheme title="Example: captured variables" linenums="1"
(define g 0)

(define next-g 
    (lambda ()
        (let ((curr-g g))
            (set! g (+ g 1))
            curr-g)))
```

