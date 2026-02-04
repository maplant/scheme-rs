# Lang

The base language library `(import (lang))` provides the most bare-bones 
standard scheme syntax and functions to get started. 

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

## `lambda` _syntax_

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

## `let` _syntax_

### Lexical bindings 

The `let` keyword is used to define lexical bindings for local variables. 

```scheme
(let ((var expr) ...) body)
```

Variables defined this way are only visible for their body. Variables are bound
to their expressions in order, but are not visible for each others binding 
expressions.

Let expressions return the last value returned in `body`.

``` scheme title="Example: let bindings" linenums="1"
(let ([x 1])
  (+ x (let ([x 2])
         x))) ; => returns 3
```

