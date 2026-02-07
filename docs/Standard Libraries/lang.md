# Lang

The base language library `(import (lang))` provides the most bare-bones 
standard scheme syntax and functions to get started. 

!!! warning
    This library is largely incomplete and _not_ everything that `scheme-rs` has
    to offer! Try out `(import (rnrs))` for a more complete language and consult
    the [r6rs specification](https://www.r6rs.org/final/r6rs.pdf) and
    [r6rs standard libraries specification](https://www.r6rs.org/final/r6rs-lib.pdf).

## `define` _syntax_

### Defining variables

```scheme
(define ⟨variable name⟩ ⟨expression⟩)
```

Defines a variable with the name `variable` and binds it to the result of `expression`

``` scheme title="Example:" linenums="1"
(define pi 3.1415)
```

### Defining funtions

``` scheme
(define (⟨function name⟩ ⟨arg⟩ ... [ . ⟨rest args⟩ ]) ⟨body⟩)
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

- `(lambda (⟨arg1⟩ ... ⟨argn⟩) ⟨body⟩)` Defines an anonymous procedure that takes `n`
  arguments and applies them to body.
- `(lambda (⟨arg1⟩ ... ⟨argn⟩ . ⟨var args⟩) ⟨body⟩)` Defines an anonymous procedure that
  takes at least `n` arguments and applies them to body. Any extra arguments are
  bound to `⟨var args⟩` as a list.
- `(lambda ⟨args⟩ ⟨body⟩)` Defines an anonymous procedure that takes any number of
  arguments and binds them to `⟨args⟩`.

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

## `let`, `let*` and `letrec` _syntax_

### `let`

The `let` keyword is used to define lexical bindings for local variables. 

```scheme
(let ((⟨var⟩ ⟨expr⟩) ...) ⟨body⟩)
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

### `let*` 

`let*` is similar to `let`, but each subsequent binding has access to the 
previous:

```scheme
(let* ((⟨var⟩ ⟨expr⟩) ...) ⟨body⟩)
```

The following code

``` scheme title="Example: let* bindings" linenums="1"
(let* ([a 1]
       [b (+ a 1)]
       [c (+ b 1)])
  (+ a b c))
```

is equivalent to:

``` scheme linenums="1"
(let ([a 1])
  (let ([b (+ a 1)])
    (let ([c (+ b 1)])
      (+ a b c))))
```

### `letrec`

`letrec` allows for the creation of recursive (or even mutually recursive) bindings:

```scheme
(let* ((⟨var⟩ ⟨expr⟩) ...) ⟨body⟩)
```

``` scheme title="Example: letrec factorial" linenums="1"
(letrec ((factorial
          (lambda (n)
            (if (= n 1)
                1
                (* n (factorial (- n 1)))))))
  (factorial 5))
```

## `set!` _syntax_

``` scheme
(set! ⟨var⟩ ⟨expr⟩)
```

`set!` allows for the mutation of variables. 

``` scheme title="Example: setting a value" linenums="1"
(define (add-one x)
  (set! x (+ x 1))
  x)

(add-one 3) ; => 4
```

Values that are exported are considered to be immutable and attempting to set 
them is a syntax violation.

## `quote` _syntax_

``` scheme
(quote ⟨expr⟩)
'⟨expr⟩
```

`quote` returns its argument literally without evaluation. It is often referred
to by it's alias, `'`. 
