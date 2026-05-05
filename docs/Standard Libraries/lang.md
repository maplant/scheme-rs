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

## `include` _syntax_

``` scheme
(include ⟨filename⟩)
```

Inserts the contents of ⟨filename⟩ at the point of expansion.

## Pattern matching

### `match` _syntax_

``` scheme 
(match ⟨expr⟩ ⟨clause⟩ ...)
```
Provides facilities for pattern matching. The pattern matcher provided by 
scheme-rs is known as the Wright-Cartwright-Shinn matcher thanks to its 
authors. It has the following grammar:

```
        patterns:                       matches:

  pat ::= identifier                      anything, and binds identifier
        | _                               anything
        | ()                              the empty list
        | #t                              #t
        | #f                              #f
        | string                          a string
        | number                          a number
        | character                       a character
        | 'sexp                           an s-expression
        | 'symbol                         a symbol (special case of s-expr)
        | (pat_1 ... pat_n)               list of n elements
        | (pat_1 ... pat_n . pat_{n+1})   list of n or more
        | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
                                          of remainder must match pat_n+1
        | #(pat_1 ... pat_n)              vector of n elements
        | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
                                          of remainder must match pat_n+1
        | #&pat                           box
        | ($ record-name pat_1 ... pat_n) a record
        | (= field pat)                   a ``field'' of an object
        | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
        | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
        | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
        | (? predicate pat_1 ... pat_n)   if predicate true and all of
                                          pat_1 thru pat_n match
        | (set! identifier)               anything, and binds setter
        | (get! identifier)               anything, and binds getter
        | `qp                             a quasi-pattern
        | (identifier *** pat)            matches pat in a tree and binds
        identifier to the path leading
        to the object that matches pat

  ooo ::= ...                             zero or more
        | ___                             zero or more
        | **1                             1 or more

  quasi-patterns:                 matches:

  qp  ::= ()                              the empty list
        | #t                              #t
        | #f                              #f
        | string                          a string
        | number                          a number
        | character                       a character
        | identifier                      a symbol
        | (qp_1 ... qp_n)                 list of n elements
        | (qp_1 ... qp_n . qp_{n+1})      list of n or more
        | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
                                          of remainder must match qp_n+1
        | #(qp_1 ... qp_n)                vector of n elements
        | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
                                          of remainder must match qp_n+1
        | #&qp                            box
        | ,pat                            a pattern
        | ,@pat                           a pattern
```


``` scheme title="Example: match expression" linenums="1"
(match '(hello world)
  ((a)   #f)
  ((a b) (list b a)))
⇒ (world hello)

(match '(hello)
  ((a)   #f)
  ((a b) (list b a)))
⇒ #f

(match #(1 2 3) (#(a b c) (+ a b c)))
⇒ 6
```


### `match-let` _syntax_

``` scheme 
(match-let ((⟨pattern⟩ ⟨expr⟩) ...) body)
```

Matches each pattern to the given expression and evaluates with the bond 
variables. Raises an error if any of the expression fail to match.

``` scheme title="Example: match expression" linenums="1"
(match-let (((a b) '(1 2))
            ((c d) '(3 4)))
  (list d c b a))
⇒ (4 3 2 1)
```

### `match-let*` _syntax_

``` scheme 
(match-let* ((⟨pattern⟩ ⟨expr⟩) ...) body)
```

`let*` version of `match-let`.

### `match-letrec` _syntax_

``` scheme 
(match-letrec ((⟨pattern⟩ ⟨expr⟩) ...) body)
```

`letrec` version of `match-let`
