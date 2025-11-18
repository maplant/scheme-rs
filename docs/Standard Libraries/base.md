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
