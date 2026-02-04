# Prompts 

The prompts library `(import (prompts))` provides dynamic delimited 
continuations.

Delimited continuations in Scheme are created by enclosing some procedure in 
what is called a _prompt_, so-called because it resembles the REPL prompt. Code
running in a prompt has the ability to abort to a prompt handler, passing along
its current continuation at the time. Unlike the continuation obtained from 
`call-cc`, the continuation of a prompt does not extent beyond the prompt 
handler. 

Dynamic delimited continuations further extend this by allowing prompts to be 
attached with a _prompt tag_. The prompt tag allows for code to decide at 
runtime which prompt they should extend to by invoking `abort-to-prompt` with
the desired prompt tag.

Continuations can be quite mind-bending by themselves, but dynamic delimited 
continuations take them to a whole new level. Experiment with them and most 
importantly have fun!

!!! warning
    Delimited continuations in scheme-rs are not fully fleshed out and are 
    expected to change in the future at least to some degree. 
    
    🐉 _Hic sunt dracones!_ 🐉

## `call-with-prompt` _procedure_

```scheme
(call-with-prompt tag thunk handler)
```

Installs a _prompt_ with the given _tag_ and _handler_ and invokes _thunk_. 
`tag` must be a Symbol. 

## `abort-to-prompt` _procedure_

``` scheme
(abort-to-prompt tag)
```

Calls the handler of the prompt installed with `tag` with the current 
continuation such that the current continuation does not extend past the prompt.

## Examples

``` scheme title="Example: installing a prompt" linenums="1"
(import (prompts))

(define print-goodbye
  (call-with-prompt
   'prompt
   (lambda ()
     (display "hello\n")
     (abort-to-prompt 'prompt)
     (display "goodbye\n"))
   (lambda (k) k)))
```

This code will print "hello", and every subsequent call to `print-goodbye` will
print "goodbye". This code can be thought of as functionally equivalent to the 
following:

``` scheme linenums="1"
(define print-goodbye
  (begin
    (display "hello\n")
    (lambda ()
      (display "goodbye\n"))))
```

