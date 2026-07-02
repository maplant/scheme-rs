(import (rnrs) (test))

;; after-thunk return values must not disturb the exception unwinding through it.

(assert-equal?
 (guard (e (#t (condition-message e)))
   (dynamic-wind
       (lambda () #f)
       (lambda () (error 'inner "the real error"))
       (lambda () #f)))
 "the real error")
