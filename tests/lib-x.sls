(library (tests lib-x)
  (export val f loaded?)
  (import (rnrs))
  (define val 10)
  val
  (define (f x) (+ x 1))
  (f 1)
  (define loaded? #t))
