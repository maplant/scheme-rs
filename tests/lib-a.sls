 (library (tests lib-a)
    (export macro-a var-a)
    (import (rnrs))
    (define var-a 1)
    (define-syntax macro-a
      (syntax-rules ()
        ((_ x) (+ x 1)))))
