(library (tests lib-b)
  (export macro-b)
  (import (rnrs) (tests lib-a))
  (define-syntax macro-b
    (syntax-rules ()
      ((_ x) (+ (macro-a (* x 2)) var-a)))))
