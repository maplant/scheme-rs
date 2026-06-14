(library (tests lib-c)
  (export make-wrapped wrap-value)
  (import (rnrs))

  (define (wrap-value v) (list 'wrapped v))

  (define-syntax make-wrapped
    (lambda (x)
      (syntax-case x ()
        ((_ (name . formals) body ...)
         #'(define (name . formals)
             (wrap-value (begin body ...))))))))
