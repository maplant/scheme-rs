(library (rnrs enums (6))
  (export make-enumeration enum-set-universe enum-set-indexer
          enum-set-constructor enum-set->list enum-set-member? enum-set-subset?
          enum-set=? enum-set-union enum-set-intersection enum-set-difference
          enum-set-complement enum-set-projection define-enumeration)
  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs syntax-case (6)))

  (define (enum-set-indexer enum-set)
    (let* ((symbols (enum-set->list
                     (enum-set-universe enum-set)))
           (cardinality (length symbols)))
      (lambda (x)
        (cond
         ((memq x symbols)
          => (lambda (probe)
               (- cardinality (length probe))))
         (else #f)))))

  (define-syntax define-enumeration
    (lambda (x)
      (syntax-case x ()
        ([_ type-name
            (symbol ...)
            constructor-syntax]
         #'(begin
             (define-syntax type-name
               (lambda (x)
                 (syntax-case x (symbol ...)
                   ([_ symbol] #'(quote symbol)) ...)))
             (define enum (enum-set-constructor (make-enumeration '(symbol ...))))
             (define-syntax constructor-syntax
               (lambda (x)
                 ;; TODO: Compile time checks
                 (syntax-case x ()
                   [(_ symbols (... ...)) #'(enum (quote (symbols (... ...))))])))))))))
