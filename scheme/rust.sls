;; Scheme procedures for rust interoperability
(library (rust (1))
  (export define-rust-type)
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs records procedural (6)))

  ;; Defines a scheme record from a rust RTD.
  (define-syntax define-rust-type
    (lambda (x)
      (syntax-case x ()
        [(_ name rtd constructor predicate
          #'(begin
             (define rtd (builtin))
             (define rcd (make-record-constructor-descriptor rtd #f #f))
             (define constructor (record-constructor rcd))
             (define predicate (condition-predicate rtd))
             (define-syntax name
               (lambda (x)
                 (syntax-case x (rtd rcd)
                   [(_ rtd) #'rtd]
                   [(_ rcd) #'rcd])))))]))))
