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
        [(_ name builtin-rtd constructor predicate)
          #'(begin
             (define rec-rtd (builtin-rtd))
             (define rec-rcd (make-record-constructor-descriptor rec-rtd #f #f))
             (define constructor (record-constructor rec-rcd))
             (define predicate (record-predicate rec-rtd))
             (define-syntax name
               (lambda (x)
                 (syntax-case x (rtd rcd)
                   [(_ rtd) #'rec-rtd]
                   [(_ rcd) #'rec-rcd]))))]))))
