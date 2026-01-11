(library (rnrs conditions (6))
  (export define-condition-type
          &condition
          &message make-message-condition
          &warning make-warning
          &serious make-serious-condition
          &trace make-trace
          &error make-error
          &import make-import-condition
          &violation make-violation
          &assertion make-assertion-violation
          &lexical make-lexical-violation
          &syntax make-syntax-violation
          &undefined make-undefined-violation
          (import (only (rnrs conditions builtins) simple-conditions condition)))
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs conditions builtins (6))
          (rnrs records procedural (6))
          (rnrs records syntactic (6)))

  (define-syntax from-builtin
    (lambda (x)
      (syntax-case x ()
        [(_ name constructor builtin)
         #'(begin
             (define rtd (builtin))
             (define rcd (make-record-constructor-descriptor rtd #f #f))
             (define constructor (record-constructor rcd))
             (define-syntax name
               (lambda (x)
                 (syntax-case x (rtd rcd)
                   [(_ rtd) #'rtd]
                   [(_ rcd) #'rcd]))))])))

  (define-syntax define-condition-type
    (lambda (x)
      (syntax-case x ()
        [(_ condition-type
            supertype
            constructor predicate
            (field accessor) ...)
         #'(begin
             (define-record-type (condition-type constructor simple-predicate)
               (parent supertype)
               (fields (immutable field accessor) ...))
             (define (predicate x)
               (call/cc (lambda (return)
                          (begin
                            (for-each
                             (lambda (condition)
                               (if (simple-predicate condition)
                                   (return #t)))
                             (simple-conditions x))
                            #f)))))])))

  (from-builtin &condition make-condition &condition-rtd)
  (from-builtin &message make-message-condition &message-rtd)
  (from-builtin &warning make-warning &warning-rtd)
  (from-builtin &serious make-serious-condition &serious-rtd)
  (from-builtin &trace make-trace &trace-rtd)
  (from-builtin &error make-error &error-rtd)
  (from-builtin &import make-import-condition &import-rtd)
  (from-builtin &violation make-violation &violation-rtd)
  (from-builtin &assertion make-assertion-violation &assertion-rtd)
  (from-builtin &lexical make-lexical-violation &lexical-rtd)
  (from-builtin &syntax make-syntax-violation &syntax-rtd)
  (from-builtin &undefined make-undefined-violation &undefined-rtd))
        
