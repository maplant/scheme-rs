(library (rnrs conditions (6))
  (export simple-conditions
          condition condition?
          define-condition-type
          condition-predicate
          condition-accessor
          &condition
          &message make-message-condition message-condition? condition-message
          &warning make-warning warning?
          &serious make-serious-condition serious-condition?
          &trace make-trace trace?
          &error make-error error?
          &import make-import-condition import-condition?
          &violation make-violation violation?
          &assertion make-assertion-violation assertion-violation?
          &irritants make-irritants-condition irritants-condition? condition-irritants
          &who make-who-condition who-condition? condition-who
          &non-continuable make-non-continuable-violation non-continuable-violation? 
          &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation? 
          &lexical make-lexical-violation lexical-violation?
          &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
          &undefined make-undefined-violation undefined-violation?)
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs records procedural (6))
          (rnrs records syntactic (6)))

  (define (condition-predicate rtd)
    (let [(record-predicate (record-predicate rtd))]
      (lambda (x)
        ;; TODO: Add error for if x is not a condition
        (call/cc (lambda (return)
                         (begin
                           (for-each
                            (lambda (condition)
                              (if (record-predicate condition)
                                  (return #t)))
                            (simple-conditions x))
                           #f))))))

  (define (condition-accessor rtd proc)
   (let [(record-predicate (record-predicate rtd))]
     (lambda (x)
       ;; TODO: Add error for if x is not a condition
       (call/cc (lambda (return)
                        (begin
                          (for-each
                           (lambda (condition)
                             (if (record-predicate condition)
                                 (return (proc condition))))
                           (simple-conditions x))))))))

  (define-syntax from-builtin
    (lambda (x)
      (syntax-case x ()
        [(_ name constructor predicate builtin)
         #'(begin
             (define condition-rtd (builtin))
             (define condition-rcd (make-record-constructor-descriptor condition-rtd #f #f))
             (define constructor (record-constructor condition-rcd))
             (define predicate (condition-predicate condition-rtd))
             (define-syntax name
               (lambda (x)
                 (syntax-case x (rtd rcd)
                   [(_ rtd) #'condition-rtd]
                   [(_ rcd) #'condition-rcd]))))])))

  (define-syntax define-condition-type
    (lambda (x)
      (syntax-case x ()
        [(_ condition-type
            supertype
            constructor predicate
            (field accessor) ...)
         (with-syntax [((real-accessor ...) (generate-temporaries #'(accessor ...)))]
           #'(begin
               (define-record-type (condition-type constructor simple-predicate)
                 (parent supertype)
                 (fields (immutable field real-accessor) ...))
               (define (predicate x)
                 (call/cc (lambda (return)
                            (begin
                              (for-each
                               (lambda (condition)
                                 (if (simple-predicate condition)
                                     (return #t)))
                               (simple-conditions x))
                              #f))))
               (define accessor
                 (condition-accessor (record-type-descriptor condition-type) real-accessor)) ...))])))

  (from-builtin &condition make-condition simple-condition? &condition-rtd)  
  (from-builtin &message make-message-condition message-condition? &message-rtd)
  (define condition-message (condition-accessor (&message-rtd) (record-accessor (&message-rtd) 0)))
  (from-builtin &warning make-warning warning? &warning-rtd)
  (from-builtin &serious make-serious-condition serious-condition? &serious-rtd)
  (from-builtin &trace make-trace trace? &trace-rtd)
  (define condition-trace (condition-accessor (&trace-rtd) (record-accessor (&trace-rtd) 0)))
  (from-builtin &error make-error error? &error-rtd)
  (from-builtin &import make-import-condition import-condition? &import-rtd)
  (from-builtin &violation make-violation violation? &violation-rtd)
  (from-builtin &assertion make-assertion-violation assertion-violation? &assertion-rtd)
  (from-builtin &irritants make-irritants-condition irritants-condition? &irritants-rtd)
  (define condition-irritants (condition-accessor (&irritants-rtd) (record-accessor (&irritants-rtd) 0)))
  (from-builtin &who make-who-condition who-condition? &who-rtd)
  (define condition-who (condition-accessor (&who-rtd) (record-accessor (&who-rtd) 0)))
  (from-builtin &non-continuable make-non-continuable-violation non-continuable-violation? &non-continuable-rtd)
  (from-builtin &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation? &implementation-restriction-rtd)
  (from-builtin &lexical make-lexical-violation lexical-violation? &lexical-rtd)
  (from-builtin &syntax make-syntax-violation syntax-violation? &syntax-rtd)
  (define syntax-violation-form (condition-accessor (&syntax-rtd) (record-accessor (&syntax-rtd) 0)))
  (define syntax-violation-subform (condition-accessor (&syntax-rtd) (record-accessor (&syntax-rtd) 1)))
  (from-builtin &undefined make-undefined-violation undefined-violation? &undefined-rtd))
        
