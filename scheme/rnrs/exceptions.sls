(library (rnrs exceptions (6))
  (export guard
          (import (rnrs exceptions builtins (6))))
  (import (rnrs base))

  (define-syntax guard-inner
    (syntax-rules (else =>)
      ((_ var (else result1 result2 ...))
       (begin result1 result2 ...))
      ((_ var (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((_ var (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (guard-inner var clause1 clause2 ...))))
      ((_ var (test)) (begin test (raise var)))
      ((_ var (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (guard-inner var clause1 clause2 ...))))
      ((_ var (test result1 result2 ...))
       (if test
           (begin result1 result2 ...)
           (raise var)))
           
      ((_ var (test result1 result2 ...)
                 clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (guard-inner var clause1 clause2 ...)))))
  
  (define-syntax guard
    (syntax-rules ()
      ([_ (var clause1 clausen ...) body]
       (with-exception-handler
        (lambda (var)
          (guard-inner var clause1 clausen ...))
        (lambda () body))))))
