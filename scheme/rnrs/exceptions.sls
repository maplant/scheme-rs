(library (rnrs exceptions (6))
  (export guard
          (import (rnrs exceptions builtins (6))))
  (import (rnrs base))

  (define-syntax guard-inner
    (syntax-rules (else =>)
      [(_ continue var (else result1 result2 ...))
       (continue (begin result1 result2 ...))]
      [(_ continue var (test => result))
       (let ((temp test))
         (if temp (continue (result temp))))]
      [(_ continue var (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (continue (result temp))
             (guard-inner continue var clause1 clause2 ...)))]
      [(_ continue var (test)) (begin test (raise var))]
      [(_ continue var (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (continue temp)
             (guard-inner continue var clause1 clause2 ...)))]
      [(_ continue var (test result1 result2 ...))
       (if test
           (continue (begin result1 result2 ...))
           (raise var))]
      [(_ continue var (test result1 result2 ...)
          clause1 clause2 ...)
       (if test
           (continue (begin result1 result2 ...))
           (guard-inner continue var clause1 clause2 ...))]))
  
  (define-syntax guard
    (syntax-rules ()
      ([_ (var clause1 clausen ...) body]
       (call/cc
        (lambda (continue)
          (with-exception-handler
           (lambda (var)
             (guard-inner continue var clause1 clausen ...))
           (lambda () body))))))))
