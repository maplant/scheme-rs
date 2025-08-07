(library (rnrs base (6))
  (export syntax-rules with-syntax cond case)
  (import (rnrs base builtins (6)))

  ;; syntax-rules is defined in terms of syntax-case:
  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
        ((_ (i ...) ((keyword . pattern) template) ...)
         (syntax (lambda (x)
                   (syntax-case x (i ...)
                     ((dummy . pattern) (syntax template))
                     ...)))))))

  ;; Likewise, so is with-syntax
  (define-syntax with-syntax
   (lambda (x)
     (syntax-case x ()
       ((_ ((p e0) ...) e1 e2 ...)
        (syntax (syntax-case (list e0 ...) ()
                  ((p ...) (let () e1 e2 ...))))))))

  (define-syntax cond
   (syntax-rules (else =>)
     ((cond (else result1 result2 ...))
      (begin result1 result2 ...))
     ((cond (test => result))
      (let ((temp test))
        (if temp (result temp))))
     ((cond (test => result) clause1 clause2 ...)
      (let ((temp test))
        (if temp
            (result temp)
            (cond clause1 clause2 ...))))
     ((cond (test)) test)
     ((cond (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
            temp
            (cond clause1 clause2 ...))))
     ((cond (test result1 result2 ...))
      (if test (begin result1 result2 ...)))
     ((cond (test result1 result2 ...)
            clause1 clause2 ...)
      (if test
          (begin result1 result2 ...)
          (cond clause1 clause2 ...)))))

  (define-syntax case
   (syntax-rules (else)
     ((case expr0
        ((key ...) res1 res2 ...)
        ...
        (else else-res1 else-res2 ...))
      (let ((tmp expr0))
        (cond
         ((memv tmp '(key ...)) res1 res2 ...)
         ...
         (else else-res1 else-res2 ...))))
     ((case expr0
        ((keya ...) res1a res2a ...)
        ((keyb ...) res1b res2b ...)
        ...)
      (let ((tmp expr0))
        (cond
         ((memv tmp '(keya ...)) res1a res2a ...)
         ((memv tmp '(keyb ...)) res1b res2b ...)
         ...))))))
