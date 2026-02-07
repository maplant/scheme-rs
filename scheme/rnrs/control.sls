(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (rnrs syntax-case (6))
          (rnrs letrec (6))
          (rnrs base builtins (6))
          (rnrs base primitives (6)))

  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
        ((_ (i ...) ((keyword . pattern) template) ...)
         (syntax (lambda (x)
                   (syntax-case x (i ...)
                     ((dummy . pattern) (syntax template))
                     ...)))))))

    (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))

  (define-syntax do
    (syntax-rules ()
      ((do ((var init step ...) ...)
           (test expr ...)
         command ...)
       (letrec
           ((loop
             (lambda (var ...)
               (if test
                   (begin
                     #f ; avoid empty begin
                     expr ...)
                   (begin
                     command
                     ...
                     (loop (do "step" var step ...)
                           ...))))))
         (loop init ...)))
      ((do "step" x)
       x)
      ((do "step" x y)
       y)))

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))

  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (length '(x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (length '(x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                  args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args)))))
