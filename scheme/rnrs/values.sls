(library (rnrs values (6))
  (export values let-values let*-values)
  (import (rnrs base builtins (6))
          (rnrs syntax-rules (6))
          (rnrs base primitives (6)))

  (define (values . things)
    (call-with-current-continuation
     (lambda (cont) (apply cont things))))
  
  (define-syntax let-values
    (syntax-rules ()
      ((let-values (binding ...) body1 body2 ...)
       (let-values-helper1
        ()
        (binding ...)
        body1 body2 ...))))

  (define-syntax let-values-helper1
    ;; map over the bindings
    (syntax-rules ()
      ((let-values
           ((id temp) ...)
         ()
         body1 body2 ...)
       (let ((id temp) ...) body1 body2 ...))
      ((let-values
           assocs
         ((formals1 expr1) (formals2 expr2) ...)
         body1 body2 ...)
       (let-values-helper2
        formals1
        ()
        expr1
        assocs
        ((formals2 expr2) ...)
        body1 body2 ...))))

  (define-syntax let-values-helper2
    ;; create temporaries for the formals
    (syntax-rules ()
      ((let-values-helper2
        ()
        temp-formals
        expr1
        assocs
        bindings
        body1 body2 ...)
       (call-with-values
           (lambda () expr1)
         (lambda temp-formals
           (let-values-helper1
            assocs
            bindings
            body1 body2 ...))))
      ((let-values-helper2
        (first . rest)
        (temp ...)
        expr1
        (assoc ...)
        bindings
        body1 body2 ...)
       (let-values-helper2
        rest
        (temp ... newtemp)
        expr1
        (assoc ... (first newtemp))
        bindings
        body1 body2 ...))
      ((let-values-helper2
        rest-formal
        (temp ...)
        expr1
        (assoc ...)
        bindings
        body1 body2 ...)
       (call-with-values
           (lambda () expr1)
         (lambda (temp ... . newtemp)
           (let-values-helper1
            (assoc ... (rest-formal newtemp))
            bindings
            body1 body2 ...))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () body1 body2 ...)
       (let-values () body1 body2 ...))
      ((let*-values ((name1 expr1) (name2 expr2) ...)
         body1 body2 ...)
       (let-values ((name1 expr1))
         (let*-values ((name2 expr2) ...)
           body1 body2 ...))))))
