(library (rnrs syntax-rules (6))
  (export syntax-rules with-syntax identifier-syntax)
  (import (rnrs base builtins (6))
          (rnrs base primitives (6))
          (rnrs syntax-case (6)))

  ;; Define syntax-rules in terms of syntax case 
  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
        ((_ (i ...) ((keyword . pattern) template) ...)
         (syntax (lambda (x)
                   (syntax-case x (i ...)
                     ((dummy . pattern) (syntax template))
                     ...)))))))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (syntax (syntax-case (list e0 ...) ()
                   ((p ...) (let () e1 e2 ...))))))))

  (define-syntax identifier-syntax
    (lambda (x)
      (syntax-case x (set!)
        [(_ e)
         #'(lambda (x)
             (syntax-case x ()
               [id (identifier? #'id) #'e]
               [(_ x (... ...)) #'(e x (... ...))]))]
        [(_ (id exp1) ((set! var val) exp2))
         (and (identifier? #'id) (identifier? #'var))
         #'(make-variable-transformer
            (lambda (x)
              (syntax-case x (set!)
                [(set! var val) #'exp2]
                [(id x (... ...)) #'(exp1 x (... ...))]
                [id (identifier? #'id) #'exp1])))]))))          
