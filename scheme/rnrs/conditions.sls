(library (rnrs conditions (6))
  (export &syntax)
  (import (rnrs base special-keywords (6))
          (rnrs syntax-case (6))
          (rnrs conditions builtins (6))
          (rnrs records procedural (6)))

  (define syntax-rtd (&syntax-rtd))
  (define syntax-rcd (make-record-constructor-descriptor syntax-rtd #f #f))
  
  (define-syntax &syntax
    (lambda (x)
      (syntax-case x (rtd rcd)
        [(_ rtd) #'syntax-rtd]
        [(_ rcd) #'syntax-rcd]))))
        
