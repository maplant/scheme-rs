(library (rnrs letrec (6))
  (export letrec letrec*)
  (import (rnrs syntax-rules (6))
          (rnrs base primitives))

  (define-syntax letrec
    (syntax-rules ()
      ((letrec () body1 body2 ...)
       (let () body1 body2 ...))
      ((letrec ((var init) ...) body1 body2 ...)
       (letrec-helper
        (var ...)
        ()
        ((var init) ...)
        body1 body2 ...))))

  (define-syntax letrec-helper
   (syntax-rules ()
     ((letrec-helper
       ()
       (temp ...)
       ((var init) ...)
       body1 body2 ...)
      (let ((var $undefined) ...)
        (let ((temp init) ...)
          (set! var temp)
          ...)
        (let () body1 body2 ...)))
     ((letrec-helper
       (x y ...)
       (temp ...)
       ((var init) ...)
       body1 body2 ...)
      (letrec-helper
       (y ...)
       (newtemp temp ...)
       ((var init) ...)
       body1 body2 ...))))

  (define-syntax letrec*
    (syntax-rules ()
      ((letrec* ((var1 init1) ...) body1 body2 ...)
       (let ((var1 $undefined) ...)
         (set! var1 init1) ...
         (let () body1 body2 ...))))))
