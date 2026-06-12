(library (rnrs letrec (6))
  (export letrec*)
  (import (rnrs syntax-rules (6))
          (rnrs base primitives))

  (define-syntax letrec*
    (syntax-rules ()
      ((letrec* ((var1 init1) ...) body1 body2 ...)
       (let ((var1 $undefined) ...)
         (set! var1 init1) ...
         (let () body1 body2 ...))))))
