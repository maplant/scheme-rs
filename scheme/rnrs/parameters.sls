(library (rnrs parameters)
  (export make-parameter parameter? parameterize)
  (import (rnrs) (rnrs parameters bridge))

  (define (make-parameter init . args)
    (let* ((converter (if (null? args) #f (car args)))
           (converted-init (if converter (converter init) init)))
      (%make-parameter converted-init (or converter #f))))

  ;; Duplicate parameters in one parameterize: the last binding wins (matches Chez).
  (define-syntax parameterize
    (syntax-rules ()
      ((_ () body ...)
       (begin body ...))
      ((_ ((param val) ...) body ...)
       (let* ((ps (list param ...))
              (vs (map (lambda (rp v)
                         (let ((c (%parameter-converter rp)))
                           (if c (c v) v)))
                       ps
                       (list val ...))))
         (%call-with-parameterization ps vs (lambda () body ...)))))))
