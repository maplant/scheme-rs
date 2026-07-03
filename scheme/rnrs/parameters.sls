(library (rnrs parameters)
  (export make-parameter parameter? parameterize)
  (import (rnrs) (rnrs parameters bridge))

  (define (make-parameter init . args)
    (let* ((converter (if (null? args) #f (car args)))
           (converted-init (if converter (converter init) init))
           (param (%make-parameter converted-init (or converter #f))))
      (case-lambda
        (() (%parameter-ref param))
        ((val)
         (let ((prev (%parameter-ref param)))
           (%parameter-set! param (if converter (converter val) val))
           prev)))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ () body ...)
       (begin body ...))
      ((_ ((param val) ...) body ...)
       (let ((procs (list param ...))
             (vs (list val ...)))
         (let* ((new-vals vs)
                (swap!
                 (lambda ()
                   (let ((old-vals (map (lambda (p) (p)) procs)))
                     (for-each (lambda (p v) (p v)) procs new-vals)
                     (set! new-vals old-vals)))))
           (dynamic-wind swap! (lambda () body ...) swap!)))))))
