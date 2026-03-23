(library (generators (1))
  (export yield generator)
  (import (rnrs) (prompts))

  (define generator-prompt-tag (gensym))

  (define (yield . args)
    (apply abort-to-prompt (cons generator-prompt-tag args)))

  (define (generator func)
    (lambda ()
      (call-with-prompt
       generator-prompt-tag
       func
       (lambda (k . returned)
         (set! func k)
         (apply values returned))))))
