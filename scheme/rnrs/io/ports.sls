(library (rnrs io ports (6))
  (export open-string-input-port
          (import (rnrs io builtins)))
  (import (rnrs base) (rnrs mutable-strings))

  (define (read-string input-string input-start output-string output-start count)
    (if (> count 0)
        (let ([offset (- count 1)])
          (string-set! output-string
                       (+ output-start offset)
                       (string-ref input-string (+ input-start offset)))
          (read-string input-string input-start output-string output-start offset))))

  (define (open-string-input-port input-string)
    (define curr-pos 0)
    (define length (string-length input-string))
    (make-custom-textual-input-port
     input-string
     ;; read!
     (lambda (output-string start count)
       (let ([adjusted-count (min count (- length curr-pos))])
         (read-string input-string curr-pos output-string start adjusted-count)
         (set! curr-pos (+ curr-pos adjusted-count))
         adjusted-count))
     ;; get-position
     (lambda () curr-pos)
     ;; set-position
     (lambda (pos) (set! curr-pos pos))
     ;; close
     #f)))
