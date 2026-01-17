(library (benches yin-yang)
  (export run)
  (import (rnrs))

  (define (run)
    (call/cc
     (lambda (exit)
       (define iters-limit 100)
       (define iters 0)
       (let* ((yin
               ((lambda (cc)
                  #;(display "@")
                  (set! iters (+ iters 1))
                  (if (= iters iters-limit) exit cc))
                (call/cc (lambda (c) c))))
              (yang
               ((lambda (cc)
                  #;(display "*")
                  cc)
                (call/cc (lambda (c) c)))))
         (yin yang))))))
