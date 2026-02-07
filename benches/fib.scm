(library (benches fib)
  (export run)
  (import (rnrs))

  (define (fib n)
     (define (iter a b count)
       (if (<= count 0)
           a
           (iter b (+ a b) (- count 1))))
     (iter 0 1 n))

  (define (run) (fib 10000)))
