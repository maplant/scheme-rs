(define (fib n)
   (define (iter a b count)
     (if (<= count 0)
         a
         (iter b (+ a b) (- count 1))))
   (iter 0 1 n))

(fib 100)
