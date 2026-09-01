(import (rnrs) (test))

;; Reading a variable has to yield a counted reference to its contents. A `set!`
;; can otherwise drop the last reference to the value that was just read, and
;; the allocator hands that storage straight to the next allocation.

(define g (/ 1.0 3.0))

(define (read-then-clobber n)
  (let ((v g))
    (set! g n)
    (let ((decoy (+ 7.0 n)))
      (list v decoy))))

(let loop ((n 0.0))
  (when (< n 32.0)
    (set! g (/ 1.0 3.0))
    (assert-equal? (read-then-clobber n) (list (/ 1.0 3.0) (+ 7.0 n)))
    (loop (+ n 1.0))))
