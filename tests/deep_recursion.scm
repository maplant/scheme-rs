(import (rnrs) (test))

;; Returning to a continuation goes through the trampoline, so the ascent of a
;; non-tail recursion costs heap rather than native stack. This depth overflows
;; the native stack if the ascent nests natively.

(define (sum-to n)
  (if (= n 0)
      0
      (+ 1 (sum-to (- n 1)))))

(assert-equal? (sum-to 200000) 200000)
