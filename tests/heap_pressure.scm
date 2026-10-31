(import (rnrs))

;; Allocates several times the heap in short-lived pairs.
(let loop ((i 0))
  (when (< i 3000000)
    (cons i i)
    (loop (+ i 1))))
