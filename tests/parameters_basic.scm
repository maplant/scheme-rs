(import (rnrs) (rnrs parameters) (test))

;; default value
(define p (make-parameter 10))
(assert-equal? (p) 10)

;; bare set mutates the task root; read sees it
(p 42)
(assert-equal? (p) 42)

;; converter applied at creation and on set. The set returns zero
;; values (matching hashtable-set!/set-car!'s convention), not one
;; unspecified value -- (define x (p 3)) would error, so sequence via
;; begin, not via the return value.
(define c (make-parameter 5 (lambda (x) (* x 2))))
(assert-equal? (c) 10)
(c 3)
(assert-equal? (c) 6)

;; parameter? recognizes parameters and rejects plain procedures
(assert-equal? (parameter? p) #t)
(assert-equal? (parameter? car) #f)
