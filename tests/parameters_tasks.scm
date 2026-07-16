(import (rnrs) (rnrs parameters) (test) (threads (1)))

;; Inheritance: value at spawn time is visible in the child.
(define p (make-parameter 'default))
(p 'before-spawn)
(join (spawn (lambda () (assert-equal? (p) 'before-spawn))))

;; Isolation: a bare set in the child is child-local (guile-fibers).
(join (spawn (lambda ()
               (p 'child-value)
               (assert-equal? (p) 'child-value))))
(assert-equal? (p) 'before-spawn)

;; Snapshot at spawn, not a live link: a parent mutation after the child
;; has been joined does not retroactively appear anywhere, and a child
;; that joined before the mutation saw the old value.
(define q (make-parameter 0))
(q 1)
(join (spawn (lambda () (assert-equal? (q) 1))))
(q 2)
(join (spawn (lambda () (assert-equal? (q) 2))))
(assert-equal? (q) 2)

;; parameterize binding visible in a task spawned inside the body
;; (R7RS: "threads created inside <body>").
(define r (make-parameter 'outer))
(parameterize ((r 'inner))
  (join (spawn (lambda () (assert-equal? (r) 'inner)))))
(assert-equal? (r) 'outer)

;; ...and the child's mutation of that binding is its own copy:
(define s (make-parameter 0))
(parameterize ((s 10))
  (join (spawn (lambda () (s 99) (assert-equal? (s) 99))))
  (assert-equal? (s) 10))
