(import (rnrs) (rnrs parameters) (test) (async))

;; Task inheritance and isolation through tokio spawn.
(define p (make-parameter 'root))
(p 'before)
(await (spawn (lambda () (assert-equal? (p) 'before))))
(await (spawn (lambda () (p 'task-local) (assert-equal? (p) 'task-local))))
(assert-equal? (p) 'before)

;; future: snapshot at CREATION, not first poll. let-bound so creation,
;; mutation, and await sequence within one body. (A top-level define RHS
;; would trip the pre-existing top-level ordering bug - see the
;; linuss/toplevel-order branch - so this deliberately uses let.)
(define q (make-parameter 0))
(let ()
  (q 1)
  (let ((f (future (lambda () (q)))))
    (q 2)
    (assert-equal? (await f) 1)
    (assert-equal? (q) 2)))

;; parameterize binding visible in a task spawned inside the body.
(define r (make-parameter 'outer))
(parameterize ((r 'inner))
  (await (spawn (lambda () (assert-equal? (r) 'inner)))))
(assert-equal? (r) 'outer)
