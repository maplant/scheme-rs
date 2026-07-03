(import (rnrs) (rnrs parameters) (async) (test))

(define p (make-parameter 42))

(p 99)

;; A spawned task sees the parent's parameter values as of spawn time.
(let ((f (spawn (lambda () (p)))))
  (assert-equal? (await f) 99))

;; Values are snapshotted, not shared: a direct set in the child is
;; visible to the child but never to the parent.
(let ((f (spawn (lambda () (p 5) (p)))))
  (assert-equal? (await f) 5)
  (assert-equal? (p) 99))

;; parameterize in the child is likewise invisible to the parent.
(let ((f (spawn (lambda () (parameterize ((p 7)) (p))))))
  (assert-equal? (await f) 7)
  (assert-equal? (p) 99))

;; future bodies run lazily but snapshot at creation time.
(let ((f (future (lambda () (p)))))
  (p 123)
  (assert-equal? (await f) 99))
