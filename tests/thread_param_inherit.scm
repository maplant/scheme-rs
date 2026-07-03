(import (rnrs) (rnrs parameters) (threads) (test))

(define p (make-parameter 42))

(p 99)

;; A spawned thread sees the parent's parameter values as of spawn time.
(let ((h (spawn (lambda () (p)))))
  (assert-equal? (join h) 99))

;; Values are snapshotted, not shared: a direct set in the child is
;; visible to the child but never to the parent.
(let ((h (spawn (lambda () (p 5) (p)))))
  (assert-equal? (join h) 5)
  (assert-equal? (p) 99))

;; parameterize in the child is likewise invisible to the parent.
(let ((h (spawn (lambda () (parameterize ((p 7)) (p))))))
  (assert-equal? (join h) 7)
  (assert-equal? (p) 99))
