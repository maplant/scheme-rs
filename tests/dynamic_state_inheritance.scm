(import (rnrs) (test) (threads (1)))

;; 1. RED: a hashtable hash function must observe the exception handler
;; installed by its caller. Today the hash function runs in a fresh, empty
;; ContBarrier, so raise-continuable finds no handler and the operation
;; fails; the guard below then produces 'no-handler-seen.
(define ht
  (make-hashtable
   (lambda (k) (raise-continuable 'need-hash))
   eq?))

(define handler-result
  (guard (e (#t 'no-handler-seen))
    (with-exception-handler
      (lambda (c) 42)
      (lambda ()
        (hashtable-set! ht 'a 1)
        (hashtable-ref ht 'a #f)))))

(assert-equal? handler-result 1)

;; 2. GUARD: escape procedures still cannot cross the Rust re-entry frame.
;; The fresh barrier id at the callback boundary must keep rejecting jumps.
(assert-equal?
 (call/cc
  (lambda (k)
    (let ((ht2 (make-hashtable (lambda (key) (k 'escaped)) eq?)))
      (guard (e (#t 'blocked))
        (hashtable-set! ht2 'x 1)
        'not-blocked))))
 'blocked)

;; 3. GUARD: a spawned thread must not run the parent's winders. Each parent
;; winder fires exactly once, from the parent.
(define order '())
(dynamic-wind
    (lambda () (set! order (cons 'in order)))
    (lambda ()
      (join (spawn (lambda ()
                     (guard (e (#t 'caught))
                       (error 'child "boom"))))))
    (lambda () (set! order (cons 'out order))))
(assert-equal? order '(out in))
