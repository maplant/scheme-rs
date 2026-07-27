(import (rnrs) (rnrs parameters) (test))

;; A parameterize binding is visible inside a hashtable hash callback —
;; impossible under per-barrier storage; the point of the redesign.
(define p (make-parameter 1))
(define seen '())
(define ht
  (make-hashtable
   (lambda (k) (set! seen (cons (p) seen)) 7)
   eq?))
(parameterize ((p 99))
  (hashtable-set! ht 'a 1))
(assert-equal? (car seen) 99)

;; A continuation captured inside a parameterize re-enters its binding.
(define q (make-parameter 0))
(define k* #f)
(define trail '())
(parameterize ((q 5))
  (call/cc (lambda (k) (set! k* k)))
  (set! trail (cons (q) trail)))
(when (< (length trail) 2) (k* #f))
(assert-equal? trail '(5 5))

;; Non-idempotent converter drift regression: entering and leaving
;; parameterize must not change the outer value (Chez drifts here;
;; rebinding applies the converter at entry only).
(define ni (make-parameter 1 (lambda (x) (+ x 1))))
(assert-equal? (ni) 2)
(parameterize ((ni 10)) #f)
(parameterize ((ni 10)) #f)
(parameterize ((ni 10)) #f)
(assert-equal? (ni) 2)
