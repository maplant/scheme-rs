(import (rnrs) (srfi :230) (runtime (1)) (test))

;; Basic ref/set
(let ((box (make-atomic-box 42)))
  (assert-equal? (atomic-box-ref box) 42)
  (atomic-box-set! box 99)
  (assert-equal? (atomic-box-ref box) 99))

;; Swap
(let ((box (make-atomic-box 'a)))
  (let ((old (atomic-box-swap! box 'b)))
    (assert-equal? old 'a)
    (assert-equal? (atomic-box-ref box) 'b)))

;; CAS success
(let ((box (make-atomic-box 'W)))
  (let ((prev (atomic-box-compare-and-swap! box 'W 'S)))
    (assert-equal? prev 'W)
    (assert-equal? (atomic-box-ref box) 'S)))

;; CAS failure
(let ((box (make-atomic-box 'S)))
  (let ((prev (atomic-box-compare-and-swap! box 'W 'C)))
    (assert-equal? prev 'S)
    (assert-equal? (atomic-box-ref box) 'S)))

;; CAS with booleans
(let ((box (make-atomic-box #f)))
  (let ((prev (atomic-box-compare-and-swap! box #f #t)))
    (assert-equal? prev #f)
    (assert-equal? (atomic-box-ref box) #t)))

;; atomic-box? predicate
(assert-equal? (atomic-box? (make-atomic-box 0)) #t)
(assert-equal? (atomic-box? 42) #f)
(assert-equal? (atomic-box? "hello") #f)

;; --- GC-managed values ---

;; Store a pair
(let ((box (make-atomic-box (cons 1 2))))
  (assert-equal? (atomic-box-ref box) (cons 1 2))
  (atomic-box-set! box (list 1 2 3))
  (assert-equal? (atomic-box-ref box) (list 1 2 3)))

;; Store a vector
(let ((box (make-atomic-box (vector 'a 'b 'c))))
  (assert-equal? (atomic-box-ref box) (vector 'a 'b 'c)))

;; Store a string
(let ((box (make-atomic-box "hello")))
  (assert-equal? (atomic-box-ref box) "hello")
  (atomic-box-set! box "world")
  (assert-equal? (atomic-box-ref box) "world"))

;; --- Swap with GC-managed values ---

(let ((box (make-atomic-box (cons 'x 'y))))
  (let ((old (atomic-box-swap! box (cons 'a 'b))))
    (assert-equal? old (cons 'x 'y))
    (assert-equal? (atomic-box-ref box) (cons 'a 'b))))

;; --- CAS eq? semantics with heap-allocated values ---

;; CAS uses eq? (pointer identity), not equal?
;; Two independently constructed lists are equal? but not eq?,
;; so CAS should fail even though the values are structurally equal.
(let* ((original (list 1 2 3))
       (box (make-atomic-box original))
       (expected (list 1 2 3))
       (prev (atomic-box-compare-and-swap! box expected 'replaced)))
  (assert-equal? (atomic-box-ref box) original)
  (assert-equal? prev original))

;; CAS succeeds when using the exact same object (eq?)
(let* ((original (list 1 2 3))
       (box (make-atomic-box original))
       (prev (atomic-box-compare-and-swap! box original 'replaced)))
  (assert-equal? prev original)
  (assert-equal? (atomic-box-ref box) 'replaced))

;; --- Error case ---

;; atomic-box-ref on a non-atomic-box should raise an error
(assert-equal?
  (call-with-current-continuation
    (lambda (escape)
      (with-exception-handler
        (lambda (exn) (escape 'caught))
        (lambda () (atomic-box-ref 42)))))
  'caught)

;; --- GC pressure ---

;; Store GC-managed values, force collection, verify they survive
(let ((box (make-atomic-box (list 1 2 3 4 5))))
  (collect-garbage)
  (assert-equal? (atomic-box-ref box) (list 1 2 3 4 5))
  (atomic-box-set! box (vector 'a 'b 'c))
  (collect-garbage)
  (assert-equal? (atomic-box-ref box) (vector 'a 'b 'c)))

;; Swap under GC pressure
(let ((box (make-atomic-box (cons 'x 'y))))
  (collect-garbage)
  (let ((old (atomic-box-swap! box (list 10 20 30))))
    (collect-garbage)
    (assert-equal? old (cons 'x 'y))
    (assert-equal? (atomic-box-ref box) (list 10 20 30))))

;; Allocate many values, store in box, force GC, verify
(let ((box (make-atomic-box #f)))
  (let loop ((i 0))
    (when (< i 100)
      (atomic-box-set! box (list i (+ i 1) (+ i 2)))
      (loop (+ i 1))))
  (collect-garbage)
  (assert-equal? (atomic-box-ref box) (list 99 100 101)))
