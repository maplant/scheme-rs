(import (rnrs) (rnrs parameters) (prompts) (test))

;; Basic parameter creation and access
(define p (make-parameter 42))
(assert-equal? (p) 42)
(assert-equal? (parameter? p) #t)
(assert-equal? (parameter? 42) #f)
(assert-equal? (parameter? car) #f)

;; Direct set returns previous value
(assert-equal? (p 99) 42)
(assert-equal? (p) 99)
(p 42) ;; reset

;; Converter applied to initial value
(define q (make-parameter 5 (lambda (x) (* x 2))))
(assert-equal? (q) 10)

;; Converter applied on direct set
(q 3)
(assert-equal? (q) 6)

;; parameterize scoping (no converter on p)
(assert-equal? (parameterize ((p 100)) (p)) 100)
(assert-equal? (p) 42)

;; parameterize applies converter
(assert-equal? (parameterize ((q 7)) (q)) 14)
;; restore applies converter to saved value: (* 6 2) = 12
(assert-equal? (q) 12)

;; Nested parameterize (no converter)
(assert-equal?
 (parameterize ((p 100))
   (parameterize ((p 200))
     (p)))
 200)
(assert-equal? (p) 42)

;; Multiple parameters in one parameterize
(assert-equal?
 (parameterize ((p 1) (q 2))
   (cons (p) (q)))
 '(1 . 4))
;; q restore: converter applied to 12 -> 24
(assert-equal? (q) 24)

;; Mutation inside parameterize doesn't leak (no converter)
(parameterize ((p 50))
  (p 60))
(assert-equal? (p) 42)

;; parameterize + call/cc: mutations preserved via swap
(define cc-param (make-parameter 'outside))
(define saved-k #f)
(define call-count 0)

(parameterize ((cc-param 'inside))
  (call-with-current-continuation
    (lambda (k) (set! saved-k k)))
  (assert-equal? (cc-param) 'inside)
  (set! call-count (+ call-count 1)))

(assert-equal? (cc-param) 'outside)

(if (< call-count 2)
    (saved-k))
(assert-equal? (cc-param) 'outside)

;; Mutation + re-entry: mutation preserved via swap
(define mut-param (make-parameter 'default))
(define mut-k #f)
(define mut-count 0)

(parameterize ((mut-param 'bound))
  (call-with-current-continuation
    (lambda (k) (set! mut-k k)))
  (if (= mut-count 0)
      (mut-param 'mutated))
  (assert-equal? (mut-param) 'mutated)
  (set! mut-count (+ mut-count 1)))

(if (< mut-count 2)
    (begin
      (assert-equal? (mut-param) 'default)
      (mut-k)))
(assert-equal? (mut-param) 'default)

;; parameterize + dynamic-wind
(define dw-param (make-parameter 'default))
(define dw-log '())

(parameterize ((dw-param 'bound))
  (dynamic-wind
    (lambda () (set! dw-log (cons (cons 'in (dw-param)) dw-log)))
    (lambda () (set! dw-log (cons (cons 'body (dw-param)) dw-log)))
    (lambda () (set! dw-log (cons (cons 'out (dw-param)) dw-log)))))

(assert-equal? (reverse dw-log) '((in . bound) (body . bound) (out . bound)))

;; parameterize + abort-to-prompt
(define prompt-param (make-parameter 'outside))

(assert-equal?
 (call-with-prompt 'test-tag
   (lambda ()
     (parameterize ((prompt-param 'inside))
       (abort-to-prompt 'test-tag 'result)))
   (lambda (k val)
     (cons val (prompt-param))))
 '(result . outside))

;; Empty parameterize
(assert-equal? (parameterize () 42) 42)

;; Non-idempotent converter: converter applied on restore (matching Chez)
(define ni-param (make-parameter 1 (lambda (x) (+ x 1))))
(assert-equal? (ni-param) 2)
(assert-equal? (parameterize ((ni-param 4)) (ni-param)) 5)
(assert-equal? (ni-param) 3)
