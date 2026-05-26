(import (rnrs) (srfi :88) (optargs) (test))

;; Plain lambda* with no special args
(assert-equal? ((lambda* (a b) (+ a b)) 3 4) 7)

;; Basic optional
(assert-equal? ((lambda* (a #:optional (b 10)) (+ a b)) 1) 11)
(assert-equal? ((lambda* (a #:optional (b 10)) (+ a b)) 1 2) 3)

;; Optional with bare variable (defaults to #f)
(assert-equal? ((lambda* (a #:optional b) (if b (+ a b) a)) 5) 5)
(assert-equal? ((lambda* (a #:optional b) (if b (+ a b) a)) 5 3) 8)

;; Multiple optionals
(assert-equal? ((lambda* (a #:optional (b 1) (c 2)) (list a b c)) 10) '(10 1 2))
(assert-equal? ((lambda* (a #:optional (b 1) (c 2)) (list a b c)) 10 20) '(10 20 2))
(assert-equal? ((lambda* (a #:optional (b 1) (c 2)) (list a b c)) 10 20 30) '(10 20 30))

;; Basic keyword
(assert-equal? ((lambda* (a #:key (x 0)) (+ a x)) 5) 5)
(assert-equal? ((lambda* (a #:key (x 0)) (+ a x)) 5 #:x 10) 15)

;; Keyword with bare variable
(assert-equal? ((lambda* (a #:key y) (list a y)) 1) '(1 #f))
(assert-equal? ((lambda* (a #:key y) (list a y)) 1 #:y 42) '(1 42))

;; Multiple keywords
(assert-equal? ((lambda* (#:key (x 1) (y 2)) (list x y))) '(1 2))
(assert-equal? ((lambda* (#:key (x 1) (y 2)) (list x y)) #:y 20) '(1 20))
(assert-equal? ((lambda* (#:key (x 1) (y 2)) (list x y)) #:x 10 #:y 20) '(10 20))
(assert-equal? ((lambda* (#:key (x 1) (y 2)) (list x y)) #:y 20 #:x 10) '(10 20))

;; Both optional and keyword
(assert-equal? ((lambda* (a #:optional (b 1) #:key (c 2)) (list a b c)) 10)
               '(10 1 2))
(assert-equal? ((lambda* (a #:optional (b 1) #:key (c 2)) (list a b c)) 10 20 #:c 30)
               '(10 20 30))

;; Rest args
(assert-equal? ((lambda* (a #:rest r) (cons a r)) 1 2 3) '(1 2 3))
(assert-equal? ((lambda* (a #:optional (b 0) #:rest r) (list a b r)) 1 2 3 4)
               '(1 2 (3 4)))

;; define* form
(define* (f x #:key (y 0)) (+ x y))
(assert-equal? (f 1) 1)
(assert-equal? (f 1 #:y 10) 11)

;; define* with optional
(define* (g x #:optional (y 5)) (+ x y))
(assert-equal? (g 1) 6)
(assert-equal? (g 1 2) 3)

;; No required args, only keyword
(define* (h #:key (a 0) (b 0)) (+ a b))
(assert-equal? (h) 0)
(assert-equal? (h #:a 3) 3)
(assert-equal? (h #:a 3 #:b 4) 7)
