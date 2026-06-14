(import (rnrs) (test) (lang))

;; define* with keyword args
(define* (greet name #:key (greeting "Hello") (punctuation "!"))
  (string-append greeting " " name punctuation))

(assert-equal? (greet "World") "Hello World!")
(assert-equal? (greet "World" #:greeting "Hi") "Hi World!")
(assert-equal? (greet "World" #:punctuation "?") "Hello World?")
(assert-equal? (greet "World" #:greeting "Hey" #:punctuation ".") "Hey World.")

;; lambda* with keyword args
(define f (lambda* (x #:key (y 10)) (+ x y)))
(assert-equal? (f 1) 11)
(assert-equal? (f 1 #:y 20) 21)

;; keyword with no default
(define* (add a #:key b) (+ a (or b 0)))
(assert-equal? (add 5) 5)
(assert-equal? (add 5 #:b 3) 8)

;; #:rest collects non-keyword args
(define* (variadic #:key (sep " ") #:rest items)
  items)
(assert-equal? (variadic 1 2 3) '(1 2 3))
(assert-equal? (variadic 1 2 #:sep "-" 3) '(1 2 3))

;; plain define* with no keywords — behaves like define
(define* (plain x y) (+ x y))
(assert-equal? (plain 3 4) 7)
