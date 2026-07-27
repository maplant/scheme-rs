(import (rnrs) (rnrs parameters) (test))

;; parameter? recognizes an actual parameter
(define p (make-parameter 42))
(assert-equal? (parameter? p) #t)

;; parameter? rejects a closure that merely closes over a parameter
(assert-equal? (parameter? (lambda () (p))) #f)

;; procedure? answers #t for a parameter
(assert-equal? (procedure? p) #t)

;; apply works with parameters as operators
(assert-equal? (apply p '()) 42)
(p 10)
(assert-equal? (apply p '()) 10)

;; a converter parameter works through direct application
(define q (make-parameter 5 (lambda (x) (* x 2))))
(assert-equal? (q) 10)
(q 3)
(assert-equal? (q) 6)

;; too many arguments raises an error that names the parameter type
(assert-equal?
 (guard (e (#t (condition-message e)))
   (p 1 2))
 "parameter accepts zero or one arguments")
