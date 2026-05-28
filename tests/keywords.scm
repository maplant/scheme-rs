(import (rnrs) (srfi :88) (test))

;; keyword? predicate
(assert-equal? (keyword? #:foo) #t)
(assert-equal? (keyword? 'foo) #f)
(assert-equal? (keyword? "foo") #f)
(assert-equal? (keyword? 42) #f)

;; keyword->string
(assert-equal? (keyword->string #:hello) "hello")
(assert-equal? (keyword->string #:foo-bar) "foo-bar")

;; string->keyword
(assert-equal? (keyword? (string->keyword "test")) #t)
(assert-equal? (keyword->string (string->keyword "test")) "test")

;; Round-trip
(assert-equal? (string->keyword (keyword->string #:abc)) #:abc)

;; Keywords are self-evaluating (no quote needed)
(assert-equal? (let ((x #:key)) x) #:key)

;; Keywords with the same name are eqv?
(assert-equal? (eqv? #:foo #:foo) #t)
(assert-equal? (eqv? #:foo (string->keyword "foo")) #t)
(assert-equal? (eqv? #:foo #:bar) #f)

;; Keywords are disjoint from symbols
(assert-equal? (symbol? #:foo) #f)
