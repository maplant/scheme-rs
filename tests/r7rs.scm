;; r6rs.scm - Compatibility test for the R6RS implementation

;; 6.8 Vectors

(assert-eq (vector 'a 'b 'c) #(a b c))

(assert-eq (vector-ref '#(1 1 2 3 5 8 13 21) 5) 8)

(assert-eq
  (let ((vec (vector 0 '(2 2 2 2) "Anna")))
    (vector-set! vec 1 '("Sue" "Sue"))
    vec)
  #(0 ("Sue" "Sue") "Anna"))

; not part of examples
(assert-eq (vector? #(0 (2 2 2 2) "Anna")) #t)

