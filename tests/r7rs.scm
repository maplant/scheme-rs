;; r7rs.scm - Compatibility test for the R7RS small implementation

;; 6.8 Vectors

; not part of r7rs examples
(assert-eq (make-vector 10 'a) #(a a a a a a a a a a))

(assert-eq (vector 'a 'b 'c) #(a b c))

(assert-eq (vector-ref '#(1 1 2 3 5 8 13 21) 5) 8)

(assert-eq
  (let ((vec (vector 0 '(2 2 2 2) "Anna")))
    (vector-set! vec 1 '("Sue" "Sue"))
    vec)
  #(0 ("Sue" "Sue") "Anna"))

(assert-eq
  (vector->list '#(dah dah didah))
  '(dah dah didah))
(assert-eq
  (vector->list '#(dah dah didah) 1 2)
  '(dah))
(assert-eq
  (list->vector '(dididit dah))
  #(dididit dah))

 (assert-eq
   (string->vector "ABC")
   #(#\A #\B #\C))
 (assert-eq (vector->string #(#\A #\B #\C)) "ABC")

(define a #(1 8 2 8))
(define b (vector-copy a))
(define c (vector-copy b 1 3))
(vector-set! b 0 3)

(assert-eq b #(3 8 2 8))
(assert-eq c #(8 2))

(set! a (vector 1 2 3 4 5))
(set! b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
(assert-eq b #(10 1 2 40 50))

(assert-eq (vector-append #(a b c) #(d e f))
           #(a b c d e f))

(define v (vector 1 2 3 4 5))
(vector-fill! v 'smash 2 4)
(assert-eq v #(1 2 smash smash 5))
