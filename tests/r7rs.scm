;; r7rs.scm - Compatibility test for the R7RS small implementation

(import (rnrs)
        (test))

;; 6.2 Numbers

(assert-equal? (positive? 1) #t)
(assert-equal? (positive? 0) #f)
(assert-equal? (positive? (- 1)) #f)

(assert-equal? (negative? 1) #f)
(assert-equal? (negative? 0) #f)
(assert-equal? (negative? -1) #t)

(assert-equal? (abs 1) 1)
(assert-equal? (abs 0) 0)
(assert-equal? (abs -1) 1)

(assert-equal? (min 2 4 1 3) 1)
(assert-equal? (min 2 4 -1 3) -1)
(assert-equal? (min 2 4 3) 2)

(assert-equal? (max 2 4 1 3) 4)
(assert-equal? (max 2 4 3)   4)
(assert-equal? (max 3 2 1)   3)

;; 6.6 Characters
(assert-equal? (char->integer #\a) 97)
(assert-equal? (integer->char 97) #\a)

;; 6.8 Vectors

(assert-equal? (make-vector 10 'a) #(a a a a a a a a a a))

(assert-equal? (vector 'a 'b 'c) #(a b c))

(assert-equal? (vector-ref '#(1 1 2 3 5 8 13 21) 5) 8)

(assert-equal?
  (let ((vec (vector 0 '(2 2 2 2) "Anna")))
    (vector-set! vec 1 '("Sue" "Sue"))
    vec)
  #(0 ("Sue" "Sue") "Anna"))

(assert-equal?
  (vector->list '#(dah dah didah))
  '(dah dah didah))
(assert-equal?
  (vector->list '#(dah dah didah) 1 2)
  '(dah))
(assert-equal?
  (list->vector '(dididit dah))
  #(dididit dah))

 (assert-equal?
   (string->vector "ABC")
   #(#\A #\B #\C))
 (assert-equal? (vector->string #(#\A #\B #\C)) "ABC")

(let* ((a #(1 8 2 8))
       (b (vector-copy a))
       (c (vector-copy b 1 3)))

  (vector-set! b 0 3)

  (assert-equal? b #(3 8 2 8))
  (assert-equal? c #(8 2)))

(let ((a (vector 1 2 3 4 5))
      (b (vector 10 20 30 40 50)))

  (vector-copy! b 1 a 0 2)
  (assert-equal? b #(10 1 2 40 50)))

(assert-equal? (vector-append #(a b c) #(d e f))
           #(a b c d e f))

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v 'smash 2 4)
  (assert-equal? v #(1 2 smash smash 5)))
