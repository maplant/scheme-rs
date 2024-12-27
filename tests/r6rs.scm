;; r6rs.scm - Compatibility test for the R6RS implementation
;;
;; As of right now, this test simply takes all of the examples
;; given in the r6rs spec and runs them, asserting the values to
;; be the ones given in the spec.

;; 1.2. Expressions

;; The following are omitted because they don't really show anything:
;; (assert-eq #t #t)
;; (assert-eq 23 23)

(assert-eq (+ 23 42) 65)
(assert-eq (+ 14 (* 23 42)) 980)

;; 1.3. Variables and binding

(assert-eq
 (let ((x 23)
       (y 42))
   (+ x y))
 65)

;; 1.4. Definitions

(define x 23)
(define y 42)
(assert-eq (+ x y) 65)

(define x 23)
(define y 42)
(assert-eq (let ((y 43))
             (+ x y))
           66)

(assert-eq (let ((y 43))
             (let ((y 44))
               (+ x y)))
           67)

;; 1.6 Procedures

(define (f x)
  (+ x 42))

(assert-eq (f 23) 65)

(define (f x)
  (+ x 42))

(define (g p x)
  (p x))

(assert-eq (g f 23) 65)

(define (h op x y)
  (op x y))

(assert-eq (h + 23 42) 65)
(assert-eq (h * 23 42) 966)

(assert-eq ((lambda (x) (+ x 42)) 23) 65)

;; 1.8 Assignments

(assert-eq (let ((x 23))
             (set! x 42)
             x)
           42)

;; 1.11 Continuations

(assert-eq (+ 1 (call-with-current-continuation
                 (lambda (escape)
                   (+ 2 (escape 3)))))
           4)

;; 11.2.2. Syntax definitions

(assert-eq (let ()
             (define even?
               (lambda (x)
                 (or (= x 0) (odd? (- x 1)))))
             (define-syntax odd?
               (syntax-rules ()
                 ((odd? x) (not (even? x)))))
             (even? 10))
           #t)

(assert-eq (let ()
             (define-syntax bind-to-zero
               (syntax-rules ()
                 ((bind-to-zero id) (define id 0))))
             (bind-to-zero x)
             x)
           0)

;; 11.3 Bodies

(assert-eq (let ((x 5))
             (define foo (lambda (y) (bar x y)))
             (define bar (lambda (a b) (+ (* a b) a)))
             (foo (+ x 3)))
           45)


;; 11.4.2. Procedures

;; (skipping a bunch of these because this stuff works)

(assert-eq ((lambda (x)
              (define (p y)
                (+ y 1))
              (+ (p x) x))
            5)
           11)

;; 11.4.3 Conditionals

(assert-eq (if (> 3 2) 'yes 'no) 'yes)
(assert-eq (if (> 2 3) 'yes 'no) 'no)
(assert-eq (if (> 3 2)
               (- 3 2)
               (+ 3 2))
           1)

;; 11.4.5 Derived conditionals

(assert-eq (cond ((> 3 2) 'greater)
                 ((< 3 2) 'less))
           'greater)
(assert-eq (cond ((> 3 3) 'greater)
                 ((< 3 3) 'less)
                 (else 'equal))
           'equal)
(assert-eq (cond ('(1 2 3) => cadr)
                 (else #f))
           2)

(assert-eq (case (* 2 3)
             ((2 3 5 7) 'prime)
             ((1 4 6 8 9) 'composite))
           'composite)
(assert-eq (case (car '(c d))
             ((a e i o u) 'vowel)
             ((w y) 'semivowel)
             (else 'consonant))
           'consonant)

;; 11.4.6. Binding constructs

(assert-eq (let ((x 2) (y 3))
             (let* ((x 7)
                    (z (+ x y)))
               (* z x)))
           70)

(assert-eq (letrec ((even?
                     (lambda (n)
                       (if (zero? n)
                           #t
                           (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (zero? n)
                           #f
                           (even? (- n 1))))))
             (even? 88))
           #t)

(assert-eq (letrec* ((p
                      (lambda (x)
                        (+ 1 (q (- x 1)))))
                     (q
                      (lambda (y)
                        (if (zero? y)
                            0
                            (+ 1 (p (- y 1))))))
                     (x (p 5))
                     (y x))
             y)
           5)

(assert-eq (let-values (((a b) (values 1 2))
                        ((c d) (values 3 4)))
             (list a b c d))
           '(1 2 3 4))

(assert-eq (let-values (((a b . c) (values 1 2 3 4)))
             (list a b c))
           '(1 2 (3 4)))

(assert-eq (let ((a 'a) (b 'b) (x 'x) (y 'y))
             (let-values (((a b) (values x y))
                          ((x y) (values a b)))
               (list a b x y)))
           '(x y a b))

;;(assert-eq (let ((a 'a) (b 'b) (x 'x) (y 'y))
;;             (let*-values (((a b) (values x y))
;;                           ((x y) (values a b)))
;;               (list a b x y)))
;;           (x y x y))
