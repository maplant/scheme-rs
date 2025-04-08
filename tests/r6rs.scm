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

;; mark

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

;; ?? boolean=?

(assert-eq (boolean=? #t #t #t) #t)
(assert-eq (boolean=? #f #f #f) #t)
(assert-eq (boolean=? #t #f #f) #f)
(assert-eq (boolean=? #f #t #t) #f)
(assert-eq (boolean=? 1 2) #f)
(assert-eq (boolean=? #t 2) #f)
(assert-eq (boolean=? #t) #t)
(assert-eq (boolean=? #f) #t)

;;  6.4. list procedures

(assert-eq (make-list 2) '(#f #f))
(assert-eq (make-list 5) '(#f #f #f #f #f))

(define xs '(1 2 3 4 5))

(assert-eq (eq? (list-copy xs) xs) #f)
(assert-eq (eq? xs xs) #t)

(assert-eq (list-ref xs 2) 3)
(assert-eq (list-ref xs 3) 4)

(assert-eq (list-tail xs 3) '(4 5))

(define alist '((a . 1) (b . 2)))

(assert-eq (assoc 'a alist) '(a . 1))
(assert-eq (assoc 'b alist) '(b . 2))
(assert-eq (assoc 'c alist) #f)

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

;; 11.5. Equivalence predicates

;; Right now, constants have a new allocation per each instance. This is obviously
;; wrong, but a much deeper problem than one with the implementation of eq?

(assert-eq (eq? (list 'a) (list 'a))
           #f)

(assert-eq (let ((x 1))
             (eq? x x))
           #t)

(assert-eq (let loopv ((n 1))
             (if (> n 10)
                 '()
                 (cons n (loopv (+ n 1)))))
           '(1 2 3 4 5 6 7 8 9 10))
           
(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax
           ([break (datum->syntax #'k 'break)])
         #'(call-with-current-continuation
            (lambda (break)
              (let f () e ... (f)))))])))

(assert-eq (let ((n 3) (ls '()))
             (loop
              (if (= n 0) (break ls))
              (set! ls (cons 'a ls))
              (set! n (- n 1))))
           '(a a a))

;; 11.7 Arithmetic

(assert-eq (/ 5) 1/5)
(assert-eq (/ 5 10) 1/2)

;; 11.15 Control features

(assert-eq (let ((path '())
                 (c #f))
             (let ((add (lambda (s)
                          (set! path (cons s path)))))
               (dynamic-wind
                   (lambda () (add 'connect))
                   (lambda ()
                     (add (call-with-current-continuation
                           (lambda (c0)
                             (set! c c0)
                             'talk1))))
                   (lambda () (add 'disconnect)))
               (if (< (length path) 4)
                   (c 'talk2)
                   (reverse path))))
           '(connect talk1 disconnect connect talk2 disconnect))

(assert-eq (let ((n 0))
             (call-with-current-continuation
              (lambda (k)
                (dynamic-wind
                    (lambda ()
                      (set! n (+ n 1))
                      (k))
                    (lambda ()
                      (set! n (+ n 2)))
                    (lambda ()
                      (set! n (+ n 4))))))
             n)
           1)

;; Extra stuff
(assert-eq (let ([x 1])
             (syntax-case #'() ()
               ([] x)))
           1)
