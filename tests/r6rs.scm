;; r6rs.scm - Compatibility test for the R6RS implementation
;;
;; As of right now, this test simply takes all of the examples
;; given in the r6rs spec and runs them, asserting the values to
;; be the ones given in the spec.

(import (rnrs)
        (test))

;; 1.2. Expressions

;; The following are omitted because they don't really show anything:
;; (assert-eqv? #t #t)
;; (assert-eqv? 23 23)

(assert-eqv? (+ 23 42) 65)
(assert-eqv? (+ 14 (* 23 42)) 980)

;; 1.3. Variables and binding

(assert-eqv?
 (let ((x 23)
       (y 42))
   (+ x y))
 65)

;; 1.4. Definitions

(define x 23)
(define y 42)
(assert-eqv? (+ x y) 65)

(define x 23)
(define y 42)

(assert-eqv? (let ((y 43))
               (+ x y))
             66)

(assert-eqv? (let ((y 43))
               (let ((y 44))
                 (+ x y)))
             67)

;; 1.6 Procedures

(define (f x)
  (+ x 42))

(assert-eqv? (f 23) 65)

(define (f x)
  (+ x 42))

(define (g p x)
  (p x))

(assert-eqv? (g f 23) 65)

(define (h op x y)
  (op x y))

(assert-eqv? (h + 23 42) 65)
(assert-eqv? (h * 23 42) 966)

;; mark

(assert-eqv? ((lambda (x) (+ x 42)) 23) 65)

;; 1.8 Assignments

(assert-eqv? (let ((x 23))
               (set! x 42)
               x)
             42)

;; 1.11 Continuations
(assert-eqv? (+ 1 (call-with-current-continuation
                   (lambda (escape)
                     (+ 2 (escape 3)))))
             4)

;; ?? boolean=?

(assert-eqv? (boolean=? #t #t #t) #t)
(assert-eqv? (boolean=? #f #f #f) #t)
(assert-eqv? (boolean=? #t #f #f) #f)
(assert-eqv? (boolean=? #f #t #t) #f)
(assert-eqv? (boolean=? 1 2) #f)
(assert-eqv? (boolean=? #t 2) #f)
(assert-eqv? (boolean=? #t) #t)
(assert-eqv? (boolean=? #f) #t)

;;  6.4. list procedures

(assert-eqv? (make-list 2) '(#f #f))
(assert-eqv? (make-list 5) '(#f #f #f #f #f))

(define xs '(1 2 3 4 5))

(assert-eqv? (eq? (list-copy xs) xs) #f)
(assert-eqv? (eq? xs xs) #t)

(assert-eqv? (list-ref xs 2) 3)
(assert-eqv? (list-ref xs 3) 4)

(assert-eqv? (list-tail xs 3) '(4 5))

(define alist '((a . 1) (b . 2)))

(assert-eqv? (assoc 'a alist) '(a . 1))
(assert-eqv? (assoc 'b alist) '(b . 2))
(assert-eqv? (assoc 'c alist) #f)

;; 11.2.2. Syntax definitions

(assert-eqv? (let ()
               (define even?
                 (lambda (x)
                   (or (= x 0) (odd? (- x 1)))))
               (define-syntax odd?
                 (syntax-rules ()
                   ((odd? x) (not (even? x)))))
               (even? 10))
             #t)

(assert-eqv? (let ()
               (define-syntax bind-to-zero
                 (syntax-rules ()
                   ((bind-to-zero id) (define id 0))))
               (bind-to-zero x)
               x)
             0)

;; 11.3 Bodies

(assert-eqv? (let ((x 5))
               (define foo (lambda (y) (bar x y)))
               (define bar (lambda (a b) (+ (* a b) a)))
               (foo (+ x 3)))
             45)


;; 11.4.2. Procedures

;; (skipping a bunch of these because this stuff works)

(assert-eqv? ((lambda (x)
                (define (p y)
                  (+ y 1))
                (+ (p x) x))
              5)
             11)

;; 11.4.3 Conditionals

(assert-eqv? (if (> 3 2) 'yes 'no) 'yes)
(assert-eqv? (if (> 2 3) 'yes 'no) 'no)
(assert-eqv? (if (> 3 2)
                 (- 3 2)
                 (+ 3 2))
             1)

;; 11.4.5 Derived conditionals

(assert-eqv? (cond ((> 3 2) 'greater)
                   ((< 3 2) 'less))
             'greater)
(assert-eqv? (cond ((> 3 3) 'greater)
                   ((< 3 3) 'less)
                   (else 'equal))
             'equal)
(assert-eqv? (cond ('(1 2 3) => cadr)
                   (else #f))
             2)

(assert-eqv? (case (* 2 3)
               ((2 3 5 7) 'prime)
               ((1 4 6 8 9) 'composite))
             'composite)
(assert-eqv? (case (car '(c d))
               ((a e i o u) 'vowel)
               ((w y) 'semivowel)
               (else 'consonant))
             'consonant)


;; 11.4.6. Binding constructs

(assert-eqv? (let ((x 2) (y 3))
               (let* ((x 7)
                      (z (+ x y)))
                 (* z x)))
             70)

(assert-eqv? (letrec ((even?
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

(assert-eqv? (letrec* ((p
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

(assert-eqv? (let-values (((a b) (values 1 2))
                          ((c d) (values 3 4)))
               (list a b c d))
             '(1 2 3 4))

(assert-eqv? (let-values (((a b . c) (values 1 2 3 4)))
               (list a b c))
             '(1 2 (3 4)))

(assert-eqv? (let ((a 'a) (b 'b) (x 'x) (y 'y))
               (let-values (((a b) (values x y))
                            ((x y) (values a b)))
                 (list a b x y)))
             '(x y a b))

(assert-eqv? (let ((a 'a) (b 'b) (x 'x) (y 'y))
               (let*-values (((a b) (values x y))
                             ((x y) (values a b)))
                 (list a b x y)))
             '(x y x y))

;; 11.5. Equivalence predicates

;; Right now, constants have a new allocation per each instance. This is obviously
;; wrong, but a much deeper problem than one with the implementation of eq?

(assert-eqv? (eq? (list 'a) (list 'a))
             #f)

(assert-eqv? (let ((x 1))
               (eq? x x))
             #t)

(assert-eqv? (let loopv ((n 1))
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

(assert-eqv? (let ((n 3) (ls '()))
               (loop
                (if (= n 0) (break ls))
                (set! ls (cons 'a ls))
                (set! n (- n 1))))
             '(a a a))

;; 11.7 Arithmetic

(assert-eqv? (/ 5) 1/5)
(assert-eqv? (/ 5 10) 1/2)

;; 11.15 Control features

(assert-eqv? (let ((path '())
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

(assert-eqv? (let ((n 0))
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

;; 11.18 Binding constructs for syntactic-keywords
(assert-eqv? (let-syntax ((when (syntax-rules ()
                                  ((when test stmt1 stmt2 ...)
                                   (if test
                                       (begin stmt1
                                              stmt2 ...))))))
               (let ((if #t))
                 (when if (set! if 'now))
                 if))
             'now)

(assert-eqv? (let ()
               (let-syntax
                   ((def (syntax-rules ()
                           ((def stuff ...) (define stuff ...)))))
                 (def foo 42))
               foo)
             42)

(assert-eqv? (let ((x 'outer))
               (let-syntax ((m (syntax-rules () ((m) x))))
                 (let ((x 'inner))
                   (m))))
             'outer)

;; TODO: Fix parser for this, I guess.
;; (assert-eqv? (let ()
;;              (let-syntax ())
;;              5)
;;            5)

(assert-eqv? (letrec-syntax
                 ((my-or (syntax-rules ()
                           ((my-or) #f)
                           ((my-or e) e)
                           ((my-or e1 e2 ...)
                            (let ((temp e1))
                              (if temp
                                  temp
                                  (my-or e2 ...)))))))
               (let ((x #f)
                     (y 7)
                     (temp 8)
                     ;; TODO: In the R6RS docs these are let and if and everything
                     ;; works fine. Got to fix the parser
                     (let odd?)
                     (if even?))
                 (my-or x
                        (let temp)
                        (if y)
                        y)))
             7)

(assert-eqv? (let ((f (lambda (x) (+ x 1))))
               (let-syntax ((f (syntax-rules ()
                                 ((f x) x)))
                            (g (syntax-rules ()
                                 ((g x) (f x)))))
                 (list (f 1) (g 1))))
             '(1 2))

(assert-eqv? (let ((f (lambda (x) (+ x 1))))
               (letrec-syntax ((f (syntax-rules ()
                                    ((f x) x)))
                               (g (syntax-rules ()
                                    ((g x) (f x)))))
                 (list (f 1) (g 1))))
             '(1 1))

;; Extra stuff:

(assert-eqv? (let ([x 1])
               (syntax-case #'() ()
                 ([] x)))
             1)

;; Guile hygiene example:

(define-syntax defconst
  (lambda (x)
    (syntax-case x ()
      [(_ name val)
       (syntax (begin
                 (define t val)
                 (define-syntax name
                   (lambda (x)
                     (syntax-case x ()
                       ([_] #'t))))))])))

;; foo is already bound to a macro in this scope
(defconst newfoo 42)
(defconst newbar 70)

(assert-eqv? (newfoo) 42)
(assert-eqv? (newbar) 70)

;; Realized this was an issue when doing escape analysis:
(define (test a) (set! a '()))

;; r6rs-lib:

;; 6.3. Procedural layer

(define :point
  (make-record-type-descriptor
   'point #f
   #f #f #f
   '#((mutable x) (mutable y))))
(define :point-cd
  (make-record-constructor-descriptor :point #f #f))

(define make-point (record-constructor :point-cd))
(define point? (record-predicate :point))
(define point-x (record-accessor :point 0))
(define point-y (record-accessor :point 1))
(define point-x-set! (record-mutator :point 0))
(define point-y-set! (record-mutator :point 1))

(define p1 (make-point 1 2))

(assert-eqv? (point? p1) #t)
(assert-eqv? (point-x p1) 1)
(assert-eqv? (point-y p1) 2)
(point-x-set! p1 5)
(assert-eqv? (point-x p1) 5)

(define :point2
  (make-record-type-descriptor
   'point2 :point
   #f #f #f '#((mutable x) (mutable y))))
(define make-point2
  (record-constructor
   (make-record-constructor-descriptor :point2
                                       #f #f)))
(define point2? (record-predicate :point2))
(define point2-xx (record-accessor :point2 0))
(define point2-yy (record-accessor :point2 1))

(define p2 (make-point2 1 2 3 4))
(assert-eqv? (point? p2) #t)
(assert-eqv? (point-x p2) 1)
(assert-eqv? (point-y p2) 2)
(assert-eqv? (point2-xx p2) 3)
(assert-eqv? (point2-yy p2) 4)

(define :point-cd/abs
  (make-record-constructor-descriptor
   :point #f
   (lambda (new)
     (lambda (x y)
       (new (abs x) (abs y))))))

(define make-point/abs
  (record-constructor :point-cd/abs))

(assert-eqv? (point-x (make-point/abs -1 -2)) 1)
(assert-eqv? (point-y (make-point/abs -1 -2)) 2)

;; Test from make the define-record-type macro:
(define (get-clause id ls)
  (syntax-case ls ()
    [() #f]
    [((x . rest) . ls)
     (if (free-identifier=? id #'x)
         #'(x . rest)
         (get-clause id #'ls))]))

(define (test x)
  (syntax-case x ()
    [(_ field-spec* ...) #t]
    [_ #f]))

(assert-eqv? (test (get-clause #'fields #'((fields x y)))) #t)
