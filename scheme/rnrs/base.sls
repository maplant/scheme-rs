(library (rnrs base (6))
  (export cond case let* caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr
          cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr call/cc
          for-each string-for-each vector-for-each vector-map append make-list
          list-copy list-tail list-ref map reverse positive? negative? abs min
          max quasiquote gcd lcm identifier-syntax assert rationalize div0 mod0
          div0-and-mod0 
          (import (rnrs base builtins (6)))
          (import (rnrs syntax-rules (6)))
          (import (rnrs values (6)))
          (import (rnrs letrec (6)))
          (import (except (rnrs base primitives (6)) $undefined)))
  (import (rnrs lists (6))
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs io simple builtins)
          (only (rnrs base primitives (6)) $undefined))

  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (mod a b))))

  (define (lcm a b)
    (* (/ (abs b) (gcd a b)) (abs a)))

  (define (div0-and-mod0 x y)
    (define q (div x y))
    (define r (mod x y))
    (cond ((< r (abs (/ y 2)))
              (values q r))
             ((> y 0)
              (values (+ q 1) (- x (* (+ q 1) y))))
             (else
              (values (- q 1) (- x (* (- q 1) y))))))

  (define (div0 x y)
    (let-values ([(q r) (div0-and-mod0 x y)])
      q))

  (define (mod0 x y)
    (let-values ([(q r) (div0-and-mod0 x y)])
      r))

  ;; rationalize function taken from Larceny, with the following copyright
  ;; attribution:
  ;; 
  ;; From MacScheme.
  ;;
  ;; This code was written by Alan Bawden.
  ;; Its copyright status is unknown to me [i.e., to Will. --lars]
  ;;
  ;; Modified for R6RS semantics on infinities and NaNs.

  (define (rationalize x e)
    (define (simplest-rational x y)
      (define (simplest-rational-internal x y)      ; assumes 0 < X < Y
        (let ((fx (floor x))        ; [X] <= X < [X]+1
              (fy (floor y)))       ; [Y] <= Y < [Y]+1, also [X] <= [Y]
          (cond ((not (< fx x))
                 ;; X is an integer so X is the answer:
                 fx)
                ((= fx fy)
                 ;; [Y] = [X] < X < Y so expand the next term in the continued
                 ;; fraction:
                 (+ fx (/ (simplest-rational-internal
                           (/ (- y fy)) (/ (- x fx))))))
                (else
                 ;; [X] < X < [X]+1 <= [Y] <= Y so [X]+1 is the answer:
                 (+ 1 fx)))))
      (cond ((< y x)
             ;; Y < X so swap and try again:
             (simplest-rational y x))
            ((not (< x y))
             ;; X = Y so if either is a rational that is the answer, otherwise
             ;; X and Y are both infinite or both NaN.
             (cond ((rational? x) x)
                   ((rational? y) y)
                   (else x)))
            ((positive? x) 
             ;; 0 < X < Y which is what SIMPLEST-RATIONAL-INTERNAL expects:
             (simplest-rational-internal x y))
            ((negative? y)
             ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
             (- (simplest-rational-internal (- y) (- x))))
            ((and (exact? x) (exact? e))
             ;; X <= 0 <= Y so zero is the answer:
             0)
            (else 0.0)))
    (simplest-rational (- x e) (+ x e)))
  
  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  (define-syntax case
    (syntax-rules (else)
      ((case expr0
         ((key ...) res1 res2 ...)
         ...
         (else else-res1 else-res2 ...))
       (let ((tmp expr0))
         (cond
          ((memv tmp '(key ...)) res1 res2 ...)
          ...
          (else else-res1 else-res2 ...))))
      ((case expr0
         ((keya ...) res1a res2a ...)
         ((keyb ...) res1b res2b ...)
         ...)
       (let ((tmp expr0))
         (cond
          ((memv tmp '(keya ...)) res1a res2a ...)
          ((memv tmp '(keyb ...)) res1b res2b ...)
          ...)))))

  (define-syntax let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
       (let () body1 body2 ...))
      ((let* ((name1 expr1) (name2 expr2) ...)
         body1 body2 ...)
       (let ((name1 expr1))
         (let* ((name2 expr2) ...)
           body1 body2 ...)))))
  
  ;; TODO: All of the car/cdr combinations
  (define caar (lambda (x) (car (car x))))
  (define cadr (lambda (x) (car (cdr x))))
  (define cdar (lambda (x) (cdr (car x))))
  (define cddr (lambda (x) (cdr (cdr x))))
  (define caaar (lambda (x) (car (car (car x)))))
  (define caadr (lambda (x) (car (car (cdr x)))))
  (define cadar (lambda (x) (car (cdr (car x)))))
  (define cdaar (lambda (x) (cdr (car (car x)))))
  (define caddr (lambda (x) (car (cdr (cdr x)))))
  (define cdadr (lambda (x) (cdr (car (cdr x)))))
  (define cddar (lambda (x) (cdr (cdr (car x)))))
  (define cdddr (lambda (x) (cdr (cdr (cdr x)))))
  (define caaaar (lambda (x) (car (car (car (car x))))))
  (define caaadr (lambda (x) (car (car (car (cdr x))))))
  (define caadar (lambda (x) (car (car (cdr (car x))))))
  (define caaddr (lambda (x) (car (car (cdr (cdr x))))))
  (define cadaar (lambda (x) (car (cdr (car (car x))))))
  (define cadadr (lambda (x) (car (cdr (car (cdr x))))))
  (define caddar (lambda (x) (car (cdr (cdr (car x))))))
  (define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
  (define cdaaar (lambda (x) (cdr (car (car (car x))))))
  (define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
  (define cdadar (lambda (x) (cdr (car (cdr (car x))))))
  (define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
  (define cddaar (lambda (x) (cdr (cdr (car (car x))))))
  (define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
  (define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
  (define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
  
  ;; call/cc is an alias of call-with-current-continuation
  (define call/cc call-with-current-continuation)

  ;; TODO: a lot of these should be made into rust functions, as of right now
  ;; these are quite slow.

  (define (for-each proc list1 . listn)
    (let ([items (apply zip (cons list1 listn))])
      (let loop ((items items))
        (unless (null? items)
          (apply proc (car items))
          (loop (cdr items))))))

  (define (string-for-each proc str1 . strn)
    (let ([items (apply zip (cons (string->list str1) (map string->list strn)))])
      (let loop ((items items))
        (unless (null? items)
          (apply proc (car items))
          (loop (cdr items))))))
  
  (define (vector-map proc vec1 . vecn)
    (list->vector (apply map proc (cons (vector->list vec1) (map vector->list vecn)))))

  (define (vector-for-each proc vec1 . vecn)
    (let ([items (apply zip (cons (vector->list vec1) (map vector->list vecn)))])
      (let loop ((items items))
        (unless (null? items)
          (apply proc (car items))
          (loop (cdr items))))))

  (define (make-list n)
    (if (> n 0)
        (cons #f (make-list (- n 1)))
        '()))

  (define (list-copy lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (list-copy (cdr lst)))))

  (define (list-tail lst n)
    (if (> n 0)
        (list-tail (cdr lst) (- n 1))
        lst))

  (define (list-ref lst n)
    (if (> n 0)
        (list-ref (cdr lst) (- n 1))
        (car lst)))

  (define (reverse ls)
    (define (reverse ls acc)
      (if (null? ls)
          acc
          (reverse (cdr ls) (cons (car ls) acc))))
    (reverse ls '()))

  (define (positive? x) (> x 0))

  (define (negative? x) (< x 0))

  (define (abs x)
    (if (< x 0)
        (- x)
        x))

  (define (min x . xs)
    (let loop ((xs xs) (smallest x))
      (if (null? xs)
          smallest
          (loop (cdr xs)
                (if (< (car xs) smallest)
                    (car xs)
                    smallest)))))

  (define (max x . xs)
    (let loop ([xs xs] [biggest x] [is-inexact (inexact? x)])
      (if (null? xs)
          (if is-inexact (inexact biggest) biggest)
          (let ([x (car xs)])
            (loop (cdr xs)
                  (if (> x biggest) x biggest)
                  (or is-inexact (inexact? x)))))))

  ;; quasiquote is Copyright (c) 2006 Andre van Tonder
  ;; 
  ;; Copyright statement at http://srfi.schemers.org/srfi-process.html
  ;;
  ;; Optimised version copied from portable syntax-case (Dybvig)
  (define-syntax quasiquote
     (let ()
       (define (quasi p lev)
         (syntax-case p (unquote quasiquote)
           ((unquote p)
            (if (= lev 0)
              (syntax ("value" p))
                (quasicons (syntax ("quote" unquote))
                           (quasi (syntax (p)) (- lev 1)))))
           ((quasiquote p) (quasicons (syntax ("quote" quasiquote))
                                      (quasi (syntax (p)) (+ lev 1))))
           ((p . q)
            (syntax-case (syntax p) (unquote unquote-splicing)
              ((unquote p ...)
               (if (= lev 0)
                   (quasilist* (syntax (("value" p) ...))
                               (quasi (syntax q) lev))
                   (quasicons
                    (quasicons (syntax ("quote" unquote))
                               (quasi (syntax (p ...)) (- lev 1)))
                    (quasi (syntax q) lev))))
              ((unquote-splicing p ...)
               (if (= lev 0)
                   (quasiappend (syntax (("value" p) ...))
                                (quasi (syntax q) lev))
                   (quasicons
                    (quasicons (syntax ("quote" unquote-splicing))
                               (quasi (syntax (p ...)) (- lev 1)))
                    (quasi (syntax q) lev))))
              (_ (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))))
           (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
           (p (syntax ("quote" p)))))
       (define (vquasi p lev)
         (syntax-case p ()
           ((p . q)
            (syntax-case (syntax p) (unquote unquote-splicing)
              ((unquote p ...)
               (if (= lev 0)
                   (quasilist* (syntax (("value" p) ...))
                               (vquasi (syntax q) lev))
                   (quasicons
                    (quasicons (syntax ("quote" unquote))
                               (quasi (syntax (p ...)) (- lev 1)))
                    (vquasi (syntax q) lev))))
              ((unquote-splicing p ...)
               (if (= lev 0)
                   (quasiappend (syntax (("value" p) ...))
                                (vquasi (syntax q) lev))
                   (quasicons
                    (quasicons
                     (syntax ("quote" unquote-splicing))
                     (quasi (syntax (p ...)) (- lev 1)))
                    (vquasi (syntax q) lev))))
              (_ (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
           (() (syntax ("quote" ())))))
       (define (quasicons x y)
         (with-syntax ((x x) (y y))
           (syntax-case (syntax y) ()
             (("quote" dy)
              (syntax-case (syntax x) ()
                (("quote" dx) (syntax ("quote" (dx . dy))))
                (_ (if (null? (syntax dy))
                       (syntax ("list" x))
                       (syntax ("list*" x y))))))
             (("list" . stuff) (syntax ("list" x . stuff)))
             (("list*" . stuff) (syntax ("list*" x . stuff)))
             (_ (syntax ("list*" x y))))))
       (define (quasiappend x y)
         (syntax-case y ()
           (("quote" ())
            (cond
              ((null? x) (syntax ("quote" ())))
            ((null? (cdr x)) (car x))
              (else (with-syntax (((p ...) x)) (syntax ("append" p ...))))))
           (_
            (cond
              ((null? x) y)
              (else (with-syntax (((p ...) x) (y y))
                     (syntax ("append" p ... y))))))))
       (define (quasilist* x y)
         (let f ((x x))
           (if (null? x)
               y
               (quasicons (car x) (f (cdr x))))))
       (define (quasivector x)
         (syntax-case x ()
           (("quote" (x ...)) (syntax ("quote" #(x ...))))
           (_
            (let f ((y x) (k (lambda (ls)
                               (quasisyntax
                                ("vector" (unsyntax-splicing ls))))))
              (syntax-case y ()
                (("quote" (y ...)) (k (syntax (("quote" y) ...))))
                (("list" y ...) (k (syntax (y ...))))
                (("list*" y ... z)
                 (f (syntax z) (lambda (ls) (k (append (syntax (y ...)) ls)))))
                (else (quasisyntax ("list->vector" (unsyntax x)))))))))
       (define (emit x)
         (syntax-case x ()
           (("quote" x) (syntax 'x))
           (("list" x ...)
            (quasisyntax
             (list (unsyntax-splicing (map emit (syntax (x ...)))))))
           ;; could emit list* for 3+ arguments if implementation supports list*
           (("list*" x ... y)
            (let f ((x* (syntax (x ...))))
              (if (null? x*)
                  (begin
                    (emit (syntax y)))
                  (begin
                    (quasisyntax
                     (cons (unsyntax (emit (car x*))) (unsyntax (f (cdr x*)))))))))
           (("append" x ...)
            (quasisyntax
             (append (unsyntax-splicing (map emit (syntax (x ...)))))))
           (("vector" x ...)
            (quasisyntax
             (vector (unsyntax-splicing (map emit (syntax (x ...)))))))
           (("list->vector" x)
            (quasisyntax (list->vector (unsyntax (emit (syntax x))))))
           (("value" x) (syntax x))))
       (lambda (x)
         (syntax-case x ()
           ;; convert to intermediate language, combining introduced (but not
           ;; unquoted source) quote expressions where possible and choosing
           ;; optimal construction code otherwise, then emit Scheme code
           ;; corresponding to the intermediate language forms.
           ((_ e) (emit (quasi (syntax e) 0)))))))

  (define-syntax assert
    (syntax-rules ()
      [(_ expr)
       (unless expr
           (assertion-violation #f "assertion failed"))])))
