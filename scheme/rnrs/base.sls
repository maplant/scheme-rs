(library (rnrs base (6))
  (export cond case let* caar cadr cddr caaar caadr cadar cdaar
          caddr cdadr cddar cdddr call/cc for-each string-for-each
          vector-for-each append make-list list-copy list-tail list-ref assoc
          map reverse positive? negative? abs min max quasiquote
          identifier-syntax assert
          (import (rnrs base builtins (6)))
          (import (rnrs syntax-rules (6)))
          (import (rnrs values (6)))
          (import (rnrs letrec (6)))
          (import (except (rnrs base special-keywords (6)) $undefined)))
  
  (import (rnrs lists (6))
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs io simple builtins)
          (only (rnrs base special-keywords (6)) $undefined))

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
  (define cddr (lambda (x) (cdr (cdr x))))
  (define caaar (lambda (x) (car (car (car x)))))
  (define caadr (lambda (x) (car (car (cdr x)))))
  (define cadar (lambda (x) (car (cdr (car x)))))
  (define cdaar (lambda (x) (cdr (car (car x)))))
  (define caddr (lambda (x) (car (cdr (cdr x)))))
  (define cdadr (lambda (x) (cdr (car (cdr x)))))
  (define cddar (lambda (x) (cdr (cdr (car x)))))
  (define cdddr (lambda (x) (cdr (cdr (cdr x)))))

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

  #;(define (string-for-each proc str1 . strn)
    (let loop ([i 0]
               [len (string-length str1)])
      (when (< i len)
        (proc (string-ref str1 i))
        (loop (+ i 1) len)))
    (unless (null? strn)
      (apply string-for-each (cons proc strn))))

  #;(define (vector-for-each proc vector1 . vectorn)
    (let loop ([i 0]
               [len (vector-length vector1)])
      (when (< i len)
        (proc (vector-ref vector1 i))
        (loop (+ i 1) len)))
    (unless (null? vectorn)
      (apply vector-for-each (cons proc vectorn))))

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

  (define (assoc k lst)
    (if (null? lst)
        #f
        (let ((pair (car lst)))
          (if (equal? (car pair) k)
              pair
              (assoc k (cdr lst))))))

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
    (let loop ((xs xs) (biggest x))
      (if (null? xs)
          biggest
          (loop (cdr xs)
                (if (> (car xs) biggest)
                    (car xs)
                    biggest)))))

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
