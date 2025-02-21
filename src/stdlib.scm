;; Contains standard definitions for functions that can be easily defined in 
;; terms of forms supported by the compiler. This allows us to provide  a minimum
;; amount of core functionality while still supporting the entire r6rs spec.
;;
;; By and large, these macro definitions come from Appendix B of the R6RS
;; standard. This allows for use to test the correctness of the compiler.
;;
;; This code is for definitions only. Most builtins are not provided at this point.


;; Define syntax-rules in terms of syntax-case:
(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      ((_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
                 (syntax-case x (i ...)
                   ((dummy . pattern) (syntax template))
                   ...))))))) 

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ ((p e0) ...) e1 e2 ...)
       (syntax (syntax-case (list e0 ...) ()
		 ((p ...) (let () e1 e2 ...))))))))
;; 
;; Aliases:
;; 
(define equal? eqv?) ;; TODO(map): This is INCORRECT, needs to be fixed!
(define (member obj list)
  (memp (lambda (x) (equal? x obj)) list))
(define (memv obj list)
  (memp (lambda (x) (eqv? x obj)) list))
(define (memq obj list)
  (memp (lambda (x) (eq? x obj)) list))
; (define call-with-current-continuation call/cc)

;;
;; WIP: All of the car/cdr combinations
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))

;;
;; Complex definitions:
;;
(define (memp proc list)
  (if (and (pair? list)
           (proc (car list)))
      list
      (if (null? list) #f (memp proc (cdr list)))))

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


(define-syntax letrec
  (syntax-rules ()
    ((letrec () body1 body2 ...)
     (let () body1 body2 ...))
    ((letrec ((var init) ...) body1 body2 ...)
     (letrec-helper
      (var ...)
      ()
      ((var init) ...)
      body1 body2 ...))))

(define-syntax letrec-helper
  (syntax-rules ()
    ((letrec-helper
      ()
      (temp ...)
      ((var init) ...)
      body1 body2 ...)
     (let ((var <undefined>) ...)
       (let ((temp init) ...)
         (set! var temp)
         ...)
       (let () body1 body2 ...)))
    ((letrec-helper
      (x y ...)
      (temp ...)
      ((var init) ...)
      body1 body2 ...)
     (letrec-helper
      (y ...)
      (newtemp temp ...)
      ((var init) ...)
      body1 body2 ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 <undefined>) ...)
       (set! var1 init1) ...
       (let () body1 body2 ...)))))

(define (values . things)
  (call-with-current-continuation
   (lambda (cont) (apply cont things))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body1 body2 ...)
     (let-values-helper1
      ()
      (binding ...)
      body1 body2 ...))))

(define-syntax let-values-helper1
  ;; map over the bindings
  (syntax-rules ()
    ((let-values
         ((id temp) ...)
       ()
       body1 body2 ...)
     (let ((id temp) ...) body1 body2 ...))
    ((let-values
         assocs
       ((formals1 expr1) (formals2 expr2) ...)
       body1 body2 ...)
     (let-values-helper2
      formals1
      ()
      expr1
      assocs
      ((formals2 expr2) ...)
      body1 body2 ...))))

(define-syntax let-values-helper2
  ;; create temporaries for the formals
  (syntax-rules ()
    ((let-values-helper2
      ()
      temp-formals
      expr1
      assocs
      bindings
      body1 body2 ...)
     (call-with-values
         (lambda () expr1)
       (lambda temp-formals
         (let-values-helper1
          assocs
          bindings
          body1 body2 ...))))
    ((let-values-helper2
      (first . rest)
      (temp ...)
      expr1
      (assoc ...)
      bindings
      body1 body2 ...)
     (let-values-helper2
      rest
      (temp ... newtemp)
      expr1
      (assoc ... (first newtemp))
      bindings
      body1 body2 ...))
    ((let-values-helper2
      rest-formal
      (temp ...)
      expr1
      (assoc ...)
      bindings
      body1 body2 ...)
     (call-with-values
         (lambda () expr1)
       (lambda (temp ... . newtemp)
         (let-values-helper1
          (assoc ... (rest-formal newtemp))
          bindings
          body1 body2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   #f ; avoid empty begin
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do "step" var step ...)
                         ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (fmls b1 b2 ...))
     (lambda fmls b1 b2 ...))
    ((_ (fmls b1 b2 ...) ...)
     (lambda args
       (let ((n (length args)))
         (case-lambda-help args n
                           (fmls b1 b2 ...) ...))))))

(define-syntax case-lambda-help
  (syntax-rules ()
    ((_ args n)
     (assertion-violation #f "unexpected number of arguments"))
    ((_ args n ((x ...) b1 b2 ...) more ...)
     (if (= n (length '(x ...)))
         (apply (lambda (x ...) b1 b2 ...) args)
         (case-lambda-help args n more ...)))
    ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
     (if (>= n (length '(x1 x2 ...)))
         (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                args)
         (case-lambda-help args n more ...)))
    ((_ args n (r b1 b2 ...) more ...)
     (apply (lambda r b1 b2 ...) args))))

(define (for-each func lst . remaining)
  (let loop ((rest lst))
    (unless (null? rest)
      (func (car rest))
      (loop (cdr rest))))
  (if (not (null? remaining))
      (begin
        (apply for-each (cons func remaining)))))

(define (append l m)
 (if (null? l) m
  (cons (car l) (append (cdr l) m))))

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

(define (map function list1 . more-lists)
  (define (some? function list)
    ;; returns #f if (function x) returns #t for 
    ;; some x in the list
    (and (pair? list)
         (or (function (car list))
             (some? function (cdr list)))))
  (define (map1 function list)
    ;; non-variadic map.  Returns a list whose elements are
    ;; the result of calling function with corresponding
    ;; elements of list
    (if (null? list)
        '()
        (cons (function (car list))
              (map1 function (cdr list)))))
  ;; Variadic map implementation terminates
  ;; when any of the argument lists is empty
  (let ((lists (cons list1 more-lists)))
    (if (some? null? lists)
        '()
        (cons (apply function (map1 car lists)) 
              (apply map function (map1 cdr lists))))))

(define (reverse ls)
  (define (reverse ls acc)
    (if (null? ls)
        acc
        (reverse (cdr ls) (cons (car ls) acc))))
  (reverse ls '()))
