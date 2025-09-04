;; List functions that are needed by quasiquote and thus need to be evaluated
;; earlier. Really, these should just be moved into builtins. 

(library (rnrs lists (6))
  (export append map)
  (import (rnrs base builtins (6)) (rnrs base special-keywords (6)))

  (define (append l m)
    (if (null? l) m
        (cons (car l) (append (cdr l) m))))
 
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
               (apply map function (map1 cdr lists)))))))
   
