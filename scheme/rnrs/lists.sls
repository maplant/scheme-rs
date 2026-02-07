(library (rnrs lists (6))
  (export find for-all exists filter partition fold-left fold-right remp remove
          remv remq memp member memv memq assp assoc assv assq cons*)

  (import (rnrs base builtins (6))
          (rnrs base primitives (6))
          (rnrs control (6))
          (rnrs values (6)))

  (define call/cc call-with-current-continuation)

  (define (find proc list)
    (if (null? list)
        #f
        (let [(head (car list))]
          (if (proc (car list))
              head
              (find proc (cdr list))))))

  (define (for-all proc list1 . listn)
    (let ([items (apply zip (cons list1 listn))])
      (if (null? items)
          #t
          (call/cc
           (lambda (return)
             (let loop ((items items))
               (if (null? (cdr items))
                   (apply proc (car items))
                   (and (apply proc (car items))
                        (loop (cdr items))))))))))

  (define (exists proc list1 . listn)
    (let ([items (apply zip (cons list1 listn))])
      (if (null? items)
          #f
          (call/cc
           (lambda (return)
             (let loop ((items items))
               (if (null? (cdr items))
                   (apply proc (car items))
                   (or (apply proc (car items))
                       (loop (cdr items))))))))))

  (define (filter proc list)
    (if (null? list)
        '()
        (let [(head (car list))
              (tail (filter proc (cdr list)))]
          (if (proc head)
              (cons head tail)
              tail))))

  (define (partition proc list)
    (if (null? list)
        (values '() '())
        (let [(head (car list))]
          (let-values [((succ fail) (partition proc (cdr list)))]
            (if (proc head)
                (values (cons head succ) fail)
                (values succ (cons head fail)))))))

  (define (fold-left combine accum list1 . listn)
    (let loop ([items (apply zip (cons list1 listn))])
      (if (null? items)
          accum
          (begin
            (set! accum (apply combine accum (car items)))
            (loop (cdr items))))))

  (define (append-if-list item to-append)
    (if (pair? item)
        (append item to-append)
        (list item to-append)))

  (define (reverse ls)
   (define (reverse ls acc)
     (if (null? ls)
         acc
         (reverse (cdr ls) (cons (car ls) acc))))
   (reverse ls '()))

  (define (fold-right combine accum list1 . listn)
    (let loop ([items (reverse (apply zip (cons list1 listn)))])
      (if (null? items)
          accum
          (begin
            (set! accum (apply combine (append (car items) (list accum))))
            (loop (cdr items))))))

  (define (remp proc list)
    (let loop ([out '()]
               [items list])
      (if (null? items)
          (reverse out)
          (begin
            (if (not (proc (car items)))
                (set! out (cons (car items) out)))
            (loop out (cdr items))))))

  (define (remove obj list)
    (remp (lambda (x) (equal? x obj)) list))

  (define (remv obj list)
    (remp (lambda (x) (eqv? x obj)) list))

  (define (remq obj list)
    (remp (lambda (x) (eq? x obj)) list))

  (define (memp proc list)
    (let loop ([items list])
      (if (null? items)
          #f
          (if (proc (car items))
              items
              (loop (cdr items))))))

  (define (member obj list)
    (memp (lambda (x) (equal? x obj)) list))

  (define (memv obj list)
    (memp (lambda (x) (eqv? x obj)) list))

  (define (memq obj list)
    (memp (lambda (x) (eq? x obj)) list))

  (define (assp proc alist)
    (let loop ([items alist])
      (if (null? items)
          #f
          (let ([head (car items)])
            (if (proc (car head))
                head
                (loop (cdr items)))))))

  (define (assoc obj alist)
    (assp (lambda (x) (equal? x obj)) alist))

  (define (assv obj alist)
    (assp (lambda (x) (eqv? x obj)) alist))

  (define (assq obj alist)
    (assp (lambda (x) (eq? x obj)) alist))

  (define (cons* obj1 . objs)
    (define (cons* . objs)
      (let ([head (car objs)]
            [tail (cdr objs)])
        (if (null? tail)
            head
            (cons head (apply cons* tail)))))
    (apply cons* (cons obj1 objs))))
              

