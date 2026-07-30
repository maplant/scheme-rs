(import (rnrs) (generators) (test))

;; Every invocation of a captured continuation must see its own pristine copy
;; of the captured stack, no matter how many times it is invoked.

(define (run)
  (define k #f)
  (define results '())
  (define n 0)
  (define (nest depth)
    (if (= depth 0)
        (+ 1 (call/cc (lambda (c) (set! k c) 0)))
        (+ 1 (nest (- depth 1)))))
  (call/cc
   (lambda (return)
     (let ((r (nest 200)))
       (set! results (cons r results))
       (set! n (+ n 1))
       (if (< n 3)
           (k (* n 10))
           (return results))))))

(assert-equal? (run) '(221 211 201))

;; Re-entering a delimited continuation: each resumption of the generator has
;; to see the state the prompt was aborted from.
(define next
  (generator
   (lambda ()
     (yield 1)
     (yield 2)
     'done)))

(assert-equal? (next) 1)
(assert-equal? (next) 2)
(assert-equal? (next) 'done)
