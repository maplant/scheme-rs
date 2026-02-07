(library (rnrs hashtables (6))
  (export (import (rnrs hashtables builtins (6)))
          make-eq-hashtable make-eqv-hashtable alist->hashtable)
  (import (only (rnrs base builtins (6)) eqv? eq? equal? null? car cdr)
          (rnrs base primitives (6)))

  (define (make-eq-hashtable . k)
    (if (null? k)
        (make-hashtable eq-hash eq?)
        (make-hashtable eq-hash eq? (car k))))

  (define (make-eqv-hashtable . k)
    (if (null? k)
        (make-hashtable eqv-hash eqv?)
        (make-hashtable eqv-hash eqv? (car k))))

  (define (make-equal-hashtable . k)
    (if (null? k)
        (make-hashtable equal-hash equal?)
        (make-hashtable equal-hash equal? (car k))))

  (define (alist->hashtable alist)
    (let loop ([ht (make-equal-hashtable)]
               [alist alist])
      (if (null? alist)
          ht
          (begin
            (let [(head (car alist))]
              (hashtable-set! ht (car head) (cdr head))
              (loop ht (cdr alist))))))))
