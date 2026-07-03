(import (rnrs) (test))

;; A Scheme-defined hash function that hits an async bridge (display).
;; Hashtable operations invoke it from sync Rust that is itself running
;; inside the async evaluator on a Tokio worker thread, so this exercises
;; block_on nested within the async evaluator. This used to raise "attempt
;; to apply async function in a sync-only context".
(define (loud-hash x)
  (display "")
  x)

(define ht (make-hashtable loud-hash =))
(hashtable-set! ht 1 'one)
(hashtable-set! ht 2 'two)
(assert-equal? (hashtable-ref ht 1 'nope) 'one)
(assert-equal? (hashtable-ref ht 2 'nope) 'two)
(assert-equal? (hashtable-ref ht 3 'nope) 'nope)
(hashtable-update! ht 1 (lambda (v) (display "") 'uno) 'default)
(assert-equal? (hashtable-ref ht 1 'nope) 'uno)
