(import (rnrs) (async) (test))

;; A hash function that suspends on the tokio timer, run under the default
;; current_thread test flavor: one OS thread drives both the evaluator and
;; the reactor. Under the sync callback path this raised on the async
;; bridge; with hashtable callbacks async all the way, the timer just fires.
(define (sleepy-hash x)
  (sleep 5)
  x)

(define ht (make-hashtable sleepy-hash =))
(hashtable-set! ht 1 'one)
(hashtable-set! ht 2 'two)
(assert-equal? (hashtable-ref ht 1 'nope) 'one)
(assert-equal? (hashtable-ref ht 2 'nope) 'two)
(assert-equal? (hashtable-ref ht 3 'nope) 'nope)
