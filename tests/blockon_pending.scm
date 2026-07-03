(import (rnrs) (test) (only (threads) spawn join) (only (async) sleep))

;; sleep from (async) suspends on a tokio timer, so these force real
;; Poll::Pending round-trips instead of first-poll completion.

;; Through Handle::block_on in a spawned OS thread.
(let ((h (spawn (lambda () (sleep 5) 'woke))))
  (assert-equal? (join h) 'woke))

;; Through the park-based block_on: hashtable ops run the hash function via
;; call_sync on a runtime worker, which parks until another worker fires the
;; timer. Requires a multi_thread runtime (see blockon_pending.rs).
(define (sleepy-hash x)
  (sleep 5)
  x)

(define ht (make-hashtable sleepy-hash =))
(hashtable-set! ht 1 'one)
(assert-equal? (hashtable-ref ht 1 'nope) 'one)
