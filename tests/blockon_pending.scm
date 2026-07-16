(import (rnrs) (test) (only (threads) spawn join) (only (async) sleep))

;; sleep from (async) suspends on a tokio timer, forcing a real Poll::Pending
;; round-trip through the block_on in the spawned OS thread instead of
;; first-poll completion.
(let ((h (spawn (lambda () (sleep 5) 'woke))))
  (assert-equal? (join h) 'woke))
