(import (rnrs) (threads) (test))

;; A spawned OS thread must be able to run async bridges: display compiles
;; to an AsyncBridge under the async feature. This used to raise "attempt
;; to apply async function in a sync-only context".
(define h
  (spawn (lambda ()
           (display "")
           'done)))

(assert-equal? (join h) 'done)
