(import (rnrs) (threads) (test))

;; join must wait for the thread to finish even when it wins the race to
;; the result cell.
(assert-equal? (join (spawn (lambda () 42))) 42)
