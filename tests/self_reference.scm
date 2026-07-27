(import (rnrs) (test) (tests lib-x))

(assert-equal? loaded? #t)
(assert-equal? val 10)
(assert-equal? (f 2) 3)
