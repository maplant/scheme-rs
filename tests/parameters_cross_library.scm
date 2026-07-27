(import (rnrs) (test) (tests lib-p))
(assert-equal? loaded? #t)
(assert-equal? (p) 99)
