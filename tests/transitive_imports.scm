(import (rnrs) (test) (tests lib-b))

(assert-equal? 4 (macro-b 1))
