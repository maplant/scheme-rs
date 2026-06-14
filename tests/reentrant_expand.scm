(import (rnrs) (test) (tests lib-d))

(assert-equal? '(wrapped 11) (my-fn 10))
