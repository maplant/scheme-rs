(import (rnrs) (test) (lang))

;; The _ wildcard must evaluate the body, not return it as data.
;; Regression: a spurious quote on the _ rule in match-two caused
;; the success continuation to be returned as a quoted list.

(define result #f)
(match '(hello 1)
  (('hello _) (set! result 'ok)))
(assert-equal? result 'ok)
