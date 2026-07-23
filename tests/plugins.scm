(import (test) (test plugin))

(assert-equal? (test-plugin-add 2 3) 5)
(assert-equal? (test-plugin-add -1 1) 0)
(assert-equal? (test-plugin-greeting) "hello from plugin")
(assert-equal? (counter-value (make-counter 42)) 42)
(assert-equal? (counter-value (make-counter 0)) 0)
(assert-equal? (counter-value (make-counter -7)) -7)
