(import (test) (test plugin))

(assert-equal? (test-plugin-add 2 3) 5)
(assert-equal? (test-plugin-add -1 1) 0)
(assert-equal? (test-plugin-greeting) "hello from plugin")
