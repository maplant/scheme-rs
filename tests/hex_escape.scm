(import (rnrs) (test))

;; R6RS 4.2.1: \x<hex>; denotes the single character whose Unicode scalar
;; value is the whole hex digit run, not one character per digit pair.

(assert-equal? (string-length "\x1F468;") 1)
(assert-equal? (char->integer (string-ref "\x1F468;" 0)) 128104)
(assert-equal? (string-length "\xe9;") 1)
(assert-equal? (char->integer (string-ref "\xe9;" 0)) 233)
(assert-equal? (symbol->string '\x3BB;) (string #\x3BB))
