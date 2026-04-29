(library (rnrs bytevectors (6))
  (export endianness native-endiannes bytevector? make-bytevector
          bytevector-length bytevector=? bytevector-u8-ref u8-list->bytevector
          bytevector-push! bytevector-insert! bytevector-take!)
  (import (rnrs base (6)))

  (define-syntax endianness
    (syntax-rules ()
      [(_ little) 'little]
      [(_ big) 'big])))
