(library (rnrs bytevectors (6))
  (export endianness native-endiannes bytevector? make-bytevector
          bytevector-length bytevector-fill=? u8-list->bytevector)
  (import (rnrs base (6)))

  (define-syntax endianness
    (syntax-rules ()
      [(_ little) 'little]
      [(_ big) 'big])))
