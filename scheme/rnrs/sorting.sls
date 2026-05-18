(library (rnrs sorting (6))
  (export list-sort vector-sort vector-sort!)
  (import (rnrs base primitives))

  (define (list-sort proc list))

  (define (vector-sort proc vector))

  (define (vector-sort! proc vector)))
