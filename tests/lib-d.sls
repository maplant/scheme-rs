(library (tests lib-d)
  (export my-fn)
  (import (rnrs) (tests lib-c))

  (make-wrapped (my-fn x) (+ x 1)))
