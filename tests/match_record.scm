(import (rnrs) (rust) (test) (match))

(define-rust-type vec2 vec2-rtd make-vec2 vec2?)
(define-rust-type vec3 vec3-rtd make-vec3 vec2?)

(assert-equal?
 (match (make-vec2 1 2)
   (($ vec2 a b) (list a b)))
 '(1.0 2.0))

(assert-equal?
 (match (make-vec2 1 2)
   ((@ vec2 (y b) (x a)) (list a b)))
 '(1.0 2.0))

(assert-equal?
 (match (make-vec3 1 2 3)
   (($ vec3 a b c) (list a b c)))
 '(1.0 2.0 3.0))

(assert-equal?
 (match (make-vec3 1 2 3)
   ((@ vec3 (z c) (y b) (x a)) (list a b c)))
 '(1.0 2.0 3.0))
