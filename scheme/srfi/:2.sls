;;; SRFI-2/202: and-let*
;;; Implementation based on SRFI-202 (Pattern-matching variant of and-let*)
;;; which is a strict superset of SRFI-2.
;;;
;;; SPDX-License-Identifier: MIT

(library (srfi :2)
  (export and-let*)
  (import (rnrs)
          (match))

(define-syntax and-let*
  (lambda (stx)
    (syntax-case stx (values)

      ((_)
       #'#t)

      ((_ ())
       #'#t)

      ((_ () body ...)
       #'(let () body ...))

      ((_ ((name binding) rest ...) body ...)
       (identifier? #'name)
       #'(let ((name binding))
           (and name
                (and-let* (rest ...)
                  body ...))))

      ((_ (((values . structure) expression) rest ...)
          body ...)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               (structure
                (and-let* (rest ...)
                  body ...))
               (_ #f)))))

      ((_ ((value expression) rest ...) body ...)
       #'(match expression
           (value
            (and-let* (rest ...)
              body ...))
           (_ #f)))

      ((_ ((condition) rest ...)
          body ...)
       #'(and condition
              (and-let* (rest ...)
                body ...)))

      ((_ ((value * ... expression) rest ...) body ...)
       (identifier? #'value)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               ((value * ... . _)
                (and value
                     (and-let* (rest ...)
                       body ...)))
               (_ #f)))))

      ((_ ((value ... expression) rest ...) body ...)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               ((value ... . _)
                (and-let* (rest ...)
                  body ...))
               (_ #f)))))

      )))

)
