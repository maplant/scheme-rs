(library (lang (1))
  (export (import (only (rnrs) define lambda let let* letrec set! quote
                        quasiquote)) include)
  (import (rnrs))

  ;; R6RS-lib definition of include 
  (define-syntax include
    (lambda (x)
      (define (read-file fn k)
        (let ([p (open-file-input-port fn (file-options) (buffer-mode block) (native-transcoder))])
          (let f ([x (get-datum p)])
            (if (eof-object? x)
                (begin (close-port p) '())
                (cons (datum->syntax k x)
                      (f (get-datum p)))))))
      (syntax-case x ()
        [(k filename)
         (let ([fn (syntax->datum #'filename)])
           (with-syntax ([(exp ...)
                          (read-file fn #'k)])
             #'(begin exp ...)))]))))
