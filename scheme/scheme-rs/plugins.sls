(library (scheme-rs plugins)
  (export load-plugin)
  (import (rnrs) (scheme-rs plugins builtins))

  (define-syntax load-plugin
    (lambda (stx)
      (syntax-case stx ()
        ((_ path)
         (begin
           (%load-plugin (syntax->datum #'path))
           #'(values)))))))
