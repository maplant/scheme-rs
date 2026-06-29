(library (lang (1))
  (export (import (match))
          (import (only (rnrs) define lambda let let* letrec set! quote
                        quasiquote))
          include define* lambda*
          %keyword-ref %collect-rest)
  (import (rnrs) (srfi :88))

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
             #'(begin exp ...)))])))

  ;; ── keyword argument helpers ───────────────────────────────────────

  (define (%keyword-ref args keyword default)
    (let loop ((rest args))
      (cond
        ((null? rest) default)
        ((and (keyword? (car rest)) (eq? (car rest) keyword))
         (if (null? (cdr rest))
           (error 'lambda* "keyword argument has no value" keyword)
           (cadr rest)))
        (else (loop (cdr rest))))))

  (define (%collect-rest args keyword-syms)
    (let loop ((rest args) (acc '()))
      (cond
        ((null? rest) (reverse acc))
        ((keyword? (car rest))
         (if (memq (car rest) keyword-syms)
           (loop (if (null? (cdr rest)) '() (cddr rest)) acc)
           (loop (cdr rest) (cons (car rest) acc))))
        (else (loop (cdr rest) (cons (car rest) acc))))))

  ;; ── lambda* ────────────────────────────────────────────────────────

  (define-syntax lambda*
    (lambda (stx)
      (define (parse-formals kw formals)
        (let loop ((fs formals) (state 'required)
                   (req '()) (kw-specs '()) (rest-id #f))
          (syntax-case fs ()
            (() (values (reverse req) (reverse kw-specs) rest-id))
            ((#:key . more) (loop #'more 'key req kw-specs rest-id))
            ((#:rest name . more) (loop #'more state req kw-specs #'name))
            (((name default) . more)
             (eq? state 'key)
             (let* ((sym (syntax->datum #'name))
                    (ks (datum->syntax kw (string->keyword (symbol->string sym)))))
               (loop #'more state req
                     (cons (list #'name ks #'default) kw-specs) rest-id)))
            ((name . more)
             (identifier? #'name)
             (if (eq? state 'required)
               (loop #'more state (cons #'name req) kw-specs rest-id)
               (let* ((sym (syntax->datum #'name))
                      (ks (datum->syntax kw (string->keyword (symbol->string sym)))))
                 (loop #'more state req
                       (cons (list #'name ks #'#f) kw-specs) rest-id)))))))

      (syntax-case stx ()
        ((kw formals body ...)
         (let-values (((req kw-specs rest-id) (parse-formals #'kw #'formals)))
           (if (and (null? kw-specs) (not rest-id))
             (with-syntax (((r ...) req))
               #'(lambda (r ...) body ...))
             (let ((args-id (datum->syntax #'kw '%args)))
               (with-syntax
                 (((req-name ...) req)
                  (%args args-id)
                  ((kw-let ...)
                   (map (lambda (spec)
                          (with-syntax ((name (car spec))
                                        (kw-sym (cadr spec))
                                        (default (caddr spec))
                                        (a args-id))
                            #'(name (%keyword-ref a kw-sym default))))
                        kw-specs))
                  (rest-let
                    (if rest-id
                      (with-syntax ((rname rest-id)
                                    (a args-id)
                                    ((ks ...)
                                     (map cadr kw-specs)))
                        (list #'(rname (%collect-rest a (list ks ...)))))
                      '())))
               #'(lambda (req-name ... . %args)
                   (let* (kw-let ... . rest-let)
                     body ...))))))))))

  (define-syntax define*
    (syntax-rules ()
      ((_ (name . formals) body ...)
       (define name (lambda* formals body ...)))
      ((_ name expr)
       (define name expr))))
)
