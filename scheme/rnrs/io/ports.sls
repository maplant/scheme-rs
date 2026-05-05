(library (rnrs io ports (6))
  (export open-string-input-port buffer-mode buffer-mode? file-options
          open-bytevector-output-port string->bytevector bytevector->string
          (import (rnrs io builtins))
          (import (rnrs io conditions)))
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs enums)
          (rnrs syntax-case)
          (rnrs io simple builtins)
          (rnrs mutable-strings))

  (define-syntax buffer-mode
    (lambda (x)
      (syntax-case x (none line block)
        [(_ none) #''none]
        [(_ line) #''line]
        [(_ block) #''block])))

  (define (buffer-mode? sym)
    (or (eqv? sym 'none)
        (eqv? sym 'line)
        (eqv? sym 'block)))

  (define-syntax eol-style
    (lambda (x)
      (syntax-case x (lf cr crlf nel crnel ls none)
        ([_ lf] #''lf)
        ([_ cr] #''cr)
        ([_ crlf] #''crlf)
        ([_ nel] #''nel)
        ([_ crnel] #''crnel)
        ([_ ls] #''ls)
        ([_ none] #''none))))

  (define-syntax error-handling-mode
    (lambda (x)
      (syntax-case x (ignore raise replace)
        ([_ ignore] #''ignore)
        ([_ raise] #''raise)
        ([_ replace] #''replace))))

  (define-syntax file-options
    ;; TODO: Make this better
    (lambda (x)
      (syntax-case x ()
        ([_ symbols ...] #'((enum-set-constructor (default-file-options)) '(symbols ...))))))

  (define (string->bytevector string transcoder)
    (let-values (([string-port byte-extractor] (open-bytevector-output-port transcoder)))
      (put-string string-port string)
      (close-port string-port)
      (byte-extractor)))

  (define (bytevector->string bytevector transcoder)
    (let ((bytevector-port (open-bytevector-input-port bytevector transcoder)))
      (get-string-all bytevector-port)))

  (define (open-bytevector-output-port . maybe-transcoder)
    (define output (make-bytevector 0))
    (define curr-pos 0)
    (define (extraction-procedure)
      (set! curr-pos 0)
      (bytevector-take! output))
    (define output-port
      (make-custom-binary-output-port
       "bytevector-output-port"
       ;; write!
       (lambda (buff start count)
         (let fill ()
           (if (< (bytevector-length output) curr-pos)
               (begin (bytevector-push! output 0)
                      (fill))))
         (let write ([left count]
                     [start start])
           (if (> left 0)
               (begin (bytevector-insert! output curr-pos (bytevector-u8-ref buff start))
                      (set! curr-pos (+ curr-pos 1))
                      (write (- left 1) (+ start 1)))))
         count)
       ;; get-position
       (lambda () curr-pos)
       ;; set-position
       (lambda (pos) (set! curr-pos pos))
       ;; close
       #f))
    (if (null? maybe-transcoder)
        (values output-port extraction-procedure)
        (values (transcoded-port output-port (car maybe-transcoder)) extraction-procedure)))
  
  (define (read-bytevector input-bvec input-start output-bvec output-start count)
    (if (> count 0)
        (let ([offset (- count 1)])
          (bytevector-u8-set! output-bvec
                              (+ output-start offset)
                              (bytevector-u8-ref input-bvec (+ input-start offset)))
          (read-bytevector input-bvec input-start output-bvec output-start offset))))

  (define (open-bytevector-input-port input-bvec . maybe-transcoder)
    (define curr-pos 0)
    (define length (bytevector-length input-bvec))
    (define output-port
      (make-custom-binary-input-port
       "bytevector-input-port"
       ;; read!
       (lambda (output-bvec start count)
         (let ([adjusted-count (min count (- length curr-pos))])
           (read-bytevector input-bvec curr-pos output-bvec start adjusted-count)
           (set! curr-pos (+ curr-pos adjusted-count))
           adjusted-count))
       ;; get-position
       (lambda () curr-pos)
       ;; set-position
       (lambda (pos) (set! curr-pos pos))
       ;; close
       #f))
    (if (null? maybe-transcoder)
        output-port
        (transcoded-port output-port (car maybe-transcoder))))

  (define (read-string input-string input-start output-string output-start count)
    (if (> count 0)
        (let ([offset (- count 1)])
          (string-set! output-string
                       (+ output-start offset)
                       (string-ref input-string (+ input-start offset)))
          (read-string input-string input-start output-string output-start offset))))



  (define (open-string-input-port input-string)
    (define curr-pos 0)
    (define length (string-length input-string))
    (make-custom-textual-input-port
     input-string
     ;; read!
     (lambda (output-string start count)
       (let ([adjusted-count (min count (- length curr-pos))])
         (read-string input-string curr-pos output-string start adjusted-count)
         (set! curr-pos (+ curr-pos adjusted-count))
         adjusted-count))
     ;; get-position
     (lambda () curr-pos)
     ;; set-position
     (lambda (pos) (set! curr-pos pos))
     ;; close
     #f)))
