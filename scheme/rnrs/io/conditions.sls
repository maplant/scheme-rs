(library (rnrs io conditions (6))
  (export &i/o make-i/o-error i/o-error?
          &i/o-read make-i/o-read-error i/o-read-error?
          &i/o-write make-i/o-write-error i/o-write-error?
          &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
          &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
          &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
          &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
          &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
          &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
          &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
          &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
          &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char)
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs records procedural (6))
          (rnrs conditions (6)))

  (define-syntax from-builtin
    (lambda (x)
      (syntax-case x ()
        [(_ name constructor predicate builtin)
         #'(begin
             (define condition-rtd (builtin))
             (define condition-rcd (make-record-constructor-descriptor condition-rtd #f #f))
             (define constructor (record-constructor condition-rcd))
             (define predicate (condition-predicate condition-rtd))
             (define-syntax name
               (lambda (x)
                 (syntax-case x (rtd rcd)
                   [(_ rtd) #'condition-rtd]
                   [(_ rcd) #'condition-rcd]))))])))

  (from-builtin &i/o make-i/o-error i/o-error? &i/o-rtd)
  (from-builtin &i/o-read make-i/o-read-error i/o-read-error? &i/o-read-rtd)
  (from-builtin &i/o-write make-i/o-write-error i/o-write-error? &i/o-write-rtd)
  (from-builtin &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? &i/o-invalid-position-rtd)
  (define i/o-error-position (condition-accessor (&i/o-invalid-position-rtd) (record-accessor (&i/o-invalid-position-rtd) 0)))
  (from-builtin &i/o-filename make-i/o-filename-error i/o-filename-error? &i/o-filename-rtd)
  (define i/o-error-filename (condition-accessor (&i/o-filename-rtd) (record-accessor (&i/o-filename-rtd) 0)))
  (from-builtin &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error? &i/o-file-protection-rtd)
  (from-builtin &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error? &i/o-file-is-read-only-rtd)
  (from-builtin &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error? &i/o-file-already-exists-rtd)
  (from-builtin &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error? &i/o-file-does-not-exist-rtd)
  (from-builtin &i/o-port make-i/o-port-error i/o-port-error? &i/o-port-rtd)
  (define i/o-error-port (condition-accessor (&i/o-port-rtd) (record-accessor (&i/o-port-rtd) 0)))
  (from-builtin &i/o-decoding make-i/o-decoding-error i/o-decoding-error? &i/o-decoding-rtd)
  (from-builtin &i/o-encoding make-i/o-encoding-error i/o-encoding-error? &i/o-encoding-rtd)
  (define i/o-encoding-error-char (condition-accessor (&i/o-encoding-rtd) (record-accessor (&i/o-encoding-rtd) 0))))
