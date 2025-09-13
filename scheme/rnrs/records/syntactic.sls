
(library (rnrs records syntactic (6))
  (export define-record-type)
  (import (rnrs lists (6))
          (rnrs base builtins (6))
          (rnrs base special-keywords (6))
          (rnrs syntax-case (6))
          (rnrs records procedural (6)))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (syntax (syntax-case (list e0 ...) ()
                   ((p ...) (let () e1 e2 ...))))))))

  (define-syntax define-record-type
    (lambda (x)
      (define (id ctxt . str*)
        (datum->syntax ctxt 
          (string->symbol 
            (apply string-append 
              (map (lambda (x) 
                     (if (symbol? x)
                         (symbol->string x)
                         (if (string? x) x)))
                   str*)))))
      (define (get-record-name spec)
        (syntax-case spec ()
          [(foo make-foo foo?) #'foo]
          [foo #'foo]))
      (define (get-record-constructor-name spec ctxt)
        (syntax-case spec ()
          [(foo make-foo foo?) #'make-foo]
          [foo (id ctxt "make-" (syntax->datum #'foo))]))
      (define (get-record-predicate-name spec ctxt)
        (syntax-case spec ()
          [(foo make-foo foo?) #'foo?]
          [foo (id ctxt (syntax->datum #'foo) "?")]))
      (define (get-clause id ls)
        (syntax-case ls ()
          [() #f]
          [((x . rest) . ls)
           (if (free-identifier=? id #'x) 
               #'(x . rest)
               (get-clause id #'ls))]))
      (define (foo-rtd-code ctxt name clause*) 
        (define (convert-field-spec* ls)
          (list #'quote
            (list->vector
              (map (lambda (x) 
                     (syntax-case x (mutable immutable)
                       [(mutable name . rest) #'(mutable name)]
                       [(immutable name . rest) #'(immutable name)]
                       [name #'(immutable name)]))
                   ls))))
        (with-syntax ([name name]
                      [parent-rtd-code 
                       (syntax-case (get-clause #'parent clause*) ()
                         [(_ name) #'(record-type-descriptor name)]
                         [_ #'#f])]
                      [uid-code 
                       (syntax-case (get-clause #'nongenerative clause*) ()
                         [(_) (datum->syntax ctxt (gensym))]
                         [(_ uid) #''uid]
                         [_ #'#f])]
                      [sealed?
                       (syntax-case (get-clause #'sealed? clause*) ()
                         [(_ #t) #'#t]
                         [_      #'#f])]
                      [opaque?
                       (syntax-case (get-clause #'opaque? clause*) ()
                         [(_ #t) #'#t]
                         [_      #'#f])]
                      [fields 
                       (syntax-case (get-clause #'fields clause*) ()
                         [(_ field-spec* ...) 
                          (convert-field-spec* #'(field-spec* ...))]
                         [_ #''#()])])
          #'(make-record-type-descriptor 'name
               parent-rtd-code 
               uid-code sealed? opaque? fields)))
      (define (foo-rcd-code clause*) 
        (with-syntax ([parent-rcd-code 
                       (syntax-case (get-clause #'parent clause*) ()
                         [(_ name) #'(record-constructor-descriptor name)]
                         [_ #'#f])])
          #'(make-record-constructor-descriptor foo-rtd
               parent-rcd-code protocol)))
      (define (get-protocol-code clause*)
        (syntax-case (get-clause #'protocol clause*) ()
          [(_ expr) #'expr]
          [_        #'#f]))
      (define (do-define-record ctxt namespec clause*)
        (let ([name (get-record-name namespec)])
          (with-syntax ([make-foo (get-record-constructor-name namespec ctxt)] 
                        [foo? (get-record-predicate-name namespec ctxt)]
                        [foo-rtd-code (foo-rtd-code ctxt name clause*)]
                        [protocol-code (get-protocol-code clause*)])
            #'(begin
                (define foo-rtd foo-rtd-code)
                (define protocol protocol-code)
                (define foo-rcd foo-rcd-code)
                (define-syntax foo (list '$rtd #'foo-rtd #'foo-rcd))
                (define foo? (record-predicate foo-rtd))
                (define make-foo (record-constructor foo-rcd))
                (define foo-x* (record-accessor foo-rtd idx*))
                ...
                (define set-foo-x!* (record-mutator foo-rtd mutable-idx*))
                ...))))
      (syntax-case x ()
        [(ctxt namespec clause* ...)
         (do-define-record #'ctxt #'namespec #'(clause* ...))]))))
