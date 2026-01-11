(library (rnrs records syntactic (6))
  (export define-record-type record-constructor-descriptor record-type-descriptor)
  (import (rnrs base builtins (6))
          (rnrs base special-keywords (6))
          (rnrs syntax-case (6))
          (rnrs records procedural (6)))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (syntax (syntax-case (list e0 ...) ()
                   ((p ...) (let () e1 e2 ...))))))))

  (define-syntax record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ record-name) #'(record-name rtd)])))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ record-name) #'(record-name rcd)])))

  ;; Derived from the macro definition found in Ikarus by Abdulaziz Ghuloum.
  ;; The original macro is largely incomplete, so this extends and modifies that
  ;; heavily.

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

      ;; Fetch the record name from the name spec
      (define (get-record-name name-spec)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'record-name]
          [record-name #'record-name]))

      ;; Fetch the constructor name from the name spec
      (define (get-constructor-name name-spec ctxt)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'constructor-name]
          [record-name (id ctxt "make-" (syntax->datum #'record-name))]))

      ;; Fetch the predicate name from the name spec
      (define (get-predicate-name name-spec ctxt)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'predicate-name]
          [record-name (id ctxt (syntax->datum #'record-name) "?")]))

      ;; Get a specific clause from ls with a name id
      (define (get-clause id ls)
        (syntax-case ls ()
          [() #f]
          [((x . rest) . ls)
           (if (free-identifier=? id #'x) 
               #'(x . rest)
               (get-clause id #'ls))]))

      (define (field-specs-to-list x)
        (map (lambda (x) 
               (syntax->datum (syntax-case x (mutable immutable)
                                [(mutable name . rest) #'(mutable name . rest)]
                                [(immutable name . rest) #'(immutable name . rest)]
                                [name #'(immutable name)])))
             x))
      
      (define (field-specs-to-accessors record-name ctxt x)
        (with-syntax ([((accessor-name . k) ...) (field-specs-to-accessor-names (symbol->string (syntax->datum record-name)) ctxt x)]
                      [record-name record-name])
          #'(begin
              (define accessor-name (record-accessor (record-type-descriptor record-name) k)) ...)))

      (define (field-specs-to-accessor-names record-name ctxt x)
       (let loop ([x (field-specs-to-list x)] [fields '()] [offset 0])
         (if (null? x)
             fields
             (loop (cdr x)
                   (append fields (list (cons (field-spec-to-accessor-name record-name ctxt (cdr (car x))) offset)))
                   (+ offset 1)))))

      (define (field-spec-to-accessor-name record-name ctxt spec)
        (let ([accessor (cdr spec)])
          (datum->syntax
           ctxt
           (string->symbol
            (if (null? accessor)
                (string-append record-name "-" (symbol->string (car spec)))
                (symbol->string (car accessor)))))))

      (define (cdr-or-null x)
        (if (not (pair? x))
            '()
            (cdr x)))

      (define (field-specs-to-mutators record-name ctxt x)
        (with-syntax ([((mutator-name . k) ...) (field-specs-to-mutator-names (symbol->string (syntax->datum record-name)) ctxt x)]
                      [record-name record-name])
          #'(begin
              (define mutator-name (record-mutator (record-type-descriptor record-name) k)) ...)))

      (define (field-specs-to-mutator-names record-name ctxt x)
        (let loop ([x (field-specs-to-list x)] [fields '()] [offset 0])
          (if (null? x)
              fields
              (loop (cdr x)
                    (if (eqv? (car (car x)) 'mutable)
                        (append fields (list (cons (field-spec-to-mutator-name record-name ctxt (cdr (car x))) offset)))
                        fields)
                    (+ offset 1)))))

      (define (field-spec-to-mutator-name record-name ctxt spec)
        (let ([accessor (cdr-or-null (cdr spec))])
          (datum->syntax
           ctxt
           (string->symbol
            (if (null? accessor)
                (string-append record-name "-" (symbol->string (car spec)) "-set!")
                (symbol->string (car accessor)))))))

      ;; Construct the proper call to make-record-type-descriptor
      (define (rtd-code ctxt name clause*) 
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
                       (syntax-case (get-clause #'sealed clause*) ()
                         [(_ #t) #'#t]
                         [_      #'#f])]
                      [opaque?
                       (syntax-case (get-clause #'opaque clause*) ()
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

      ;; Construct the proper call to make-record-constructor-descriptor
      (define (rcd-code clause*) 
        (with-syntax ([parent-rcd-code 
                       (syntax-case (get-clause #'parent clause*) ()
                         [(_ name) #'(record-constructor-descriptor name)]
                         [_ #'#f])]
                      [protocol (get-protocol-code clause*)])
          #'(make-record-constructor-descriptor record-rtd
               parent-rcd-code protocol)))

      (define (get-protocol-code clause*)
        (syntax-case (get-clause #'protocol clause*) ()
          [(_ expr) #'expr]
          [_        #'#f]))

      (define (do-define-record ctxt name-spec clause*)
        (let ([name (get-record-name name-spec)])
          (with-syntax ([record-name (get-record-name name-spec)]
                        [record-constructor-name (get-constructor-name name-spec ctxt)] 
                        [record-predicate-name (get-predicate-name name-spec ctxt)]
                        [record-make-rtd (rtd-code ctxt name clause*)]
                        [record-make-rcd (rcd-code clause*)]
                        [record-accessor-defns (field-specs-to-accessors name ctxt (cdr (get-clause #'fields clause*)))]
                        [record-mutator-defns (field-specs-to-mutators name ctxt (cdr (get-clause #'fields clause*)))])
            #'(begin
                (define record-rtd record-make-rtd)
                (define record-rcd record-make-rcd)
                (define-syntax record-name
                  (lambda (x)
                    (syntax-case x (rtd rcd)
                      [(_ rtd) #'record-rtd]
                      [(_ rcd) #'record-rcd])))
                (define record-predicate-name (record-predicate record-rtd))
                (define record-constructor-name (record-constructor record-rcd))
                record-accessor-defns
                record-mutator-defns))))

      (syntax-case x ()
        [(ctxt name-spec clause* ...)
         (do-define-record #'ctxt #'name-spec #'(clause* ...))]))))
