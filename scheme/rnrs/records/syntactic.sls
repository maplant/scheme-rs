(library (rnrs records syntactic (6))
  (export define-record-type record-constructor-descriptor record-type-descriptor)
  (import (rnrs values (6))
          (rnrs base builtins (6))
          (rnrs base primitives (6))
          (rnrs syntax-case (6))
          (rnrs io simple builtins)
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
  ;;
  ;; TODO: Lots of error checking

  (define-syntax define-record-type
    (lambda (x)
      ;; Get a specific clause from a list
      (define (get-clause id ls)
        (syntax-case ls ()
          [() '()]
          [((x . rest) . ls)
           (if (free-identifier=? id #'x) 
               #'rest
               (get-clause id #'ls))]))

      (define (get-parent-rtd ls)
        (syntax-case ls (parent-rtd parent)
          [() #'#f]
          [((parent parent-name) . ls)
           #'(record-type-descriptor parent-name)]
          [((parent-rtd rtd rcd) . ls)
           #'rtd]
          [(_ . ls)
           (get-parent-rtd #'ls)]))

      (define (get-parent-rcd ls)
        (syntax-case ls (parent-rtd parent)
          [() #'#f]
          [((parent parent-name) . ls)
           #'(record-constructor-descriptor parent-name)]
          [((parent-rtd rtd rcd) . ls)
           #'rcd]
          [(_ . ls)
           (get-parent-rtd #'ls)]))

      (define (get-single-value-clause id ls)
        (syntax-case ls ()
          [() #'#f]
          [((clause . value) . ls)
           (if (free-identifier=? id #'clause)
               (syntax-case #'value ()
                 ((value) #'value))
               (get-single-value-clause id #'ls))]))

      (define (map-single-value-clause id ls proc)
        (syntax-case ls ()
          [() #'#f]
          [((clause . value) . ls)
           (if (free-identifier=? id #'clause)
               (syntax-case #'value ()
                 (() (apply proc (list '())))
                 ((value) (apply proc #'value)))
               (map-single-value-clause id #'ls proc))]))

      ;; Fetch the record name from the name spec
      (define (get-record-name name-spec)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'record-name]
          [record-name #'record-name]))
      
      ;; Fetch the constructor name from the name spec
      (define (get-constructor-name name-spec ctxt)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'constructor-name]
          [record-name (datum->syntax ctxt (string->symbol (string-append "make-" (syntax->string #'record-name))))]))

      ;; Fetch the predicate name from the name spec
      (define (get-predicate-name name-spec ctxt)
        (syntax-case name-spec ()
          [(record-name constructor-name predicate-name) #'predicate-name]
          [record-name (datum->syntax ctxt (string->symbol (string-append (syntax->string #'record-name) "?")))]))

      (define (normalize-field-specs field-specs)
        (if (null? field-specs)
            '()
            (syntax-case field-specs (mutable immutable)
              [((mutable name . rest) . remaining)
               (cons #'(mutable name . rest) (normalize-field-specs #'remaining))]
              [((immutable name . rest) . remaining)
               (cons #'(immutable name . rest) (normalize-field-specs #'remaining))]
              [(name . remaining) (cons #'(immutable name) (normalize-field-specs #'remaining))])))

      (define (syntax->string syntax)
        (symbol->string (syntax->datum syntax)))
      
      (define (default-accessor-name ctxt record-name field)
        (datum->syntax ctxt (string->symbol (string-append (syntax->string record-name) "-" (syntax->string field)))))

      (define (default-mutator-name ctxt record-name field)
        (datum->syntax ctxt (string->symbol (string-append (syntax->string record-name) "-" (syntax->string field) "-set!"))))

      (define (field-spec-to-accessor ctxt record-name field-spec)
        (syntax-case field-spec (mutable immutable)
          [(immutable field-name) (default-accessor-name ctxt record-name #'field-name)]
          [(immutable field-name accessor) #'accessor]
          [(mutable field-name) (default-accessor-name ctxt record-name #'field-name)]
          [(mutable field-name accessor _) #'accessor]
          [field-name (default-accessor-name ctxt record-name #'field-name)]))

      (define (normalized-field-specs-to-accessors ctxt record-name field-specs)
        (let loop ([k 0]
                   [accessors '()]
                   [field-specs field-specs])
          (if (null? field-specs)
              accessors
              (with-syntax ([sk k]
                            [accessor-name (field-spec-to-accessor ctxt record-name (car field-specs))]
                            [record-name record-name])
                (let [(accessor #'(define accessor-name (record-accessor (record-type-descriptor record-name) sk)))]
                  (loop (+ k 1) (cons accessor accessors) (cdr field-specs)))))))

      (define (field-spec-to-mutator ctxt record-name field-spec)
        (syntax-case field-spec (mutable immutable)
          [(immutable _) '()]
          [(immutable _ _) '()]
          [(mutable field-name) (default-mutator-name ctxt record-name #'field-name)]
          [(mutable field-name _ mutator) #'mutator]
          [field-name '()]))

      (define (normalized-field-specs-to-mutators ctxt record-name field-specs)
        (let loop ([k 0]
                   [mutators '()]
                   [field-specs field-specs])
          (if (null? field-specs)
              mutators
              (let ([mutator-name (field-spec-to-mutator ctxt record-name (car field-specs))])
                (if (null? mutator-name)
                    (loop (+ k 1) mutators (cdr field-specs))
                    (with-syntax ([sk k]
                                  [mutator-name mutator-name]
                                  [record-name record-name])
                      (let [(mutator #'(define mutator-name (record-mutator (record-type-descriptor record-name) sk)))]
                        (loop (+ k 1) (cons mutator mutators) (cdr field-specs)))))))))

      ;; Construct the proper call to make-record-type-descriptor
      (define (make-rtd ctxt name field-specs clause*) 
        (with-syntax ([name name]
                      [parent-rtd (get-parent-rtd clause*)]
                      [uid-code (map-single-value-clause
                                 #'nongenerative clause*
                                 (lambda (clause)
                                   (syntax-case clause ()
                                     [() (with-syntax ((uid (gensym))) #''uid)]
                                     [uid #''uid])))]
                      [sealed? (get-single-value-clause #'sealed clause*)]
                      [opaque? (get-single-value-clause #'opaque clause*)]
                      [fields (list->vector (syntax->datum field-specs))])
          #'(make-record-type-descriptor 'name
               parent-rtd
               uid-code sealed? opaque? 'fields)))

      ;; Construct the proper call to make-record-constructor-descriptor
      (define (make-rcd temp-rtd clause*) 
        (with-syntax ([parent-rcd (get-parent-rcd clause*)]
                      [protocol (get-single-value-clause #'protocol clause*)]
                      [record-rtd temp-rtd])
          #'(make-record-constructor-descriptor
             record-rtd parent-rcd protocol)))

      (define (validate-clause* clause* fields? parent? protocol? sealed? opaque? nongenerative? parent-rtd?)
        (syntax-case clause* ()
          [() (values)]
           [((field-clause . rest) . ls)
            (free-identifier=? #'field-clause #'fields)
            (if fields?
                (syntax-violation #f "multiple `fields` clauses specified" #'field-clause)
                (validate-clause* #'ls #t parent? protocol? sealed? opaque? nongenerative? parent-rtd?))]
           [((parent-clause _) . ls)
            (free-identifier=? #'parent-clause #'parent)
            (if (or parent? parent-rtd?)
                (syntax-violation #f "multiple `parent` or `parent-rtd` clauses specified" #'parent-clause)
                (validate-clause* #'ls fields? #t protocol? sealed? opaque? nongenerative? parent-rtd?))]
           [((protocol-clause expr) . ls)
            (free-identifier=? #'protocol-clause #'protocol)
            (if protocol?
                (syntax-violation #f "multiple `protocol` clauses specified" #'protocol-clause)
                (validate-clause* #'ls fields? parent? #t sealed? opaque? nongenerative? parent-rtd?))]
           [((sealed-clause val) . ls)
            (free-identifier=? #'sealed-clause #'sealed)
            (if sealed?
                (syntax-violation #f "multiple `sealed` clauses specified" #'sealed-clause)
                (validate-clause* #'ls fields? parent? protocol? #t opaque? nongenerative? parent-rtd?))]
           [((opaque-clause val) . ls)
            (free-identifier=? #'opaque-clause #'opaque)
            (if opaque?
                (syntax-violation #f "multiple `opaque` clauses specified" #'opaque-clause)
                (validate-clause* #'ls fields? parent? protocol? sealed? #t nongenerative? parent-rtd?))]
           [((nongenerative-clause . vals) . ls)
            (free-identifier=? #'nongenerative-clause #'nongenerative)
            (if nongenerative?
                (syntax-violation #f "multiple `nongenerative` clauses specified" #'nongenerative-clause)
                (validate-clause* #'ls fields? parent? protocol? sealed? opaque? #t parent-rtd?))]
           [((parent-rtd-clause rtd cd) . ls)
            (free-identifier=? #'parent-rtd-clause #'parent-rtd)
            (if (or parent-rtd? parent?)
                (syntax-violation #f "multiple `parent-rtd` or `parent` clauses specified" #'parent-rtd)
                (validate-clause* #'ls fields? parent? protocol? sealed? opaque? nongenerative? #t))]
           [(invalid-clause . ls)
            (syntax-violation #f "invalid clause" #'invalid-clause)]))

      (define (define-record ctxt name-spec clause*)
        (validate-clause* clause* #f #f #f #f #f #f #f)
        (let ([name (get-record-name name-spec)]
              [field-specs (normalize-field-specs (get-clause #'fields clause*))]
              [temp-rtd #'record-rtd])
          (with-syntax ([record-name name]
                        [record-constructor-name (get-constructor-name name-spec ctxt)]
                        [record-predicate-name (get-predicate-name name-spec ctxt)]
                        [record-make-rtd (make-rtd ctxt name field-specs clause*)]
                        [record-make-rcd (make-rcd temp-rtd clause*)]
                        [(record-accessors ...) (normalized-field-specs-to-accessors ctxt name field-specs)]
                        [(record-mutators ...) (normalized-field-specs-to-mutators ctxt name field-specs)]
                        [record-rtd temp-rtd])
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
                record-accessors ...
                record-mutators ...))))

      (syntax-case x ()
        [(ctxt name-spec clause* ...)
         (define-record #'ctxt #'name-spec #'(clause* ...))])
      )))
