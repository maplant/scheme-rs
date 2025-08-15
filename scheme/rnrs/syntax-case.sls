(library (rnrs syntax-case (6))
  (export quasisyntax
          (import (rnrs syntax-case builtins (6)))
          (import (rnrs syntax-case special-keywords (6))))
  (import (rnrs base builtins (6))
          (rnrs base special-keywords (6)))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (syntax (syntax-case (list e0 ...) ()
		   ((p ...) (let () e1 e2 ...))))))))

  (define (dbg label x)
    ;(display label)
    ;(display x)
    ;(display "\n")
    x)

  (define (flatten e)
    ; (dbg "Flatten: " e)
    (syntax-case e ()
      [((head1 head2) tail ...)
       (with-syntax (((flattened-tail ...) (flatten (syntax (tail ...)))))
         (syntax (head1 head2 flattened-tail ...)))]
      [()
       (syntax ())]))

  ;#`(1 2 #,@(list 3 4))

  ;; (define-syntax quasisyntax
  ;;   (lambda (e)
      
  ;;     ;; Expand returns a list of the form
  ;;     ;;    [template[t/e, ...] (replacement ...)]
  ;;     ;; Here template[t/e ...] denotes the original template
  ;;     ;; with unquoted expressions e replaced by fresh
  ;;     ;; variables t, followed by the appropriate ellipses
  ;;     ;; if e is also spliced.
  ;;     ;; The second part of the return value is the list of
  ;;     ;; replacements, each of the form (t e) if e is just
  ;;     ;; unquoted, or ((t ...) e) if e is also spliced.
  ;;     ;; This will be the list of bindings of the resulting
  ;;     ;; with-syntax expression.
      

  ;;     (define (expand x level)
  ;;       ;(display "Expanding: ");
  ;;       ;(display x)
  ;;       ;(display "\n")
  ;;       (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
  ;;         ((quasisyntax e)
  ;;          (with-syntax (((k _)     x) ;; original identifier must be copied
  ;;                        ((e* reps) (dbg "43: " (expand (syntax e) (+ level 1)))))
  ;;            (syntax ((k e*) reps))))                                  
  ;;         ((unsyntax e)
  ;;          (= level 0)
  ;;          (with-syntax (((t) (generate-temporaries '(t))))
  ;;            (syntax (t ((t e))))))
  ;;         (((unsyntax e ...) . r)
  ;;          (= level 0)
  ;;          (with-syntax (((r* (rep ...)) (dbg "51: " (expand (syntax r) 0)))
  ;;                        ((t ...)        (dbg "52: " (generate-temporaries
  ;;                                                     (syntax (e ...)))))
  ;;                        ((e* ...) (syntax (e ...))))
  ;;            (dbg "54: " (syntax ((t ... . r*)
  ;;                                 ((t e*) ... rep ...))))))
  ;;         (((unsyntax-splicing e ...) . r)
  ;;          (= level 0)
  ;;          (with-syntax (((r* (rep ...)) (dbg "58: " (expand (syntax r) 0)))
  ;;                        ((t ...)        (generate-temporaries
  ;;                                         (syntax (e ...))))
  ;;                        ((e* ...) (syntax (e ...))))
  ;;            (with-syntax (((t* ...) (dbg "now 72: " (flatten (syntax ((t ((... ...) (... ...))) ...)))))); ((t (... ...)) ...))))))
  ;;              (syntax ((t* ... . r*)
  ;;                       (((t ((... ...) (... ...))) e*) ... rep ...))))))
  ;;         ((k . r)
  ;;          (and (> level 0)
  ;;               (identifier? (syntax k))
  ;;               (or (free-identifier=? (syntax k) (syntax unsyntax))
  ;;                   (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
  ;;          (with-syntax (((r* reps) (dbg "69: " (expand (syntax r) (- level 1)))))
  ;;            (syntax ((k . r*) reps))))
  ;;         ((h . t)
  ;;          (with-syntax (((h* (rep1 ...)) (dbg "72: " (expand (syntax h) level)))
  ;;                        ((t* (rep2 ...)) (dbg "73: " (expand (syntax t) level))))
  ;;            (syntax ((h* . t*)
  ;;                     (rep1 ... rep2 ...)))))
  ;;         (#(e ...)
  ;;          (with-syntax ((((e* ...) reps)
  ;;                         (dbg "78: " (expand (vector->list (syntax #(e ...))) level))))
  ;;            (syntax (#(e* ...) reps))))
  ;;         (other
  ;;          (syntax (other ())))))
      
  ;;     (syntax-case e ()
  ;;       ((_ template)
  ;;        (with-syntax (((template* replacements) (dbg "85: " (expand (syntax template) 0))))
  ;;          (dbg "89: " (syntax
  ;;           (with-syntax replacements (syntax template*)))))))))
)
