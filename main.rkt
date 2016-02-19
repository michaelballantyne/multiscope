#lang racket/base

(require
  (for-syntax racket/base)
  (for-meta 2 racket/base))

; because we want to add scopes to absolutely everything we import,
; we don't provide any initial bindings through the module language
; other than #%module-begin and forms for require. The forms for require
; will unfortunately be accessible to all the scopes, but may be overridden.
(provide
 (rename-out [module-begin #%module-begin])
 #%top-interaction
 #%top #%app #%datum
 ; for require
 require only-in except-in prefix-in rename-in combine-in relative-in only-meta-in
 for-syntax for-template for-label for-meta submod quote lib file planet)

(begin-for-syntax
  (define expanding-module1? #f)
  (define (set-expanding-module1!)
      (set! expanding-module1? #t))
  (begin-for-syntax
    (define expanding-module2? #f)
    (define (set-expanding-module2!)
      (set! expanding-module2? #t))))

(define-syntax (module-begin stx)
  (syntax-case stx (scopes default)
    [(_ (scopes
          [default-scope default-require-specs ...]
          [scope scope-require-specs ...] ...)
        forms ...)

     (let ()
       ; effect at phase 1
       (set! expanding-module1? #t)

       (with-syntax ([(all-scopes ...) #'(default-scope scope ...)]
                     [((all-require-specs ...) ...) #'((default-require-specs ...)
                                                       (scope-require-specs ...) ...)])
         (with-syntax ([(scope-introducer ...) (map syntax->datum (generate-temporaries #'(all-scopes ...)))]
                       ; hygiene-breaking introductions:
                       [#%top-interaction (datum->syntax stx '#%top-interaction)]
                       [require (datum->syntax stx 'require)]
                       [subref (datum->syntax stx ''sub)]
                       [require-intro #'require]
                       [(all-scopes-intro ...) (syntax-local-introduce #'(all-scopes ...))]
                       ; We create the identifying syntax object scopes for each scope during the
                       ; module-begin exapansion rather than in the resulting module to
                       ; ensure they are the same for every instantiation of the module,
                       ; so that things like top-interaction apply the same scope and thus work.
                       [(scoped-ids ...) (map
                                           (lambda (id) ((make-syntax-introducer) id))
                                           (syntax->list #'(all-scopes ...)))])
           #`(#%module-begin
              (begin-for-syntax
                (let-syntax ([m (lambda (stx)
                                  (set-expanding-module2!)
                                  #'5)])
                  (m)))
              (module sub racket/base
                (module sub racket/base
                  ; The scope created during the expansion is transferred to the expanded
                  ; module through a syntax object having the scope and a paired syntax object
                  ; without it. At module instantiation an introducer is reconstructed
                  ; from the difference between these syntax objects.
                  (define scope-introducer
                    (make-syntax-delta-introducer
                      #'scoped-ids
                      #'all-scopes))
                  ...

                  ; Adds the syntax object scope for this scope and removes the syntax object
                  ; scopes for all other scopes, ensuring that the accessible identifiers within
                  ; each scope are distinct.
                  (define (apply-scope this-introducer all-introducers stx)
                    (this-introducer
                      (for/fold ([stx stx])
                                ([introducer all-introducers])
                                (introducer stx 'remove))
                      'add))

                  (provide scope-introducer ...
                           apply-scope))

                (require-intro
                  (only-in multiscope)
                  (for-syntax racket/base racket/list (submod "." sub))
                  (for-meta 2 racket/base (submod "." sub)) ; for quasisyntax-like phase 1 macro
                  )

                ; phase 0 macro; produces a scoped version of the argument syntax.
                (define-syntax (all-scopes-intro stx)
                  (syntax-case stx ()
                    [(_ body (... ...))
                     (when (not expanding-module1?)
                       (raise-syntax-error
                         'multiscope
                         "scope-application macro used outside of defining module (likely through an exported macro). This is unsupported. Use the phase 1 syntax-quote-like macros instead."
                         stx))
                     ; don't add any scopes to begin; it needs to resolve
                     ; to the definition available to the macro (not the use-site)
                     ; lest the reference be ambiguous.
                     #`(begin
                         #,@(apply-scope
                              scope-introducer
                              (list scope-introducer ...)
                              #'(body (... ...))))
                     ]))
                ...

                ; phase 1 macro; quasisyntax-like.
                (begin-for-syntax
                  (begin-for-syntax
                    (define (scoped-syntax this-introducer stx)
                      (syntax-case stx (all-scopes-intro ...)
                        [(all-scopes-intro body)
                         (scoped-syntax
                           scope-introducer
                           #'body)]
                        ...
                        [(els (... ...))
                         (datum->syntax
                           #'(els (... ...))
                           (map
                             (lambda (arg) (scoped-syntax this-introducer arg))
                             (syntax->list #'(els (... ...))))
                           )]
                        [el
                          (if (and (identifier? #'el) (syntax-pattern-variable? (syntax-local-value #'el (lambda () 5))))
                            #'el
                            (apply-scope this-introducer (list scope-introducer ...) #'el))]
                        )))

                  (define-syntax (all-scopes-intro stx)
                    (syntax-case stx ()
                      [(_ body (... ...))
                       (when (not expanding-module2?)
                         (raise-syntax-error 'multiscope "scoped syntax-quoting phase 1 macro used outside of defining module (likely through an exported macro). This is unsupported. Apply scopes at a higher phase but within the originating module instead."))
                       #`(syntax #,@(scoped-syntax
                                      scope-introducer
                                      #'(body (... ...))))]))
                  ...
                  )

                (provide
                  all-scopes-intro ...
                  (for-syntax all-scopes-intro ...)
                  ))

              (require subref)

              ; Applying the appropriate scope macro to the require-spec causes
              ; the imported identifier to have that scope (and thus only be referenced
              ; when the referring identifier also has the scope). We also happen to apply
              ; the scope to `require` itself, but this has no impact as require is
              ; a reference and resolution is by scope subset.
              (all-scopes (require all-require-specs ...)) ...

              ; Forms in the remainder of the module body and entered at the REPL are
              ; resolved in the default scope.

              (define-syntax (#%top-interaction stx)
                (set-expanding-module1!)
                (syntax-case stx ()
                  [(_ . form)
                   #'(default-scope form)]))

              (default-scope forms) ...))))]))

(module reader syntax/module-reader
  #:language 'multiscope)

