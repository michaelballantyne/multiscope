#lang racket/base

(require
  (for-syntax racket/base syntax/parse)
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
      (set! expanding-module2? #t)))

  (define-syntax-class scope-spec
    (pattern [name:id require-specs ...])))

(define-syntax (module-begin stx)
  (syntax-parse stx #:datum-literals (scopes)
    [(_ (scopes
          default-scope-spec:scope-spec
          other-scope-spec:scope-spec ...)
        forms ...)
     #:with (all-scope-specs:scope-spec ...) #'(default-scope-spec other-scope-spec ...)
     
     (begin
       ; effect at phase 1
       (set! expanding-module1? #t)

       (with-syntax ([(scope-introducer ...) (generate-temporaries #'(all-scope-specs.name ...))]
                     ; hygiene-breaking introductions:
                     [#%top-interaction (datum->syntax stx '#%top-interaction)]
                     [subref (datum->syntax stx ''sub)]
                     ; We create the identifying syntax object scopes for each scope during the
                     ; module-begin exapansion rather than in the resulting module to
                     ; ensure they are the same for every instantiation of the module,
                     ; so that things like top-interaction apply the same scope and thus work.
                     [(scoped-ids ...) (map
                                         (lambda (id) ((make-syntax-introducer #t) id))
                                         (syntax->list #'(all-scope-specs.name ...)))])
         #`(#%module-begin
            (begin-for-syntax
              (let-syntax ([m (lambda (stx)
                                (set-expanding-module2!)
                                #'(void))])
                (m)))
            ; Use this submodule so that `(provide (all-defined-out))` in the final
            ; module doesn't export the scope application definitions.
            (module sub racket/base
              ; Use another submodule so we don't have to repeat these definitions
              ; for the two phases.
              (module sub racket/base
                ; The scope created during the expansion is transferred to the expanded
                ; module through a syntax object having the scope and a paired syntax object
                ; without it. At module instantiation an introducer is reconstructed
                ; from the difference between these syntax objects.
                (define scope-introducer
                  (make-syntax-delta-introducer
                    #'scoped-ids
                    #'all-scope-specs.name))
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

              (require
                ; For later reference to `expanding-module1?` and `expanding-module2?`. Not needed for the
                ; binding (because those references are macro-inserted), but needed to ensure the `multiscope`
                ; module has been loaded into the namespace. (Hence the empty identifier list in `only-in`.
                (only-in multiscope)
                ; Use `rename-in` to make sure the `scope-introducer` bindings have the right scope to capture
                ; inserted references in this module. (because generate-temporaries makes identifiers with scopes
                ; different from those normally on macro-introduced identifiers)
                (for-syntax racket/base racket/list
                            (rename-in (submod "." sub) [scope-introducer scope-introducer] ...))
                ; for quasisyntax-like phase 1 macro
                (for-meta 2 racket/base
                          (rename-in (submod "." sub) [scope-introducer scope-introducer] ...)))

              ; phase 0 macro; produces a scoped version of the argument syntax.
              (define-syntax (all-scope-specs.name stx)
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
                    (syntax-case stx (all-scope-specs.name ...)
                      [(all-scope-specs.name body)
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

                (define-syntax (all-scope-specs.name stx)
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
                all-scope-specs.name ...
                (for-syntax all-scope-specs.name ...)))

            (require subref)

            ; Applying the appropriate scope macro to the require-spec causes
            ; the imported identifier to have that scope (and thus only be referenced
            ; when the referring identifier also has the scope). We also happen to apply
            ; the scope to `require` itself, but this has no impact as require is
            ; a reference and resolution is by scope subset.
            (all-scope-specs.name (require all-scope-specs.require-specs ...)) ...

            ; Forms in the remainder of the module body and entered at the REPL are
            ; resolved in the default scope.

            (define-syntax (#%top-interaction stx)
              (set-expanding-module1!)
              (syntax-case stx ()
                [(_ . form)
                 #'(default-scope-spec.name form)]))

            (default-scope-spec.name forms) ...)))]))

(module reader syntax/module-reader
  #:language 'multiscope)

