#lang racket/base

(require
  (for-syntax racket/base syntax/parse)
  (for-meta 2 racket/base syntax/parse syntax/id-table))

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
    (set! expanding-module1? #t)))

(begin-for-syntax
  (begin-for-syntax
    (define expanding-module2? #f)
    (define (set-expanding-module2!)
      (set! expanding-module2? #t))))


(module multi-phase-helpers racket
  (define-syntax-rule
    (transient-effect statements ...)
    (begin
      (define-syntax (m stx)
        (begin
          statements ...)
        #'(void))
      (m)))

  (define-syntax-rule
    (define-syntax/no-provide var expr)
    (begin
      (define-syntax tmp expr)
      (define-syntax var
        (make-rename-transformer
          (syntax-property #'tmp
                           'not-provide-all-defined
                           #t)))))

  (define-for-syntax (apply-scope this-introducer all-introducers stx)
    (this-introducer
      (for/fold ([stx stx])
                ([introducer all-introducers])
                (introducer stx 'remove))
      'add))

  (provide transient-effect
           define-syntax/no-provide
           (for-syntax apply-scope)))

(require 'multi-phase-helpers
         (for-syntax 'multi-phase-helpers))


(begin-for-syntax
  (define (make-phase0-scope-macro this-introducer other-introducers)
    (lambda (stx)
      (syntax-parse stx
        [(_ body ...)
         (when (not expanding-module1?)
           (raise-syntax-error
             'multiscope
             "scope-application macro used outside of defining module (likely through an exported macro). This is unsupported. Use the phase 1 syntax-quote-like macros instead."
             stx))
         ; don't add any scopes to begin; it needs to resolve
         ; to the definition available to the macro (not the use-site)
         ; lest the reference be ambiguous.
         (define/syntax-parse (scoped-body ...)
           (apply-scope this-introducer
                        other-introducers
                        #'(body ...)))

         #'(begin scoped-body ...)])))

  (begin-for-syntax
    (define (make-phase1-scope-macro scope-ids this-introducer other-introducers)
      (define table
        (make-immutable-free-id-table
          ; Not sure why, but this only works if #:phase is 0.
          ; Seems like it should be 1, as we'd want to change
          ; behavior if the phase 1 macro had been shadowed.
          (map cons scope-ids other-introducers) #:phase 0))

      (define (scoped-syntax this-introducer stx)
        (syntax-parse stx
          [(scope body)
           #:when (and (identifier? #'scope)
                       (free-id-table-ref table #'scope #f))
           (scoped-syntax
             (free-id-table-ref table #'scope)
             #'body)]
          [(list-element ...)
           (datum->syntax
             #'(list-element ...)
             (for/list ([el (syntax->list #'(list-element ...))])
               (scoped-syntax this-introducer el)))]
          [pattern-var
           #:when (and (identifier? #'pattern-var)
                       (syntax-pattern-variable?
                         (syntax-local-value #'pattern-var (lambda () #f))))
           #'pattern-var]
          [other
           (apply-scope this-introducer other-introducers #'other)]))

      (syntax-parser
        [(_ body ...)
         (when (not expanding-module2?)
           (raise-syntax-error 'multiscope "scoped syntax-quoting phase 1 macro used outside of defining module (likely through an exported macro). This is unsupported. Apply scopes at a higher phase but within the originating module instead."))

         (with-syntax ([(scoped-body ...) (scoped-syntax this-introducer #'(body ...))])
           #'(syntax scoped-body ...))]))))


(define-syntax (define-scopes stx)
  (syntax-parse stx
    [(_ scope ...)

     (define/syntax-parse (scope-introducer ...) (generate-temporaries #'(scope ...)))

     ; We create the identifying syntax object scopes for each scope during the
     ; define-scopes exapansion rather than in the resulting module to
     ; ensure they are the same for every instantiation of the module,
     ; so that things like top-interaction apply the same scope and thus work.
     (define/syntax-parse (scoped-ids ...)
       (for/list ([id (syntax->list #'(scope ...))])
         ((make-syntax-introducer #t) id)))

     #'(begin
           ; For phase 0 macro
           (begin-for-syntax
             (define scope-introducer
               (make-syntax-delta-introducer
                 #'scoped-ids
                 #'scope))
             ...)

           ; For phase 1 syntax quoting macro
           (begin-for-syntax
             (begin-for-syntax
               (define scope-introducer
                 (make-syntax-delta-introducer
                   #'scoped-ids
                   #'scope))
               ...
           ))

           ; phase 0 macro; produces a scoped version of the argument syntax.
           (define-syntax/no-provide scope
             (make-phase0-scope-macro scope-introducer (list scope-introducer ...)))
           ...

           ; phase 1 macro; quasisyntax-like.
           (begin-for-syntax
             (define-syntax/no-provide scope
               (make-phase1-scope-macro (list #'scope ...)
                                        scope-introducer
                                        (list scope-introducer ...)))
             ...))]))


(begin-for-syntax
  (define-syntax-class scope-spec
    (pattern [name:id require-specs ...]))

  (define (make-top-interaction default-scope)
    (lambda (stx)
      (set-expanding-module1!)
      (syntax-case stx ()
        [(_ . form)
         #`(#,default-scope form)]))))


(define-syntax (module-begin stx)
  (syntax-parse stx #:datum-literals (scopes)
    [(_ (scopes
          default-scope:scope-spec
          other-scope:scope-spec ...)
        forms ...)
     #:with (scope:scope-spec ...) #'(default-scope other-scope ...)
     #:with #%top-interaction (datum->syntax stx '#%top-interaction)

     #'(#%module-begin
        (transient-effect
          (set-expanding-module1!))

        (begin-for-syntax
          (transient-effect
            (set-expanding-module2!)))

        (define-scopes scope.name ...)

        ; Applying the appropriate scope macro to the require-spec causes
        ; the imported identifier to have that scope (and thus only be referenced
        ; when the referring identifier also has the scope). We also happen to apply
        ; the scope to `require` itself, but this has no impact as require is
        ; a reference and resolution is by scope subset.
        (scope.name (require scope.require-specs ...)) ...

        ; Forms in the remainder of the module body and entered at the REPL are
        ; resolved in the default scope.

        (define-syntax #%top-interaction (make-top-interaction #'default-scope.name))

        (default-scope.name forms) ...)]))


(module reader syntax/module-reader
  #:language 'multiscope)
