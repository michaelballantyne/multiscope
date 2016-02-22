A module language for programming with multiple named scopes, orthogonal to lexical nesting.

## Examples

### Code Generation

```
#lang multiscope

(scopes
  [rkt racket/base]
  [c (only-in racket/base define)])

(c
 (define +
   (rkt
     (lambda (lhs rhs)
       (format "(~s + ~s)" lhs rhs)))))
       
(displayln (c (+ 5 (rkt (+ 2 3)))))
; => (5 + 5)
```

Code generation functions can use the same names as forms from Racket, and staged computations can intermix references to them by entering named scopes.

### Relational programming in miniKanren

```
#lang multiscope

(scopes
  [rkt racket rackunit]
  [mk "mk/mk.rkt" (only-in racket/base define quasiquote unquote)])

(mk
  (define (append l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (append d s res))])))

(let ([l1 '(a b)]
      [l2 '(c d e)])
  (check-equal?
    (first
      (mk (run 1 (q) (append (rkt l1) (rkt l2) q))))
    (append l1 l2)))
```

Relational miniKanren operators and functional froms from racket are kept in separate namespaces, allowing relations to use the same names as their functional counterparts (like `append`). Accidental reference to a non-relational operator from Racket will lead to an unbound identifier error.


## Why?

It is often convenient for forms of an embedded DSL to use some of the same identifiers as forms from racket/base (or another relevant language), but wrangling the names in a module that needs to use both the DSL and the built-in forms can be awkward.

Method overloading in object-oriented languages and typeclass based function overloading in typed functional languages partially avoid these problems, but dynamic functional languages need another solution.

Code that intermixes forms from different languages

## Usage

Modules in the `multiscope` language start with the `scopes` form, declaring the names of scopes and the require-specs for their initial imports. The first declared scope is the default scope, and is applied to all forms in the remaining body of the module and to forms entered in DrRacket's interaction pane.

```
(scopes
  [name require-spec ...]
  [name require-spec ...]
  ...)
```

Within the body of the module, the scope names are bound (at phase 0) to macros that cause their argument to be evaluated within that named scope. The scope-applying macros are visible in every named scope. When scope-applications are nested, the innermost scope applies. Other than their scoping effects, the scope-application macros have the same behavior as `begin`.

### Macros

Because of implementation limitations, don't write macros that expand to uses of the scope application macros, like this:

```
#lang multiscope

(scopes
  [a racket/base (for-syntax racket/base)]
  [b racket/match])
  
(define-syntax (m stx)
  (syntax-case stx ()
    [(_ arg)
     #'(b (match 1
            [1 arg]))]))

(provide m)
```

There isn't a good way to make these work across separate compilation boundaries. To avoid confusing behaviors, the current implementation explicitly produces an error when a scope-application macro is applied outside of the dynamic extent of the expansion of the module where the scope is defined.

Instead, `#lang multiscope` provides forms resembling the `syntax` syntax-quoting form that apply scopes to quoted syntax objects. These are bound to the scope names at phase 1.

```
#lang multiscope

(scopes
  [a racket/base (for-syntax racket/base)]
  [b racket/match])
  
(define-syntax (m stx)
  (syntax-case stx ()
    [(_ arg)
     (b (match 1
          [1 arg]))]))

(provide m)
```

In the above example `match` and the `#%datum` form for the numeric literals will be resolved in the `b` scope. The value of `arg` will be resolved in its original scope at the use site.

The phase 1 macros also recognize the identifiers of other scopes, allowing nesting. The behavior is a bit different than the phase 0 scope application macros. Scope applications are recognized and applied in any position, rather than just expression position. Thus the previous example could also be written:

```
#lang multiscope

(scopes
  [a racket/base (for-syntax racket/base)]
  [b racket/match])
  
(define-syntax (m stx)
  (syntax-case stx ()
    [(_ arg)
     (a ((b match) 1
          [1 arg]))]))

(provide m)
```

## Implementation Concepts

The model of scopes from Racket's new scope-set based expander ([[www.cs.utah.edu/plt/scope-sets/]]) underlies the implementation. Each named scope is implemented by a scope-sets scope object in the macro expander (via  `make-syntax-introducer`). The scope-applying macro for a given named scope applies its scope object and removes the scope object for the other named scopes from the syntax of its argument.

## Caveats

DrRacket's binding arrows will point to every location where an identifier was imported via require or the initial bindings of a scope, even those for other, incorrect scopes.

There is currently no scoping form that behaves like quasiquote, so some macros may be inconvenient to write.
