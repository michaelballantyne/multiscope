#lang racket/base

(module m1 multiscope
  (scopes
    [a racket/base rackunit (for-syntax racket/base syntax/parse)]
    [b racket/base racket/match])


  (test-case "use binding from one scope in a binding macro for another"
    (define-syntax macro1
      (syntax-parser
        [(_ binder body)
         (b
           (match 1
             [binder body]))]))

    (check-equal?
      (macro1 x x)
      1))

  (define x 1)

  (define-syntax macro2
    (syntax-parser
      [(_ arg)
       (b
         (match (a x)
           [1 arg]))]))

  (provide macro2)

  (module* m2 racket/base
    (require
      rackunit
      (submod ".."))

    (test-case "reference passes through macro without scope modification"
      (check-equal?
        (let ([x 2])
          (macro2 x))
        2))))

(require (submod "." m1 m2))
