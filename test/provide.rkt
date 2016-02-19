#lang racket/base

(module a multiscope
  (scopes
    [a racket/base]
    [b racket/base])

  (define x 'a)
  (b (define x 'b))
  (a (provide x))

  (define y 'a)
  (b (define y 'b))
  (b (provide y)))

(module b racket/base
  (require
    rackunit
    (submod ".." a))

  (test-case "provide should export the binding from the current scope"
    (check-equal?
      (list x y)
      '(a b))))

(require 'b)

(module c multiscope
  (scopes
    [a racket/base]
    [b racket/base])

  (b
    (define x 'b)
    (provide x))

  (module* d racket/base
    (require
      rackunit
      (submod ".."))

    (test-case "require/provide to submodule in default scope should allow access to definition in different scope"
      (check-equal?
        x
        'b))))

(require (submod "." c d))
