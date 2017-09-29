#lang racket/base

(require rackunit
         racket/string)

(namespace-require 'racket/base)

(eval '(module a multiscope
         (scopes
           [s1 racket])
         (define x 5)
         (provide (all-defined-out))))

(eval '(require 'a))

(check-equal? (eval 'x) 5)

(check-exn
  (lambda (e)
    (string-contains? (exn-message e) "s1: undefined"))
  (lambda ()
    (eval '(s1 x))) 5)
