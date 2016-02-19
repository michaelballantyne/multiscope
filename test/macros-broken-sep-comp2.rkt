#lang racket/base

(require
  rackunit
  racket/string
  "macros-broken-sep-comp1.rkt")

(test-case
  "executing scope-manipulating macro outside of defining module should error"
  (check-exn
    (lambda (e)
      (and (exn:fail:syntax? e)
           (string-prefix? (exn-message e) "multiscope:")))
    (lambda ()
      (eval #'(let ([x 2])
                (macro x)))
      )))


(test-case
  "executing scope-manipulating phase 1 macro outside of defining module should error"
  (check-exn
    (lambda (e)
      (and (exn:fail:syntax? e)
           (string-prefix? (exn-message e) "multiscope:")))
    (lambda ()
      (eval #'(let-syntax ([m3 (macro2)])
                 (let ([x 2])
                   (m3 x))))
      )))
