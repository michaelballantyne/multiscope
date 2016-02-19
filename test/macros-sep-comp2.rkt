#lang racket/base

(require
  rackunit
  "macros-sep-comp1.rkt")

(test-case "reference passes through macro without scope modification"
           (check-equal?
             (let ([x 2])
               (macro x))
             2))
