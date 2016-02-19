#lang multiscope

(scopes
    [a racket/base (for-syntax racket/base syntax/parse)]
    [b racket/base racket/match])

(define-syntax macro
    (syntax-parser
      [(_ arg)
       (b
         (match 1
           [1 arg]))]))

(provide macro)
