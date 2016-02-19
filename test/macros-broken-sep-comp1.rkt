#lang multiscope

(scopes
    [a racket/base
       (for-syntax racket/base syntax/parse)
       (for-meta 2 racket/base syntax/parse)]
    [b racket/base racket/match])

(define-syntax macro
    (syntax-parser
      [(_ arg)
       #'(b
           (match 1
             [1 arg]))]))

(begin-for-syntax
  (define-syntax macro2
    (syntax-parser
      [(_) #'(syntax-parser
                     [(_ arg)
                      (b
                        (match 1
                          [1 arg]))])])))

(define-syntax macro3 (macro2))

(provide macro (for-syntax macro2))
