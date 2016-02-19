#lang multiscope

(scopes
  [rkt racket/base rackunit]
  [a racket/base]
  [b])

(module a racket/base
  (define x 'a)
  (provide x))

(a (require 'a))

(module b racket/base
  (define x 'b)
  (provide x))

(b (require 'b))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(test-case "racket/base is visible in `rkt` and `a` but not `b`"
  (check-equal? (list 1 2 3) '(1 2 3))
  (check-equal? (a (list 1 2 3)) '(1 2 3))
  (check-exn
    exn:fail?
    (lambda ()
      (eval #'(b (list 1 2 3)) ns))))

(test-case "x is defined differently in `a` and `b`, and is not defined in `rkt`"
  (check-equal? (a x) 'a)
  (check-equal? (b x) 'b)
  (check-exn
    exn:fail?
    (lambda ()
      (eval #'x ns))))

(test-case "shadowing shadows in the scope of the binding, but no other"
  (check-equal?
    (a (let ([x 'a2])
         (list
           x
           (b x))))
    '(a2 b)))

(test-case "splicing behavior"
  (check-equal?
    (let ()
      (define y 'y-rkt)
      (a (define y 'y-a))
      (a
        (define x 'x-a)
        (list
          x
          y)))
    '(x-a y-a)))

