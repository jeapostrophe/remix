#lang racket/base

(define-signature ^eq
  ==)

(define-unit (@eq-symbol)
  (export ^eq)
  (define == symbol=?))


(define-unit
  (@eq-pair
   [(^prefix a: ^eq) (@eq-symbol)]
   [(^prefix b: ^eq) (@eq-symbol)])

  (define (== x y)
    (and (a:== (car x) (car y))
         (b:== (cdr x) (cdr y)))))
