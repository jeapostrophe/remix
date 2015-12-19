#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (#%semi stx)
  (raise-syntax-error '#%semi "illegal outside of block" stx))

(begin-for-syntax
  (define-splicing-syntax-class semi-seq
    #:literals (#%semi)
    #:attributes ([semi-form 1] [tail-form 1])
    (pattern (~seq (~seq (~and semi-piece-form (~not #%semi)) ... #%semi) ...
                   (~and tail-form (~not #%semi)) ...)
             #:attr [semi-form 1] (syntax->list #'((semi-piece-form ...) ...)))))

(provide #%semi
         (for-syntax semi-seq))
