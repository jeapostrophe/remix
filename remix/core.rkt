#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (#%semi stx)
  (raise-syntax-error '#%semi "illegal outside of block" stx))

(define-syntax (remix-module-begin stx)
  (syntax-parse stx
    #:literals (#%semi)
    [(_ (~seq (~and semi-form (~not #%semi)) ... #%semi) ...
        (~and tail-form (~not #%semi)) ...)
     (syntax/loc stx
       (#%module-begin (semi-form ...) ... tail-form ...))]))

(provide (rename-out
          [remix-module-begin #%module-begin])
         #%semi
         require)
