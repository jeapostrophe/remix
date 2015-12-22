#lang racket/base
(require remix/semi
         (for-syntax racket/base
                     syntax/parse))

(define-syntax (remix-module-begin stx)
  (syntax-parse stx
    [(_ s:semi-seq)
     (syntax/loc stx
       (#%module-begin s.semi-form ... s.tail-form ...))]))

(provide (rename-out
          [remix-module-begin #%module-begin])
         #%semi
         unquote
         require)
