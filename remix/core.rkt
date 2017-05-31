#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (remix-module-begin stx)
  (syntax-parse stx
    [(_ s ...)
     (syntax/loc stx
       (#%module-begin s ...))]))

(provide (rename-out
          [remix-module-begin #%module-begin])
         unquote
         require)
