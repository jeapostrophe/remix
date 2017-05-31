#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (remix-module-begin stx)
  (syntax-parse stx
    [(_ s ...)
     (syntax/loc stx
       (#%module-begin s ...))]))

(define-syntax (remix-require stx)
  (syntax-parse stx
    [(_ m)
     (syntax/loc stx
       (begin (require (rename-in m
                                  [#%required internal-#%required]))
              (internal-#%required m)))]
    [(_ m ...)
     (syntax/loc stx
       (begin (remix-require m)
              ...))]))

(provide (rename-out
          [remix-module-begin #%module-begin]
          [remix-require require]
          ;; xxx make
          #;[remix-require* require*]))
