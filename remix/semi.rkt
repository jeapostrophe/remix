#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (#%semi stx)
  (raise-syntax-error '#%semi "illegal outside of top-level, block, or braces" stx))

(begin-for-syntax
  (define-syntax-class not-semi
    #:literals (#%semi unquote)
    (pattern (~and (~not #%semi)
                   (~not (unquote . _)))))
  (define-splicing-syntax-class semi-piece
    #:literals (#%semi unquote)
    #:attributes (it)
    (pattern (unquote it))
    (pattern (~seq sp:not-semi ... #%semi)
             #:attr it #'(sp ...)))
  (define-splicing-syntax-class semi-seq
    #:attributes ([semi-form 1] [tail-form 1])
    (pattern (~seq s:semi-piece ... tail-form:not-semi ...)
             #:attr [semi-form 1] (syntax->list #'(s.it ...)))))

(provide #%semi
         (for-syntax semi-seq))
