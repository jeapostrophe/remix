#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (#%semi stx)
  (raise-syntax-error '#%semi "illegal outside of top-level, block, or braces" stx))

(begin-for-syntax
  (define-syntax-class not-semi
    #:literals (#%semi unquote)
    (pattern (~and (~not #%semi)
                   (~not (unquote #%semi)))))
  (define-splicing-syntax-class semi-piece
    #:literals (#%semi unquote)
    #:attributes (it)
    (pattern (~seq #%semi it))
    (pattern (~seq sp:not-semi ... #%semi)
             #:attr it #'(sp ...))
    (pattern (~seq sp:not-semi ... (~and uqs (unquote #%semi)))
             #:attr it
             (with-syntax ([semi-#%braces (datum->syntax #'uqs '#%braces)])
               #'(semi-#%braces sp ...))))
  (define-splicing-syntax-class semi-seq
    #:attributes ([semi-form 1] [tail-form 1])
    (pattern (~seq s:semi-piece ... tail-form:not-semi ...)
             #:attr [semi-form 1] (syntax->list #'(s.it ...)))))

(provide #%semi
         (for-syntax semi-seq))
