#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(define-syntax (#%required stx)
  (syntax-parse stx
    [(_ client)
     (with-syntax ([x (format-id #'client "x")])
       (syntax/loc stx
         (define x 42)))]))

(provide #%required)
