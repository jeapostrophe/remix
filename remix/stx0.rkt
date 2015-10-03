#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ x:id . body:expr)
     (syntax/loc stx
       (define x (remix-block . body)))]
    [(_ (x:id . args:expr) . body:expr)
     (syntax/loc stx
       (def x (remix-λ args . body)))]))

(define-syntax (remix-block stx)
  ;; xxx gather up defs and turn into bind
  (syntax-parse stx
    [(_ . body:expr)
     (syntax/loc stx
       (let () . body))]))

;; xxx also make it a #%dot transformer that is cut.
(define-syntax (remix-λ stx)
  (syntax-parse stx
    ;; xxx transform args into bind plus what racket λ needs
    [(_ (arg:id ...) . body:expr)
     (syntax/loc stx
       (λ (arg ...) (remix-block . body)))]))

(define-syntax (#%brackets stx)
  (syntax-parse stx
    [(_ . body:expr)
     (syntax/loc stx
       (remix-block . body))]))

(define-syntax (#%braces stx)
  (syntax-parse stx))

(define-syntax (#%dot stx)
  (syntax-parse stx))

(define-syntax (remix-cond stx)
  (syntax-parse stx
    #:literals (#%brackets)
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets #:else . answer-body:expr))
     (syntax/loc stx
       (remix-block before ... . answer-body))]
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets question:expr . answer-body:expr)
        . more:expr)
     (syntax/loc stx
       (remix-block before ...
                    (if question
                        (remix-block . answer-body)
                        (remix-cond . more))))]))

(provide def
         (rename-out [remix-λ λ]
                     [remix-cond cond])
         #%brackets
         #%braces
         #%dot
         #%app
         #%datum
         module
         module*
         module+)
