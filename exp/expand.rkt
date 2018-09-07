#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/splicing)

(begin-for-syntax
  (define special-define-set
    (make-parameter (box '())))
  (define (add-to-boxed-list! b v)
    (set-box! b (cons v (unbox b)))))

(define-syntax (detect-special-defines stx)
  (syntax-parse stx
    [(_ body-expr ...)
     (define the-b (box '()))
     (define ctxt (syntax-local-context))
     (printf "ctxt: ~v\n" ctxt)
     (with-syntax ([(new-body-begin ...)
                    (parameterize ([special-define-set the-b])
                      (for/list ([be (in-list (syntax->list #'(body-expr ...)))])
                        (local-expand/capture-lifts
                         be
                         ctxt
                         (list #'define-values))))])
       (with-syntax ([(d ...) (unbox the-b)])
         (quasisyntax/loc stx
           (begin
             new-body-begin ...
             (printf "Defined: ~a\n"
                     '(d ...))))))]))

(define-syntax (special-define stx)
  (syntax-parse stx
    [(_ x:id b:expr)
     (printf "special-define ran!\n")
     (add-to-boxed-list! (special-define-set) #'x)
     (syntax/loc stx
       (define x b))]))

(detect-special-defines
 (special-define x 1)
 (special-define y 2))

(+ x y)
