#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/stxparam)

(begin-for-syntax
  (define-syntax-class template-ell
    #:attributes ([x 1])
    (pattern ()
             #:attr [x 1] null)
    (pattern (a:template-ell . d:template-ell)
             #:attr [x 1]
             (append (attribute a.x) (attribute d.x)))
    (pattern ((~literal unquote) [depth:number one:id])
             #:attr [x 1]
             (if (< (syntax-parameter-value #'template-depth)
                    (syntax->datum #'depth))
                 (list #'one)
                 null))
    (pattern _:id
             #:attr [x 1] null)))

(define-syntax-parameter template-depth 0)

(define-syntax (template-list stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx
       null)]
    [(~and (_ _ (~literal ...) ~! . _)
           (_ a:template-ell (~literal ...) . more))
     (syntax/loc stx
       (append
        (for/list ([a.x (in-list a.x)]
                   ...)
          (syntax-parameterize
              ([template-depth (add1 (syntax-parameter-value #'template-depth))])
            (template a)))
        (template-list . more)))]
    [(_ ((~literal unquote-splicing) a) . more)
     (syntax/loc stx
       (append (template a) (template-list . more)))]
    [(_ a . more)
     (syntax/loc stx
       (cons (template a) (template-list . more)))]))

(define-syntax (template stx)
  (syntax-parse stx
    ;; xxx check depth of this to be equal to template-depth?
    [(_ ((~literal unquote) [_ a]))
     #'a]
    [(_ ((~literal unquote-splicing) . _))
     (raise-syntax-error 'template "unquote-splicing not allowed in template" stx)]
    [(_ (~and a ((~literal quote) _)))
     #'a]
    [(_ (op more ...))
     (syntax/loc stx
       (apply op (template-list more ...)))]
    [(_ a)
     #'a]))

(module+ test
  (require rackunit)
  (define a 10)
  (define x (list 1 2 3 4))
  (define y (list 5 6 7 8))
  (check-equal?
   (template (+ (* ,[0 a] ,[1 x] ,[1 y]) ...))
   (+ (* 10 1 5)
      (* 10 2 6)
      (* 10 3 7)
      (* 10 4 8)))
  (check-equal?
   (template (list '+ (list '* ,[0 a] ,[1 x] ,[1 y]) ...))
   '(+ 
     (* 10 1 5)
     (* 10 2 6)
     (* 10 3 7)
     (* 10 4 8)))
  (check-equal?
   (template (+ ,[0 a] ,@(map add1 ,[0 x]) ,@,[0 y]))
   (+ 10 
      2 3 4 5
      5 6 7 8)))
