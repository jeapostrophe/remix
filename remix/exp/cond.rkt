#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (brackets-cond stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx
       (error 'cond "Reach the end of cond"))]
    [(_ ((~literal #%brackets) q . a) . more)
     (syntax/loc stx
       (if q (let () . a) (brackets-cond . more)))]
    [(_ e . more)
     (syntax/loc stx
       (let () e (brackets-cond . more)))]))

(module+ test
  (brackets-cond
   (define x 5)
   (#%brackets (even? x)
               27)
   (define y (+ x 6))
   (#%brackets (odd? y)
               28)
   19))
