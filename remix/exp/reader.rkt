#lang racket/base

(define STX? #f)

(define (remix-f f)
  (parameterize ([read-square-bracket-as-paren #f]
                 [read-curly-brace-as-paren #f]
                 [read-square-bracket-with-tag #t]
                 [read-curly-brace-with-tag #t]
                 [read-accept-dot #f]
                 [read-accept-infix-dot #f]
                 [read-cdot #t])
    (f)))

(define (remix-read) (remix-f read))
(define (remix-read-syntax) (remix-f read-syntax))

(module+ test
  (require rackunit
           racket/port)
  (define-syntax-rule (testit* t ...)
    (begin (testit . t) ...))
  (define-syntax-rule (testit str d)
    (testit-f str 'd #'d))
  
  (define (testit-f str qd stx)
    (check-equal? (with-input-from-string str remix-read) qd)
    (when STX?
      (check-equal? (with-input-from-string str remix-read-syntax) stx)))

  (check-false (read-square-bracket-with-tag))
  (check-false (read-curly-brace-with-tag))
  (check-false (read-cdot))
  
  (testit*
   ["(1 2 3)" (1 2 3)]
   ["[1 2 3]" (#%brackets 1 2 3)]
   ["{1 2 3}" (#%braces 1 2 3)]
   ["|a.b|" a.b]
   ["a.b" (#%dot a b)]
   ["a .b" (#%dot a b)]
   ["a. b" (#%dot a b)]
   ["a . b" (#%dot a b)]
   ["1.a" (#%dot 1 a)]
   ["#i1.2 .a" (#%dot 1.2 a)]
   ["1 .2.a" (#%dot 1 (#%dot 2 a))]
   ["a.#i1.2" (#%dot a 1.2)]
   ["a.b.c" (#%dot a (#%dot b c))]
   ["a.(b c)" (#%dot a (b c))]
   ["(a b).c" (#%dot (a b) c)]
   ["(a b).(c d)" (#%dot (a b) (c d))]
   ["(a b).[3]" (#%dot (a b) (#%brackets 3))]
   ["({1})" ((#%braces 1))]))
