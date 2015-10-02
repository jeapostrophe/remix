#lang racket/base

read-accept-dot
read-accept-infix-dot

(define (read-remix)
  (parameterize ([read-square-bracket-as-paren #f]
                 [read-curly-brace-as-paren #f]
                 [read-square-bracket-with-tag #t]
                 [read-curly-brace-with-tag #t])
    (read)))

(module+ test
  (require rackunit
           racket/port)
  (define-syntax-rule (testit* t ...)
    (begin (testit . t) ...))

  (define (testit str qd)
    (check-equal? (with-input-from-string str read-remix) qd))
  
  (testit*
   ["[1 2 3]" '(#%brackets 1 2 3)]
   ["{1 2 3}" '(#%braces 1 2 3)]
   ["a.b" '(#%dot a b)]
   ["1.a" '(#%dot 1 a)]
   ["1.2.a" '(#%dot 1.2 a)]
   ["a.1.2" '(#%dot a 1.2)]
   ["a.b.c" '(#%dot a (#%dot b c))]
   ["a.(b c)" '(#%dot a (b c))]
   ["(a b).c" '(#%dot (a b) c)]
   ["(a b).(c d)" '(#%dot (a b) (c d))]
   ["(a b).[3]" '(#%dot (a b) (#%brackets 3))]))
