#lang remix
(require remix/stx0
         remix/num/gen0)

(def x
  (def a 40)
  (def b 2)
  (+ a b))

(def (f x y)
  (+ [(def z (+ x x)) z] y))

(def (g x)
  (cond
    [(< x 100) "100"]
    (def z (/ x 2))
    [(< z 100) "div 100"]
    [#:else "other"]))

(module+ test
  x
  (f x x)
  (g 50)
  (g 199)
  (g 200))
