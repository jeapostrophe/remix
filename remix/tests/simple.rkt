#lang remix
(require remix/stx0
         remix/num/gen0)

(def x
  (def a 40)
  (def b 2)
  (+ a b))
(module+ test
  x)

(def (f x y)
  (+ [(def z (+ x x)) z] y))
(module+ test
  (f x x))

(def (g x)
  (cond
    [(< x 100) "100"]
    (def z (/ x 2))
    [(< z 100) "div 100"]
    [#:else "other"]))
(module+ test
  (g 50)
  (g 199)
  (g 200))

(require remix/datalog0)
(def graph (make-theory))
@datalog[graph]{
 edge(a, b). edge(b, c). edge(c, d). edge(d, a).
 path(X, Y) :- edge(X, Y).
 path(X, Y) :- edge(X, Z), path(Z, Y).
 path(X, Y)?
}


