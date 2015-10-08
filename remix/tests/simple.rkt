#lang remix
;; This requires changes to Racket that are not yet pushed, but will
;; be once the release branch is made.

;; #lang remix only contains two bindings: #%module-begin and require
;;
;; we use require to get everything else. most of it comes from stx0
(require remix/stx0
         remix/num/gen0)

;; define is replaced with def
(def z 42)
(module+ test
  z)

;; when def has more forms than one, they are put inside of a block
(def x
  (def a 40)
  (def b 2)
  (+ a b))
(module+ test
  x)

;; but of course def supports function definitions. [] is NOT the same
;; as () and defaults to expanding to a block definition
(def (f x y)
  (+ [(def z (+ x x))
      z]
     y))
(module+ test
  (f x x))

;; cond requires []s for the question-answer pairs. It uses this to
;; make any code in between clauses go in between the `if`s that pop
;; out of the cond macro. finally, cond REQUIRES a #:else clause.
(def (g x)
  (cond
    [(< x 100) "100"]
    (def z (/ x 2))
    [(< z 100) "div 100"]
    [#:else z]))
(module+ test
  (g 50)
  (g 199)
  (g 200))

;; the @ reader is always on. One fun thing about this is that you can
;; make non-() macros. I wrote a little helper function to turn the
;; string arguments that @{} produces into a string port that has
;; accurate source location information for the original file. datalog
;; uses this to make all the source locations correct, so errors in
;; datalog will give accurate source locations.
(require remix/datalog0)
(def graph (make-theory))
@datalog[graph]{
 edge(a, b). edge(b, c). edge(c, d). edge(d, a).
 path(X, Y) :- edge(X, Y).
 path(X, Y) :- edge(X, Z), path(Z, Y).
 path(X, Y)?
}

;; {} is also not (), but is an infix macro
(def v7
  {3 + 4})
(module+ test
  v7)

;; {} use C's precedence and considers the things you expect to be
;; operators. there's a syntax-time struct property that allows you to
;; specify what you want the precedence of an operator to be.
(def v-26
  {2 * 3 - 48 / 4 - 4 * 5})
(module+ test
  v-26)

;; if a symbol contains no alphabetic or numeric characters, then it
;; is considered an operator. This means you can automatically use
;; stuff like & and →, but you won't confuse it with symbols like z
(def v85
  {z * 2 + 1})
(module+ test
  v85)

(def v1
  (def & bitwise-and)
  {5 & 1})
(module+ test
  v1)

(def v56
  (def (→ x y) (+ (* x x) y))
  {v7 → v7})
(module+ test
  v56)

;; However, if you use , then you can force anything to be a binary
;; operator and force something that would have been a binary operator
;; into an argument.
(def v14
  (def (f x y) (+ x y))
  {v7 ,f v7})
(module+ test
  v14)

(def v14b
  {v7 ,(λ (x y) (+ x y)) v7})
(module+ test
  v14b)

(def v9
  (def & 2)
  {v7 + ,&})
(module+ test
  v9)

;; ...
;; ,,,
;; ooo
;; …


