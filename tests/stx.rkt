#lang remix
;; Remix comments start with ;

;; #lang remix only contains two bindings: #%module-begin and require
;;
;; We use require to get everything else. most of it comes from stx
(require remix/stx)

(module+ test
  ;; This introduces ≡ as a testing form

  ;; XXX Drop this and instead have a macro for writing down
  ;; properties that communicates with boolean forms, etc. Supports ∀,
  ;; etc.
  (require remix/test))

;; define is replaced with def
(def z 42)
(module+ test
  {z ≡ 42})

;; when def has more forms than one, they are put inside of a block
(def x
  (def a 40)
  (def b 2)
  (+ a b))
(module+ test
   {x ≡ 42})

;; If you would like to use ;-syntax in the inside of def, then you
;; need more punctuation. You have two choices.
(def x2
 [(def a 40)
  (def b 2)
  (+ a b)])
(module+ test
   {x2 ≡ 42})

(def x3
  [(def a 40)
   (def b 2)
   {a + b}])
(module+ test
  {x3 ≡ 42})

(def x4
  [{a := 40}
   (def b 2)
   {a + b}])
(module+ test
  {x4 ≡ 42})

;; but of course def supports function definitions. [] is NOT the same
;; as (), it parses as #%brackets and defaults to expanding to a block
;; definition
(def (f x y)
  (+ [(def z (+ x x))
      z]
     y))
(module+ test
  {(f x x) ≡ 126})

;; That's the same as just 'block' if you want to be specific
(def (other-f x y)
  (+ (block (def z (+ x x))
            z)
     y))
(module+ test
  {(other-f x x) ≡ 126})

;; cond requires []s for the question-answer pairs. It uses this to
;; make any code in between clauses go in between the `if`s that pop
;; out of the cond macro. 
(def (g x)
  (cond
    [(< x 100) "100"]
    (def z (/ x 2))
    [(< z 100) "div 100"]
    [#:else z]))
(module+ test
  {(g 50) ≡ "100"}
  {(g 199) ≡ "div 100"}
  {(g 200) ≡ 100})

;; If cond reaches the end without an else, then a runtime error is
;; generated
(def (g2 x)
  (cond
    [(< x 100) "100"]
    (def z (/ x 2))
    [(< z 100) "div 100"]))
(module+ test
  {(g2 50) ≡ "100"}
  {(g2 199) ≡ "div 100"}
  ;; This is the error test:
  #;(g2 200))

;; This functionality is provided by ☠ (aka impossible!)
(def (g3)
  ☠)
(module+ test
  #;(g3))

;; the @ reader is always on. One fun thing about this is that you can
;; make non-() macros. I wrote a little helper function to turn the
;; string arguments that @{} produces into a string port that has
;; accurate source location information for the original file. datalog
;; uses this to make all the source locations correct, so errors in
;; datalog will give accurate source locations.
(require remix/datalog)
(def graph (make-theory))
@datalog[graph]{
 edge(a, b). edge(b, c). edge(c, d). edge(d, a).
 path(X, Y) :- edge(X, Y).
 path(X, Y) :- edge(X, Z), path(Z, Y).
 path(X, Y)?
}

;; {} is also not (), it is parsed as #%braces, and by default is an
;; infix macro
(def v7
  {3 + 4})
(module+ test
  {v7 ≡ 7})

;; {} use C's precedence and considers the things you expect to be
;; operators. there's a syntax-time struct property that allows you to
;; specify what you want the precedence of an operator to be.
(def v-26
  {2 * 3 - 48 / 4 - 4 * 5})
(module+ test
  {v-26 ≡ -26})

;; if a symbol contains no alphabetic or numeric characters, then it
;; is considered an operator. This means you can automatically use
;; stuff like & and →, but you won't confuse it with symbols like z
(def v85
  {z * 2 + 1})
(module+ test
  {v85 ≡ 85})

(def v1
  (def & bitwise-and)
  {5 & 1})
(module+ test
  {v1 ≡ 1})

(def v56
  (def (→ x y) (+ (* x x) y))
  {v7 → v7})
(module+ test
  {v56 ≡ 56})

;; However, if you use , then you can force anything to be a binary
;; operator and force something that would have been a binary operator
;; into an argument.
(def v14
  (def (f x y) (+ x y))
  {v7 ,f v7})
(module+ test
  {v14 ≡ 14})

(def v14b
  {v7 ,(λ (x y) (+ x y)) v7})
(module+ test
  {v14b ≡ 14})

(def v9
  (def & 2)
  {v7 + ,&})
(module+ test
  {v9 ≡ 9})

;; λ is a dot-transformer for cut
(def f11
  λ.(+ 10 1))
(def v11
  (f11 'ignored))
(module+ test
  {v11 ≡ 11})

(def v11b
  ;; ((#%dot λ (+ 10 1)) 'ignored)
  (λ.(+ 10 1) 'ignored))
(module+ test
  {v11b ≡ 11})

(def v11c
  (λ.(+ $ 1) 10))
(module+ test
  {v11c ≡ 11})

;; ≙ and := are synonyms for def, and because of the {} rules, is a
;; binary operator.
{v33a ≙ 33}
{v33b := 33}
(module+ test
  {v33a ≡ 33}
  {v33b ≡ 33})

(def v28
  {(f x) ≙ x + x}
  (f 14))
(module+ test
  {v28 ≡ 28})

;; def* allows nested binding inside blocks. This is aliased to nest
;; for def* transformers like parameterize that would look strange
;; otherwise.
(def v64
  (def* x 2)
  (def* x {x + x})
  (def* x {x + x})
  (nest x {x + x})
  (def* x {x + x})
  (def* x {x + x})
  x)
(module+ test
  {v64 ≡ 64})

;; The lambda and def syntax allow all the normal forms of Racket
;; function arguments. The main exception being rest arguments are
;; specified differently because the . would be parsed incorrectly
;; otherwise.
(def (f-no-args) 42)
(def (f-one-arg x) x)
;; => (def f-one-arg (λ (x1) (def x x1) x))
(def (f-kw-arg #:x x) x)
(def (f-kw-args #:x x y) (+ x y))
(def (f-def-arg (x 20) (y 22)) (+ x y))
(def (f-two-arg x y) (+ x y))
;; (f-rest-args . x) => ((#%dot f-rest-args x))
(def (f-rest-args #%rest x) 42)
(module+ test
  {(f-no-args) ≡ 42}
  {(f-one-arg 42) ≡ 42}
  {(f-kw-arg #:x 42) ≡ 42}
  {(f-kw-args #:x 22 20) ≡ 42}
  {(f-two-arg 20 22) ≡ 42}
  {(f-def-arg) ≡ 42}
  {(f-def-arg 21) ≡ 43}
  {(f-def-arg 21 21) ≡ 42}
  {(f-rest-args) ≡ 42}
  {(f-rest-args 1) ≡ 42}
  {(f-rest-args 1 2 3) ≡ 42})

;; def supports a variety of "def transformers" that change from
;; defining a phase-0 value to something else.

;; val ensures that a function is NOT defined
(def [val v99] 99)
(module+ test
  {v99 ≡ 99})

;; stx is define-syntax
(require (for-syntax remix/stx))
(def [stx stx42] 42)

;; mac is define-simple-macro
(def [mac (flip f x y)]
  (f y x))
(module+ test
  {(flip - 5 0) ≡ (- 0 5)})

;; ... => (#%dot #%dot #%dot)
;; … (\ldots) is ... (because that doesn't work with cdots)
;; or dotdotdot or ***
(def [mac (flipper1 f x … y)]
  (f y x …))
(def [mac (flipper2 f x dotdotdot y)]
  (f y x dotdotdot))
(def [mac (flipper3 f x *** y)]
  (f y x ***))
(module+ test
  {(flipper1 - 5 9 0) ≡ (- 0 5 9)}
  {(flipper2 - 5 9 0) ≡ (- 0 5 9)}
  {(flipper3 - 5 9 0) ≡ (- 0 5 9)})

