#lang remix
;; #lang remix only contains two bindings: #%module-begin and require
;;
;; We use require to get everything else. most of it comes from stx0
(require remix/stx0
         remix/num/gen0)
(module+ test
  ;; This introduces ≡ as a testing form

  ;; XXX Drop this and instead have a macro for writing down
  ;; properties that communicates with boolean forms, etc. Supports ∀,
  ;; etc.
  (require remix/test0))

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
(require remix/datalog0)
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
(def (f-kw-arg #:x x) x)
(def (f-kw-args #:x x y) (+ x y))
(def (f-def-arg (x 20) (y 22)) (+ x y))
(def (f-two-arg x y) (+ x y))
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
(require (for-syntax remix/stx0))
(def [stx stx42] 42)

;; mac is define-simple-macro
(def [mac (flip f x y)]
  (f y x))
(module+ test
  {(flip - 5 0) ≡ (- 0 5)})

;; … (\ldots) is ... (because that doesn't work with cdots)
(def [mac (flipper f x … y)]
  (f y x …))
(module+ test
  {(flipper - 5 9 0) ≡ (- 0 5 9)})

;; data gives us interfaces, compound data, and data types and that
;; sort of thing
(require remix/data0)

;; First, we can define static interfaces, which associate dot-terms
;; with particular functions.
(def (example-f x y) x)
(def (example-g x y) y)
(def [stx example^]
  (static-interface
   [f example-f]
   [g example-g]))
(module+ test
  {(example^.f 1 2) ≡ 1}
  {(example^.g 1 2) ≡ 2})

;; These static interfaces allow nesting
(def example2-h 19)
(def [stx example2^]
  (static-interface
   [fg example^]
   [h example2-h]))
(module+ test
  {(example2^.fg.f 1 2) ≡ 1}
  {(example2^.fg.g 1 2) ≡ 2}
  {example2^.h ≡ 19}
  ;; Notice that cut works with nested dots
  {(λ.example2^.h 'ignored) ≡ 19})

;; They are also def transformers and when used in that way, they
;; implicitly pass the binding on as the first argument to functions
;; when used.
(def [example^ ee] 1)
(module+ test
  {(ee.f 2) ≡ 1}
  {(ee.g 2) ≡ 2})

;; This is especially useful inside of functions
(def (f-using-example [example^ ee])
  (ee.f 2))
(module+ test
  {(f-using-example 1) ≡ 1})

;; Sometimes a static-interface's binding's result is another
;; static-interface, rather than the binding itself. In that case, we
;; use the keyword #:is and specify another def transformer for
;; contexts where the value is in tail position.
(def [stx example3^]
  (static-interface
   ;; NB Perhaps it would be more punny to us [def id]?
   [fg example2-fg #:is example^]
   [h example2-h]))
(def example2-fg 1)
(module+ test
  {(example3^.fg.f 2) ≡ 1}
  {(example3^.fg.g 2) ≡ 2}
  {example3^.h ≡ 19})

;; XXX show an example where it isn't an interface but any def
;; transformer.

;; The syntax of interface members is not limited to identifiers. In
;; particular, #:keywords are useful. Furthermore, static-interface is
;; a def transformer itself, to clean up the syntax a little bit. I
;; expect that most people will use it this way.
(def example4-kw-key '#:key)
(def example4-key 'key)
(def [static-interface example4^]
  [#:key example4-kw-key]
  [key example4-key])
(module+ test
  {example4^.#:key ≡ '#:key}
  {example4^.key ≡ 'key})

;; A layout is a container with no sealing or representation
;; guarantees. This means you can't necessarily protect the contents
;; nor can you necessarily tell that you have one when you do.

;; layout is a def-transformer (XXX I wish I could make it phase1
;; macro also but it needs to define some functions that could be
;; called)

;; The most basic syntax is a list of fields, which are identifiers.
(def [layout posn]
  x y)
(module+ test
  ;; You will get an allocation function named #:alloc
  (def [posn p1] (posn.#:alloc [x 5] [y 7]))
  ;; And accessors
  {p1.x ≡ 5}
  {p1.y ≡ 7}
  ;; You may not have noticed, but posn was just a def transformer
  ;; that gave us access to these. We can, of course, just call them
  ;; directly through posn.
  {(posn.x p1) ≡ 5}
  ;; You will also get a copying function (XXX: Should it be named
  ;; `copy`? `update`? My analogy here is with hash-set)
  (def [posn p2] (p1.#:set [y {p1.y + 2}]))
  ;; Notice that these built-in functions are keywords, so that they
  ;; can't conflict with the fields you've defined.
  {p2.x ≡ 5}
  {p2.y ≡ 9}
  ;; This is aliased to =, which I expect is nicer to use.
  (def [posn p3] (p1.#:= [x 8]))
  {p3.x ≡ 8}
  {p3.y ≡ 7})

;; A layout can have a parent, which provides the guarantee that the
;; parent's functions will work on the child---meaning that whatever
;; the layout ends up being (and you can't decide that), the two will
;; overlap in this specific way. A layout has one or zero parents.
(def [layout quat]
  #:parent posn
  z)
(module+ test
  (def [quat q1] (quat.#:alloc [x 1] [y 2] [z 3]))
  {q1.x ≡ 1}
  {q1.y ≡ 2}
  {q1.z ≡ 3}
  ;; We can consider to be posn (imaging calling some function that
  ;; expects one) and it just works
  (def [posn qp1] q1)
  {qp1.x ≡ 1}
  {qp1.y ≡ 2}
  ;; However, that casting is computation-less, so it can be cast back
  ;; and we can get all the fields. However, if we changed it, it
  ;; wouldn't have stayed a quat.
  (def [quat qpq1] qp1)
  {qpq1.x ≡ 1}
  {qpq1.y ≡ 2}
  {qpq1.z ≡ 3})

;; A layout's fields may be specified as other layouts. When the first
;; field is a layout, this is not necessarily the same thing as a
;; parent (like C structs) but it may be. (No matter what, you'd never
;; be able to tell, since layout doesn't make representation promises
;; as a rule.)
(def [layout circle]
  [posn c] r)
(module+ test
  (def [circle c1] (circle.#:alloc [c p1] [r 8]))
  {c1.c.x ≡ 5}
  {c1.c.y ≡ 7}
  {c1.r ≡ 8})

;; A layout's fields can _actually_ just be any def transformer, and
;; thus could be static interfaces
(def [layout weird]
  [example^ e])
(module+ test
  (def [weird wr1] (weird.#:alloc [e 1]))
  {(wr1.e.f 2) ≡ 1}
  {(wr1.e.g 2) ≡ 2})

;; Now, the big reveal, layout has an extensible representation
;; planner system. At the moment, the only representations are
;;
;; layout-immutable : The default, backed by immutable vectors 
;; layout-mutable   : Backed by mutable vectors, with mutation support
;;
;; I expect to produce a few more
;;
;; (XXX) layout-c        : Compatible with C
;; (XXX) layout-optimize : Optimize for removing padding and have
;;                         cache-line-aligned accesses
;;
;; It would be possible to make layout-c right now, but define-cstruct
;; is really slow. It is trivial to have layout-optimize if you have
;; layout-c, but it would not be useful to use. mflatt and I talked
;; about a fast way of implementing them in Racket. The basic idea is
;; to have a new type of object in the VM where the pointer goes to
;; the middle of the allocated space which looks like
;;
;; [ <raw-values> | <tag> <vector layout> ]
;;
;; There may be necessary padding, but then the existing vector
;; functions would work. The raw values would use computed offsets to
;; get the values. The goal would be that parent structs would just
;; work and it would be easy to pass to C by sorting the _racket
;; pointers to the end.
;;
;; Anyways, here's a mutable example.
(def [layout world]
  #:rep layout-mutable
  [circle c1] [circle c2])
(module+ test
  (def [world w1] (world.#:alloc [c1 c1] [c2 (c1.#:set [r 3])]))
  {w1.c1.r ≡ 8}
  {w1.c2.r ≡ 3}
  ;; The set! is simultaneous
  (w1.#:set! [c1 w1.c2] [c2 w1.c1])
  {w1.c1.r ≡ 3}
  {w1.c2.r ≡ 8}
  ;; It is aliased to !
  (w1.#:! [c1 w1.c2] [c2 w1.c1])
  {w1.c1.r ≡ 8}
  {w1.c2.r ≡ 3})

;; These support mutual recursion
(def [layout even]
  #:rep layout-mutable
  e [odd o])
(def [layout odd]
  #:rep layout-mutable
  [even e] o)
(module+ test
  (def [even even1]
    (even.#:alloc
     [e 0]
     [o (odd.#:alloc
         [e #f]
         [o 1])]))
  (even1.o.#:set! [e even1])
  {even1.e ≡ 0}
  {even1.o.o ≡ 1}
  {even1.o.e.e ≡ 0}
  {even1.o.e.o.o ≡ 1}
  {even1.o.e.o.e.e ≡ 0})
