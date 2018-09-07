#lang remix
(require remix/stx
         remix/layout
         "static-interface.rkt")
(module+ test
  (require remix/test))

;; A layout is a container with no sealing or representation
;; guarantees. This means you can't necessarily protect the contents
;; nor can you necessarily tell that you have one when you do.

;; layout is a def-transformer (XXX I wish I could make it phase1
;; macro also but it needs to define some functions that could be
;; called)
;;
;; XXX maybe I can expand to a submodule and local-require

;; The most basic syntax is a list of fields, which are identifiers.
(def [layout posn]
  x y)
(module+ test
  ;; You will get an allocation function named #:alloc
  (def [posn p1] (posn.#:alloc [x 5] [y 7]))
  ;; XXX (def [posn p1] #:alloc [x 5] [y 7]) <--- def transformer for allocation
  ;; XXX (def [posn p1] [x 5] [y 7]) <--- default use is allocation
  ;; And accessors
  {p1.x ≡ 5}
  {p1.y ≡ 7}
  ;; You may not have noticed, but posn was just a def transformer
  ;; that gave us access to these. We can, of course, just call them
  ;; directly through posn.
  {(posn.x p1) ≡ 5}
  ;; You will also get a copying function
  (def [posn p2] (p1.#:set [y {p1.y + 2}]))
  ;; XXX (def [posn p2] p1 [y {p1.y + 2}]) <---- default use with expr is copy
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

;; XXX Does it do the "right thing" for copying? (i.e. when a parent
;; copies, do the child's fields get copied as is)

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
;; (XXX) layout-c         : Compatible with C
;; (XXX) layout-optimize  : Optimize for removing padding and have
;;                          cache-line-aligned accesses
;; (XXX) layout-enumerate : Use data/enumerate
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

(provide posn circle)
