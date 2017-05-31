#lang remix
(require remix/stx0
         remix/class0
         remix/num/gen0
         "layout.rkt")
(module+ test
  (require remix/test0))

(def [interface 2d<%>]
  translate
  area)

(def [interface Circle<%>]
  ;; xxx make a macro for "interface of layout's fields"
  c r)

;; A class is a representation, a constructor, and implementations of
;; interfaces.
(def [class Circle]
  (rep circle) ;; rep = representation
  (new (x y r)
   (Current.#:alloc
    [c (posn.#:alloc [x x] [y y])]
    [r r]))
  
  ;; xxx make a macro from "layout's fields implements this interface"
  ;; xxx this doesn't work
  #;(implementation Circle<%>
    [c (λ () this.c)]
    [r (λ () this.r)])

  ;; xxx this doesn't work
  #; 
  (impl 2d<%>
    [translate
     (λ (x y)
       (this.#:set
        [c (this.c.#:set [x {x + this.c.x}]
                         [y {y + this.c.y}])]))]
    [area
     (λ () {3 * this.r * this.r})]))

;; XXX allow w/o #:new?, like layout

;; xxx class as def is broken
#;
(module+ test
  (def [Circle C1] (Circle.#:new 1 2 3))
  ;; If you know something is a particular class, then you can access
  ;; its implementations directly. This is more efficient.
  {C1.Circle<%>.c.x ≡ 1}
  {C1.Circle<%>.c.y ≡ 2}
  {C1.Circle<%>.r ≡ 3}
  {(C1.2d<%>.area) ≡ 27}
  (def [Circle C1′] (C1.2d<%>.translate 3 2))
  {C1′.Circle<%>.c.x ≡ 4}
  {C1′.Circle<%>.c.y ≡ 4}
  {C1′.Circle<%>.r ≡ 3}
  ;; In contrast, when you access them as their interfaces, a lookup
  ;; is done.
  (def [2d<%> C1-as-2d] C1)
  {C1-as-2d.(area) ≡ 27}
  (def [Circle<%> C1-as-Circ] C1)
  {C1-as-Circ.c.x ≡ 1}
  {C1-as-Circ.c.y ≡ 2}
  {C1-as-Circ.r ≡ 3})

;; xxx C1 can't be defined
#;
(module+ test
  ;; Like theories, you can define functions that are generic over an
  ;; interface.
  (def (squarea [2d<%> o])
    {o.(area) * o.(area)})
  {(squarea C1) ≡ 729}
  ;; The default behavior of class dot-transformers on unknown methods
  ;; is to treat it as a generic function.
  {C1.(squarea) ≡ 729})
