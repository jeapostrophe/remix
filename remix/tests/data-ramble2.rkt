;; Interfaces & Objects

(def [interface 2d<%>]
  translate
  area)

(def [interface Circle<%>]
  (layout-interface circle))

(def [class Circle]
  #:layout circle
  #:new
  (λ (x y r)
    (this.#:alloc [c (posn.#:alloc [x x] [y y])]
                  [r r]))

  (layout-implements Circle<%>)
  
  (def [implements 2d<%>]
    [(translate x y)
     {this.#:set
      [c (this.c.#:set [x {x + this.c.x}]
                       [y {y + this.c.y}])]}]
    [(area)
     {3 * this.r * this.r}]))

(def [Circle C1] (Circle.#:new 1 2 3))
(module+ test
  {C1.Circle<%>.c.x ≡ 1}
  {C1.Circle<%>.c.y ≡ 2}
  {C1.Circle<%>.r ≡ 3}
  {(C1.2d<%>.area) ≡ 27}
  (def [Circle C1′] (C1.2d<%>.translate 3 2))
  {C1′.Circle<%>.c.x ≡ 4}
  {C1′.Circle<%>.c.y ≡ 4}
  {C1′.Circle<%>.r ≡ 3})

