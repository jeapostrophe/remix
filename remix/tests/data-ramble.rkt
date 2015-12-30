;; THEORIES + MODELS

;; INTERFACES + OBJECTS
;; An interface is just a vtable specification (theory)
;; An implementation is a vtable (model)
;; A class is a set of implementations and a representation
;; +---> (+) private : representation is available
;; +---> ( )    open : no rep, new implementations can be added
;; +---> (-)  closed : no rep, no new imps
;; An object is a sealed pair of a class and a layout

(def [interface 2d<%>]
  translate
  area)

(def [class +Circle Circle -Circle]
  #:layout circle
  #:new (λ (x y r)
          ;; this is the layout of the object wrapper
          ;;
          ;; alternatively, make this something that assumes you want
          ;; the below and acts like it.
          (this.#:alloc
           [rep (circle.#:alloc [c (posn.#:alloc [x x] [y y])]
                                [r r])])))

;; This implementation uses +Circle, so it has access to the
;; representation.
(def [implementation 2d<%> +Circle]
  [translate
   ;; this is +Circle
   (λ (this)
     {this.#:set
      [c (this.c.#:set [x {x + this.c.x}]
                       [y {y + this.c.y}])]})]
  [area
   (λ (this)
     {3 * this.r * this.r})])

;; Here's a contrived example of using Circle, where you can add
;; things but don't have the representation (i.e. you can implement it
;; based on other things it has)
(def [interface 2dview<%>]
  view-area)

(def [implementation 2dview<%> Circle #:is 2d<%>]
  [view-area
   ;; this is 2d<%>
   (λ (this) (this.area))])

