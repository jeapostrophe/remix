#lang remix
(require remix/base
         "expand-from.rkt")

(def [posn p1] (posn.#:alloc [x 5] [y 7]))
p1.x
p1.y

(def [posn p2] (p1.#:set [y {p1.y + 2}]))
p2.x
p2.y

(def [posn p3] (p1.#:= [x 8]))
p3.x
p3.y
