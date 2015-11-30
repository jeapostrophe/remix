#lang remix
(require remix/struct.0
         remix/match.0
         num/int.0
         gfx/2d.0
         big-bang.0)

(struct #rocket
 ([int h]
  [int dh]))

(data rocket
 #:this [#rocket r]
 (def (rocket [int (~opt h 0)] [int (~opt dh 1)])
  (#rocket.alloc [h h] [dh dh]))
 #:implements world/anim^
 (def (tick)
   (r.= [h {r.h + r.dh}]))
 (def (draw)
  (circle 'yellow 5)))

(big-bang (rocket.new))
