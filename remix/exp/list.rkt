#lang remix

(data seq
 (struct empty)
 (def (first t)
  (error))
 (def (rest t)
  (error))
 (def (empty? t)
   #t)
 (def (cons x t)
  ((outer cons) x t))
 (def (snoc t x)
  ((outer cons) x t)))

(data seq
 (struct cons
  [racket car]
  [racket cdr])
 (def (first t)
  t.car)
 (def (rest t)
  t.cdr)
 (def (empty? t)
   #f))
