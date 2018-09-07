#lang remix
(require remix/stx0
         remix/theory0
         remix/num/gen0)
(module+ test
   (require remix/test0))

;; A theory is a specification of some values
(def [theory Monoid]
  op id)
(module+ test
  ;; You can write generic functions over a theory. This imposes a
  ;; single constant cost to access the operations (basically, a
  ;; vector-ref) and the operation couldn't be inlined. (Although if
  ;; the generic function were inlined, then it could, presumably.)
  (def (monoid-id-test [Monoid m] a)
    ;; Notice the syntax `m.(op x y)` as short-hand for `((m.op) x y)`
    {((m.op) a m.id) ≡ m.(op m.id a)}))

;; A model is an object that satisfies the theory
(def [model Monoid Monoid-Nat:+]
  [op +]
  [id 0])

(def [model Monoid Monoid-Nat:*]
  [op *]
  [id 1])

(module+ test
  ;; You can pass the model explicitly to functions over the theory
  (monoid-id-test Monoid-Nat:+ 5)
  (monoid-id-test Monoid-Nat:* 5)
  ;; Or you can use it directly. This works exactly the same, although
  ;; we can imagine it might be inlinable.
  {((Monoid-Nat:+.op) 6 Monoid-Nat:+.id) ≡ Monoid-Nat:+.(op Monoid-Nat:+.id 6)})
