#lang remix
(require remix/stx
         remix/static-interface
         (for-syntax remix/stx))
(module+ test
   (require remix/test))

;; First, we can define static interfaces, which associate dot-terms
;; with particular functions.
(def (example-f x y) x)
(def (example-g x y) y)
(def [static-interface example^]
  (def [static-interface-member f]
    example-f)
  (def [static-interface-member g]
    example-g))
(module+ test
  {(example^.f 1 2) ≡ 1}
  {(example^.g 1 2) ≡ 2})

;; These static interfaces allow nesting
(def example2-h 19)
(def [static-interface example2^]
  (def [static-interface-member fg]
    example^)
  (def [static-interface-member h]
    example2-h))
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
;; => (begin (define real-ee 1) (define-syntax ee ...magic...))
(module+ test
  {(ee.f 2) ≡ 1}
  ;; => {(example^.f real-ee 2) ≡ 2}
  ;; => {(example^.f 1 2) ≡ 1}
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
(def [static-interface example3^]
  (def [static-interface-member fg]
    [example^ example2-fg])
  (def [static-interface-member h]
    example2-h))
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
  (def [static-interface-member #:key]
    example4-kw-key)
  (def [static-interface-member key]
    example4-key))
(module+ test
  {example4^.#:key ≡ '#:key}
  {example4^.key ≡ 'key})

(provide example^)
