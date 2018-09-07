#lang at-exp racket/base

;; Some remix macros just work:
(require remix/datalog)

(define graph (make-theory))
@datalog[graph]{
 edge(a, b). edge(b, c). edge(c, d). edge(d, a).
 path(X, Y) :- edge(X, Y).
 path(X, Y) :- edge(X, Z), path(Z, Y).
 path(X, Y)?
}

;; But others from remix/stx are specially exposed
(require remix/racket)

@lang[typed]{
#lang typed/racket

(: f (Float -> Float))
(define (f x)
  (+ x 5.0))

(f 8.0)

(provide f)
}

(module+ test
  (require (submod ".." typed)
           unstable/error)
  (f 0.0)
  (with-handlers ([exn:fail?
                   (λ (x)
                     (parameterize ([current-error-port (current-output-port)])
                       (error-display x)))])
    (f 5)))

(define (g x)
  (+ x 6.0))
(provide g)

@lang*[main]{
#lang typed/racket
(require/typed (submod "..")
  [g (Float -> Float)])

(g 1.0)
}

;; This works with different readers too, not just s-exprs

@lang[db]{
#lang datalog

edge(a, b). edge(b, c). edge(c, d). edge(d, a).
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).
path(X, Y)?
}

(module+ test
  (require (submod ".." db)))

;; But if the reader itself uses @, then you need to quote it

@lang[document]|{
#lang scribble/manual

@title{How to use Racket}

It's pretty awesome
}|

(module+ test
  (require (prefix-in doc: (submod ".." document)))
  doc:doc)
