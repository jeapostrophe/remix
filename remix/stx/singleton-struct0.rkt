#lang racket/base
(require syntax/parse/define)

(module singleton racket/base
  (require (for-syntax racket/base
                       syntax/parse
                       racket/syntax))
  (define-syntax (singleton-struct stx)
    (syntax-parse stx
      [(singleton-struct . struct-args)
       (with-syntax ([the-singleton (generate-temporary (syntax-local-name))])
         (syntax/loc stx
           (let ()
             (struct the-singleton () . struct-args)
             (the-singleton))))]))
  (provide singleton-struct))
(require (submod "." singleton)
         (for-syntax (submod "." singleton)))

(define-simple-macro (define/singleton-struct singleton:id . struct-args)
  (define singleton (singleton-struct . struct-args)))
(define-simple-macro (define-syntax/singleton-struct singleton:id . struct-args)
  (define-syntax singleton (singleton-struct . struct-args)))

(provide
 singleton-struct
 (for-syntax singleton-struct)
 define/singleton-struct
 define-syntax/singleton-struct)
