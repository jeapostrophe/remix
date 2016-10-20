#lang racket/base
;; Move this into remix/stx0

(require (for-syntax racket/base
                     remix/stx/raw0))

(begin-for-syntax
  (define (do-lang caller-id module-stx stx)
    (syntax-case stx ()
      [(_ module-name s ...)
       (identifier? #'module-name)
       (let ()
         (define ip
           (syntax-strings->input-port
            (syntax-source stx)
            (syntax->list #'(s ...))))
         (define mb
           (parameterize ([read-accept-reader #t]
                          [read-accept-lang #t])
             (read-syntax #'module-name ip)))
         (syntax-case mb ()
           [(_ _ module-lang body)
            (quasisyntax/loc stx
              (#,module-stx module-name module-lang body))]
           [_
            (raise-syntax-error caller-id "Body did not read as module" stx mb)]))])))

(define-syntax (lang stx)
  (do-lang 'lang #'module stx))
(define-syntax (lang* stx)
  (do-lang 'lang* #'module* stx))

(provide lang lang*)
