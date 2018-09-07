#lang racket/base
(require (for-syntax racket/base
                     syntax/quote
                     syntax/parse
                     racket/syntax
                     racket/generic
                     racket/format
                     racket/list
                     racket/match
                     (prefix-in remix: remix/stx)
                     remix/stx/singleton-struct
                     (for-syntax racket/base
                                 racket/syntax
                                 syntax/parse
                                 racket/generic
                                 (prefix-in remix: remix/stx)))
         racket/stxparam
         racket/unsafe/ops
         racket/performance-hint
         (prefix-in remix: remix/stx)
         remix/layout)

(define-syntax theory
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'theory "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets remix:def theory)
        ;; XXX support parameters
        [(remix:def (remix:#%brackets theory thy:id)
                    ;; XXX support properties (including type)
                    ;; XXX make expandable position
                    v:id ...)
         (syntax/loc stx
           (remix:def (remix:#%brackets layout thy)
                      ;; XXX add a property for theories
                      ;; XXX support defaults
                      v ...))]))]))

(define-syntax model
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'model "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets remix:def model)
        [(remix:def (remix:#%brackets model thy:id mod:id)
                    ;; XXX make expandable position
                    (remix:#%brackets f:id v:expr) ...)
         ;; XXX support verification of properties
         ;; XXX support theory parameters
         ;; XXX check that thy is a theory
         ;; XXX check that f is complete and apply defaults if not
         (syntax/loc stx
           (remix:def (remix:#%brackets thy mod)
                      (remix:#%app
                       (remix:#%dot thy #:alloc)
                       (remix:#%brackets f v) ...)))]))]))

(provide theory
         model)
