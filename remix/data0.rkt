#lang racket/base
(require (for-syntax
          racket/base
          syntax/parse
          racket/syntax
          (prefix-in remix: remix/stx0)
          remix/stx/singleton-struct0
          (for-syntax racket/base
                      syntax/parse
                      (prefix-in remix: remix/stx0)))
         (prefix-in remix: remix/stx0))

(begin-for-syntax
  (define-syntax (static-interface stx)
    (syntax-parse stx
      #:literals (remix:#%brackets)
      [(_si (remix:#%brackets
             lhs:id rhs:id
             (~optional
              (~seq #:is rhs-dt:id)
              #:defaults ([rhs-dt #'#f])))
            ...)
       (with-syntax ([int-name (syntax-local-name)]
                     [(def-rhs ...) (generate-temporaries #'(rhs ...))])
         (syntax/loc stx
           (let ()
             (define int-id->orig
               (make-immutable-hasheq
                (list (cons 'lhs (cons #'rhs #'rhs-dt))
                      ...)))
             (define available-ids
               (sort (hash-keys int-id->orig)
                     string<=?
                     #:key symbol->string))
             (define (get-rhs stx x)
               (define xv (syntax->datum x))
               (hash-ref int-id->orig
                         xv
                         (λ ()
                           (raise-syntax-error
                            'int-name
                            (format "Unknown component ~v, expected one of ~v"
                                    xv
                                    available-ids)
                            stx
                            x))))
             (define (get-rhs-id stx x)
               (car (get-rhs stx x)))
             (define (get-rhs-is stx x)
               (define r (cdr (get-rhs stx x)))
               (if (syntax-e r)
                   r
                   #f))
             (define (get-rhs-def stx x-stx)
               (define xd (get-rhs-is stx x-stx))
               (with-syntax* ([xb (get-rhs-id stx x-stx)]
                              [x-def
                               (if xd xd #'remix:stx)]
                              [x-def-v
                               (if xd #'xb #'(make-rename-transformer #'xb))])
                 (quasisyntax/loc stx
                   (remix:def (remix:#%brackets x-def #,x-stx) x-def-v))))
             (singleton-struct
              #:property prop:procedure
              (λ (_ stx)
                (raise-syntax-error 'int-name "Illegal in expression context" stx))
              #:methods remix:gen:dot-transformer
              [(define (dot-transform _ stx)
                 (syntax-parse stx
                   [(_dot me:id x:id)
                    (get-rhs-id stx #'x)]
                   [(_dot me:id x:id . more:expr)
                    (quasisyntax/loc stx
                      (remix:block
                       #,(get-rhs-def stx #'x)
                       (remix:#%dot x . more)))]))]
              #:methods remix:gen:app-dot-transformer
              [(define (app-dot-transform _ stx)
                 (syntax-parse stx
                   [(_app (_dot me:id x:id) . body:expr)
                    (quasisyntax/loc stx
                      (#,(get-rhs-id stx #'x) . body))]
                   [(_app (_dot me:id x:id . more:expr) . body:expr)
                    (quasisyntax/loc stx
                      (remix:block
                       #,(get-rhs-def stx #'x)
                       (remix:#%app (remix:#%dot x . more) . body)))]))]
              #:methods remix:gen:def-transformer
              [(define (def-transform _ stx)
                 (syntax-parse stx
                   #:literals (remix:#%brackets)
                   [(def (remix:#%brackets me:id i:id) . body:expr)
                    (with-syntax ([real-i (generate-temporary #'i)])
                      (syntax/loc stx
                        (begin
                          (remix:def real-i . body)
                          (remix:def (remix:#%brackets remix:mac (def-rhs . blah:expr))
                                     (remix:#%app rhs real-i . blah))
                          ...
                          (remix:def (remix:#%brackets remix:stx i)
                                     (static-interface
                                      (remix:#%brackets lhs def-rhs)
                                      ...)))))]))]))))])))

(provide (for-syntax static-interface))
