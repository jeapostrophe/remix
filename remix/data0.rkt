#lang racket/base
(require (for-syntax
          racket/base
          syntax/parse
          racket/syntax
          (prefix-in remix: remix/stx0)
          remix/stx/singleton-struct0
          (for-syntax racket/base
                      syntax/parse))
         (prefix-in remix: remix/stx0))

(begin-for-syntax
  (define-syntax (static-interface stx)
    (syntax-parse stx
      #:literals (remix:#%brackets)
      [(_si (remix:#%brackets lhs:id rhs:id) ...)
       (with-syntax ([int-name (syntax-local-name)]
                     [(def-rhs ...) (generate-temporaries #'(rhs ...))])
         (syntax/loc stx
           (let ()
             (define int-id->orig-id
               (make-immutable-hasheq
                (list (cons 'lhs #'rhs)
                      ...)))
             (define available-ids
               (sort (hash-keys int-id->orig-id)
                     string<=?
                     #:key symbol->string))
             (define (get-binding stx x)
               (define xv (syntax->datum x))
               (hash-ref int-id->orig-id
                         xv
                         (λ ()
                           (raise-syntax-error
                            'int-name
                            (format "Unknown component ~v, expected one of ~v"
                                    xv
                                    available-ids)
                            stx
                            x))))
             (singleton-struct
              #:property prop:procedure
              (λ (_ stx)
                (raise-syntax-error 'int-name "Illegal in expression context" stx))
              #:methods remix:gen:dot-transformer
              [(define (dot-transform _ stx)
                 (syntax-parse stx
                   [(_dot me:id x:id)
                    (get-binding stx #'x)]
                   [(_dot me:id x:id . more:expr)
                    (with-syntax ([xb (get-binding stx #'x)])
                      (syntax/loc stx
                        (let-syntax ([x (make-rename-transformer #'xb)])
                          (remix:#%dot x . more))))]))]
              #:methods remix:gen:app-dot-transformer
              [(define (app-dot-transform _ stx)
                 (syntax-parse stx
                   [(_app (_dot me:id x:id) . body:expr)
                    (quasisyntax/loc stx
                      (#,(get-binding stx #'x) . body))]
                   [(_app (_dot me:id x:id . more:expr) . body:expr)
                    (with-syntax ([xb (get-binding stx #'x)])
                      (syntax/loc stx
                        (let-syntax ([x (make-rename-transformer #'xb)])
                          (remix:#%app (remix:#%dot x . more) . body))))]))]
              #:methods remix:gen:def-transformer
              [(define (def-transform _ stx)
                 (syntax-parse stx
                   #:literals (remix:#%brackets)
                   [(def (remix:#%brackets me:id x:id) . body:expr)
                    (with-syntax ([real-x (generate-temporary #'x)])
                      (syntax/loc stx
                        (begin
                          (remix:def real-x . body)
                          (remix:def (remix:#%brackets remix:mac (def-rhs . blah:expr))
                                     (rhs real-x . blah))
                          ...
                          (remix:def (remix:#%brackets remix:stx x)
                                     (static-interface
                                      (remix:#%brackets lhs def-rhs)
                                      ...)))))]))]))))])))

(provide (for-syntax static-interface))
