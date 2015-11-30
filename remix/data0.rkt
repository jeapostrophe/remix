#lang racket/base
(require (for-syntax
          racket/base
          syntax/parse
          racket/syntax
          racket/generic
          racket/format
          (prefix-in remix: remix/stx0)
          remix/stx/singleton-struct0
          (for-syntax racket/base
                      racket/syntax
                      syntax/parse
                      (prefix-in remix: remix/stx0)))
         racket/unsafe/ops
         racket/performance-hint
         (prefix-in remix: remix/stx0))

(begin-for-syntax
  (define-generics static-interface
    (static-interface-members static-interface))

  (module interface-member racket/base
    (require syntax/parse)
    (define-syntax-class interface-member
      (pattern x:id)
      (pattern x:keyword))
    (provide interface-member))
  (require (submod "." interface-member)
           (for-syntax
            (submod "." interface-member)))

  (define-syntax (phase1:static-interface stx)
    (syntax-parse stx
      #:literals (remix:#%brackets)
      [(_si (remix:#%brackets
             lhs:interface-member rhs:id
             (~optional
              (~seq #:is rhs-dt:id)
              #:defaults ([rhs-dt #'#f])))
            ...
            (~optional
             (~seq #:extensions
                   extension ...)
             #:defaults ([[extension 1] '()])))
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
                     #:key ~a))
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
              ;; XXX some way to overload this with #:extensions
              #:property prop:procedure
              (λ (_ stx)
                (raise-syntax-error 'int-name "Illegal in expression context" stx))
              #:methods gen:static-interface
              [(define (static-interface-members _)
                 available-ids)]
              #:methods remix:gen:dot-transformer
              [(define (dot-transform _ stx)
                 (syntax-parse stx
                   [(_dot me:id x:interface-member)
                    (get-rhs-id stx #'x)]
                   [(_dot me:id x:interface-member . more:expr)
                    (quasisyntax/loc stx
                      (remix:block
                       #,(get-rhs-def stx #'x)
                       (remix:#%dot x . more)))]))]
              #:methods remix:gen:app-dot-transformer
              [(define (app-dot-transform _ stx)
                 (syntax-parse stx
                   [(_app (_dot me:id x:interface-member) . body:expr)
                    (quasisyntax/loc stx
                      (#,(get-rhs-id stx #'x) . body))]
                   [(_app (_dot me:id x:interface-member . more:expr) . body:expr)
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
                                     (phase1:static-interface
                                      (remix:#%brackets lhs def-rhs)
                                      ...)))))]))]
              extension ...))))])))

(define-syntax (define-phase0-def->phase1-macro stx)
  (syntax-parse stx
    [(_ base:id)
     (with-syntax ([phase0:base (format-id #'base "phase0:~a" #'base)]
                   [phase1:base (format-id #'base "phase1:~a" #'base)])
       (syntax/loc stx
         (define-syntax phase0:base
           (singleton-struct
            #:property prop:procedure
            (λ (_ stx)
              (raise-syntax-error 'base "Illegal outside def" stx))
            #:methods remix:gen:def-transformer
            [(define (def-transform _ stx)
               (syntax-parse stx
                 #:literals (remix:#%brackets)
                 [(def (remix:#%brackets me:id i:id) . body:expr)
                  (syntax/loc stx
                    (remix:def (remix:#%brackets remix:stx i)
                               (phase1:base . body)))]))]))))]))

(define-phase0-def->phase1-macro static-interface)

(provide (rename-out [phase0:static-interface static-interface])
         (for-syntax (rename-out [phase1:static-interface static-interface])
                     gen:static-interface
                     static-interface?
                     static-interface-members))

(begin-for-syntax
  ;; XXX fill this in
  (define-generics layout))

(define-syntax phase0:layout
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'layout "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets)
        [(def (remix:#%brackets me:id name:id)
           f:id ...)
         (with-syntax* ([name-alloc (format-id #f "~a-alloc" #'name)]
                        [name-set (format-id #f "~a-set" #'name)]
                        [(f-idx ...)
                         (for/list ([i (in-naturals)]
                                    [f (in-list (syntax->list #'(f ...)))])
                           i)]
                        [(name-f ...)
                         (generate-temporaries #'(f ...))])
           (syntax/loc stx
             (begin
               (define-syntax (name-alloc stx)
                 (raise-syntax-error 'name-alloc "XXX alloc"))
               (define-syntax (name-set stx)
                 (raise-syntax-error 'name-set "XXX set"))
               (begin-encourage-inline
                 (define (name-f v) (unsafe-vector*-ref v f-idx))
                 ...)
               (define-syntax name
                 (phase1:static-interface
                  (remix:#%brackets #:alloc name-alloc)
                  (remix:#%brackets #:set name-set)
                  (remix:#%brackets #:= name-set)
                  (remix:#%brackets f name-f)
                  ...
                  #:extensions
                  #:methods gen:layout
                  [])))))]))]))

(provide (rename-out [phase0:layout layout])
         (for-syntax gen:layout
                     layout?))
