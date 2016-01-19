#lang racket/base
(require (for-syntax racket/base
                     syntax/quote
                     syntax/parse
                     racket/syntax
                     racket/generic
                     racket/format
                     racket/list
                     racket/match
                     (prefix-in remix: remix/stx0)
                     remix/stx/singleton-struct0
                     (for-syntax racket/base
                                 racket/syntax
                                 syntax/parse
                                 racket/generic
                                 (prefix-in remix: remix/stx0)))
         racket/stxparam
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
            (submod "." interface-member))))

(define-syntax static-interface
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'static-interface "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets)
        [(_def (remix:#%brackets me:id the-si:id)
           ;; XXX make expandable position
           (remix:#%brackets
            lhs:interface-member rhs:id
            (~optional
             (~seq #:is rhs-dt:id)
             #:defaults ([rhs-dt #'#f])))
           ...
           (~optional
            (~seq #:extensions
                  extension ...)
            #:defaults ([[extension 1] '()])))
         (with-syntax* ([int-name (or (syntax-local-name) 'static-interface)]
                        [(def-rhs ...)
                         (for/list ([lhs (in-list
                                          (map syntax->datum
                                               (syntax->list #'(lhs ...))))])
                           (format-id #f "~a-~a-for-def" #'int-name
                                      (if (keyword? lhs) (keyword->string lhs)
                                          lhs)))]
                        [(full-def-rhs ...)
                         (for/list ([def-rhs (in-list (syntax->list #'(def-rhs ...)))]
                                    [rhs-dt (in-list (syntax->list #'(rhs-dt ...)))])
                           (if (syntax-e rhs-dt)
                               (list def-rhs '#:is rhs-dt)
                               (list def-rhs)))])
           (syntax/loc stx
             (remix:def
              (remix:#%brackets remix:stx the-si)
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
                 #:methods gen:static-interface
                 [(define (static-interface-members _)
                    available-ids)]
                 #:methods remix:gen:dot-transformer
                 [(define (dot-transform _ stx)
                    (syntax-parse stx
                      [(_dot me:id (x:interface-member . args))
                       (quasisyntax/loc stx
                         (remix:#%app (remix:#%app (remix:#%dot me x)) . args))]
                      [(_dot me:id x:interface-member)
                       (get-rhs-id stx #'x)]
                      [(_dot me:id . (~and x+more (x:interface-member . more)))
                       (quasisyntax/loc stx
                         (remix:block
                          #,(get-rhs-def stx #'x)
                          #,(syntax/loc #'x+more
                              (remix:#%dot x . more))))]))]
                 #:methods remix:gen:app-dot-transformer
                 [(define (app-dot-transform _ stx)
                    (syntax-parse stx
                      [(_app (_dot me:id (x:interface-member . args)) . body)
                       (quasisyntax/loc stx
                         (remix:#%app
                          (remix:#%app (remix:#%app (remix:#%dot me x)) . args)
                          . body))]
                      [(_app (_dot me:id x:interface-member) . body)
                       (quasisyntax/loc stx
                         (#,(get-rhs-id stx #'x) . body))]
                      [(_app (_dot me:id x:interface-member . more) . body)
                       (quasisyntax/loc stx
                         (remix:block
                          #,(get-rhs-def stx #'x)
                          (remix:#%app (remix:#%dot x . more) . body)))]))]
                 #:methods remix:gen:def-transformer
                 [(define (def-transform _ stx)
                    (syntax-parse stx
                      #:literals (remix:#%brackets)
                      [(__def (remix:#%brackets me:id i:id) . body)
                       (with-syntax ([real-i (generate-temporary #'i)])
                         (syntax/loc stx
                           (begin
                             (remix:def real-i . body)
                             (remix:def (remix:#%brackets remix:stx def-rhs)
                                        (λ (stx)
                                          (syntax-parse stx
                                            [_:id
                                             (syntax/loc stx
                                               (rhs real-i))]
                                            [(_ . blah)
                                             (syntax/loc stx
                                               (rhs real-i . blah))])))
                             ...
                             (remix:def (remix:#%brackets static-interface i)
                                        (remix:#%brackets lhs . full-def-rhs)
                                        ...
                                        #:extensions
                                        ;; NB I don't pass on other
                                        ;; extensions... I don't think
                                        ;; it can possibly make sense,
                                        ;; because I don't know what
                                        ;; they might be.
                                        #:property prop:procedure
                                        (λ (_ stx)
                                          (syntax-parse stx
                                            [_:id
                                             (syntax/loc stx
                                               real-i)]
                                            [(_ . blah)
                                             (syntax/loc stx
                                               (real-i . blah))]))))))]))]
                 extension ...)))))]))]))

(provide static-interface
         (for-syntax gen:static-interface
                     static-interface?
                     static-interface-members))

