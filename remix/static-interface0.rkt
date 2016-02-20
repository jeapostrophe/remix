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
         racket/splicing
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

(begin-for-syntax
  (struct static-interface-data
    (si-id members extensions))
  (define (empty-static-interface-data si-id)
    (static-interface-data si-id (make-hasheq) (box null))))

(define-syntax default-si #f)
(define-rename-transformer-parameter current-si
  (make-rename-transformer #'default-si))

(define-syntax static-interface-member
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'static-interface-member "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (define sid
        (or (syntax-local-value #'current-si (λ () #f))
            (raise-syntax-error 'static-interface-member
                                "Illegal outside static-interface" stx)))
      (syntax-parse stx
        #:literals (remix:#%brackets)
        [(_def (remix:#%brackets me:id the-sim:interface-member)
               (~or
                (remix:#%brackets rhs-dt the-target:id)
                (~and the-target:id
                      (~bind [rhs-dt #'#f]))))
         (define the-sim-v (syntax->datum #'the-sim))
         (define mems (static-interface-data-members sid))
         (when (hash-has-key? mems the-sim-v)
           (raise-syntax-error
            'static-interface-member
            (format "Duplicate definition of static-interface-member ~a"
                    the-sim-v)
            stx))
         (hash-set! mems the-sim-v (vector #'the-target #'rhs-dt))
         #'(void)]))]))

(define-syntax static-interface-extension
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'static-interface-extension "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (define sid
        (or (syntax-local-value #'current-si (λ () #f))
            (raise-syntax-error 'static-interface-extension
                                "Illegal outside static-interface" stx)))
      (syntax-parse stx
        #:literals (remix:#%brackets)
        [(_def (remix:#%brackets me:id)
               . extension)
         (define exts (static-interface-data-extensions sid))
         (set-box! exts (cons #'extension (unbox exts)))
         #'(void)]))]))

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
               body-expr ...)
         (syntax/loc stx
           (begin
             (define-syntax the-sid (empty-static-interface-data #'the-si))
             (splicing-syntax-parameterize
                 ([current-si
                   (make-rename-transformer #'the-sid)])
               body-expr ...)
             (static-interface-after-body the-sid)))]))]))

(define-syntax (static-interface-after-body stx)
  (syntax-parse stx
    #:literals ()
    [(_me the-sid)
     #:declare the-sid (static static-interface-data? "static interface data")
     (match-define (static-interface-data si-id members extensions-b)
       (attribute the-sid.value))
     (with-syntax* ([int-name si-id]
                    [([lhs rhs def-rhs rhs-dt full-def-rhs] ...)
                     (for/list ([(lhs rhs*rhs-dt) (in-hash members)])
                       (match-define (vector rhs rhs-dt) rhs*rhs-dt)
                       (define def-rhs
                         (format-id #f "~a-~a-for-def" #'int-name
                                    (if (keyword? lhs) (keyword->string lhs)
                                        lhs)))
                       (list
                        lhs
                        rhs
                        def-rhs
                        rhs-dt
                        (if rhs-dt
                            #'(remix:#%brackets rhs-dt def-rhs)
                            def-rhs)))]
                    [(extension ...) (reverse (unbox extensions-b))])
       (syntax/loc stx
         (remix:def
          (remix:#%brackets remix:stx int-name)
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
                                    (remix:def
                                     (remix:#%brackets static-interface-member lhs)
                                     full-def-rhs)
                                    ...
                                    (remix:def
                                     (remix:#%brackets static-interface-extension)
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
                                            (real-i . blah))])))))))]))]
             extension ...)))))]))

(provide static-interface
         static-interface-member
         static-interface-extension
         (for-syntax gen:static-interface
                     static-interface?
                     static-interface-members))
