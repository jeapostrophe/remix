#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/generic
                     racket/set
                     syntax/id-set
                     remix/stx/singleton-struct0
                     (prefix-in remix: remix/stx0))
         racket/stxparam
         racket/splicing
         remix/theory0
         remix/static-interface0
         (prefix-in remix: remix/stx0))

(struct object (interface->implementation rep))

(begin-for-syntax
  (define-generics interface
    (interface-vtable interface)))

(define-syntax interface
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'interface "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets remix:def interface)
        ;; XXX support parameters?
        [(remix:def (remix:#%brackets interface int:id)
                    ;; XXX support properties?
                    ;; XXX make expandable position
                    v:id ...)
         (with-syntax ([int-vtable
                        (format-id #f "~a-vtable" #'int)]
                       [(obj-v ...)
                        (for/list ([v (in-list (syntax->list #'(v ...)))])
                            (format-id #f "~a-~a" #'int v))])
           (syntax/loc stx
             (begin
               (remix:def int-vtable-id (gensym 'int-vtable))
               (remix:def (remix:#%brackets theory int-vtable)
                          ;; XXX add a property for vtables
                          ;; XXX support defaults?
                          v ...)
               (remix:def (obj-v o)
                          (remix:def (remix:#%brackets int-vtable vt)
                                     (hash-ref (object-interface->implementation o)
                                               int-vtable-id))
                          (remix:#%dot vt v))
               ...
               (remix:def (remix:#%brackets static-interface int)
                          (remix:#%brackets v obj-v)
                          ...
                          #:extensions
                          #:methods gen:interface
                          [(define (interface-vtable _) #'int-vtable)]))))]))]))

(begin-for-syntax
  (define-generics class)
  (struct class-expansion-data
    (cls-id [rep-id #:mutable] new-id [new-found? #:mutable] interface-set))
  (define (empty-class-expansion-data cls new-id)
    (class-expansion-data cls #f new-id #f (mutable-bound-id-set)))
  (define current-class-expansion-data
    (make-parameter #f)))

(define-syntax class
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'class "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (remix:#%brackets remix:def class)
        [(remix:def (remix:#%brackets class cls:id)
                    body-expr ...)
         (define ctxt (syntax-local-context))
         (define cls-stop-list (list #'remix:def #'remix:#%brackets))
         (with-syntax ([cls-new (format-id #f "~a-new" #'cls)]
                       [cls-Current (format-id #f "~a-Current" #'cls)]
                       [cls-alloc (format-id #f "~a-alloc" #'cls)])
           (define the-ced (empty-class-expansion-data #'cls #'cls-new))
           (with-syntax ([(new-body ...)
                          (parameterize ([current-class-expansion-data the-ced])
                            (for/list ([be (in-list (syntax->list #'(body-expr ...)))])
                              (local-expand/capture-lifts
                               be
                               ctxt cls-stop-list)))])
             (printf "after body local-expand\n")
             (with-syntax ([((int cls-int-impl) ...)
                            ;; XXX
                            '()])
               (syntax/loc stx
                 (begin
                   (remix:def (remix:#%brackets static-interface cls)
                              (remix:#%brackets #:new cls-new)
                              (remix:#%brackets int cls-int-impl)
                              ...
                              #:extensions
                              #:methods gen:class
                              [])
                   (remix:def (remix:#%brackets static-interface cls-Current)
                              (remix:#%brackets #:alloc cls-alloc))
                   ;; XXX bind this
                   (splicing-syntax-parameterize
                       ([Current (rename-dot-transformer #'cls-Current)])
                     new-body ...))))))]))]))

(begin-for-syntax
  (struct rename-dot-transformer (id)
    #:methods remix:gen:dot-transformer
    [(define/generic super-dt dot-transform)
     (define (dot-transform rdt stx)
       (super-dt (syntax-local-value (rename-dot-transformer-id rdt)) stx))]))

(define-syntax (representation stx)
  (cond
    [(current-class-expansion-data)
     => (λ (ced)
          (syntax-parse stx
            [(_ l:id)
             ;; XXX ensure l is layout
             (cond
               [(class-expansion-data-rep-id ced)
                (raise-syntax-error
                 'representation
                 (format "Duplicate definition of representation for class ~a"
                         (syntax-e (class-expansion-data-cls-id ced)))
                 stx)]
               [else
                (set-class-expansion-data-rep-id! ced #'l)])
             #'(void)]))]
    [else
     (raise-syntax-error 'representation "Illegal outside class" stx)]))

(define-syntax (new stx)
  (cond
    [(current-class-expansion-data)
     => (λ (ced)
          (syntax-parse stx
            [(_ args . body)
             (when (class-expansion-data-new-found? ced)
               (raise-syntax-error
                 'new
                 (format "Duplicate definition of constructor for class ~a"
                         (syntax-e (class-expansion-data-cls-id ced)))
                 stx))
             (set-class-expansion-data-new-found?! ced #t)
             (with-syntax ([cls-new (class-expansion-data-new-id ced)])
               (syntax/loc stx
                 (remix:def (cls-new . args) . body)))]))]
    [else
     (raise-syntax-error 'new "Illegal outside class" stx)]))

(define-syntax (implementation stx)
  (cond
    [(current-class-expansion-data)
     => (λ (ced)
          ;; XXX
          #'(void))]
    [else
     (raise-syntax-error 'implementation "Illegal outside class" stx)]))

(define-syntax-parameter Current
  (λ (stx)
    (raise-syntax-error 'Current "Illegal outside class definitions" stx)))
(define-syntax-parameter this
  (λ (stx)
    (raise-syntax-error 'this "Illegal outside class definitions" stx)))

(provide interface
         representation
         (rename-out [representation rep])
         new
         Current
         this
         implementation
         (rename-out [implementation impl])
         class)
