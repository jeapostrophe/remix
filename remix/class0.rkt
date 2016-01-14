#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/generic
                     racket/set
                     racket/match
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
    (cls-id [rep-id #:mutable] [new-found? #:mutable] interface-set))
  (define (empty-class-expansion-data cls)
    (class-expansion-data cls #f #f (mutable-bound-id-set))))

(define-syntax default-ced #f)
(define-rename-transformer-parameter current-ced
  (make-rename-transformer #'default-ced))

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
         (syntax/loc stx
           (begin
             (define-syntax the-ced (empty-class-expansion-data #'cls))
             (splicing-syntax-parameterize
                 ([current-ced
                   (make-rename-transformer #'the-ced)])
               body-expr ...)
             (class-after-body the-ced)))]))]))

(define-syntax (class-after-body stx)
  (syntax-parse stx
    [(_ the-ced)
     #:declare the-ced (static class-expansion-data? "class expansion data")
     (match-define (class-expansion-data cls-id rep-id new-found? interface-set)
       (attribute the-ced.value))
     (unless new-found?
       (raise-syntax-error 'class
                           (format "no constructor found for class ~a" cls-id)
                           stx))
     (with-syntax*
       ([cls cls-id]
        [rep rep-id]
        [cls-new (format-id #f "~a-new" #'cls)]
        [cls-Current (format-id #f "~a-Current" #'cls)]
        [cls-alloc (format-id #f "~a-alloc" #'cls)]
        [((int cls-int-impl) ...)
         ;; XXX
         '()])
       (syntax/loc stx
         (begin
           (remix:def (remix:#%brackets static-interface cls-Current)
                      (remix:#%brackets #:alloc cls-alloc))
         (splicing-syntax-parameterize
             ([Current (make-rename-transformer #'cls-Current)])
           (remix:def (remix:#%brackets static-interface cls)
                      (remix:#%brackets #:new cls-new)
                      (remix:#%brackets int cls-int-impl)
                      ...
                      #:extensions
                      #:methods gen:class
                      [])))))]))

(define-syntax (representation stx)
  (define ced
    (or (syntax-local-value #'current-ced (λ () #f))
        (raise-syntax-error 'representation "Illegal outside class" stx)))
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
        (set-class-expansion-data-rep-id! ced #'l)
        #'(void)])]))

(define-syntax (new stx)
  (define ced
    (or (syntax-local-value #'current-ced (λ () #f))
        (raise-syntax-error 'new "Illegal outside class" stx)))
  (syntax-parse stx
    [(_ args . body)
     (when (class-expansion-data-new-found? ced)
       (raise-syntax-error
        'new
        (format "Duplicate definition of constructor for class ~a"
                (syntax-e (class-expansion-data-cls-id ced)))
        stx))
     (set-class-expansion-data-new-found?!
      ced
      (λ (new-id)
        (with-syntax ([cls-new new-id])
          (syntax/loc stx
            (remix:def (cls-new . args) . body)))))
     #'(void)]))

(define-syntax (implementation stx)
  (define ced
    (or (syntax-local-value #'current-ced (λ () #f))
        (raise-syntax-error 'implementation "Illegal outside class" stx)))
  ;; XXX
  #'(void))

(remix:def (remix:#%brackets static-interface default-Current))
(define-rename-transformer-parameter Current
  (make-rename-transformer #'default-Current))

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
