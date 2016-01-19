#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/generic
                     racket/match
                     racket/dict
                     syntax/id-table
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
    (interface-vtable interface)
    (interface-vtable-id interface)))

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
                          [(define (interface-vtable _) #'int-vtable)
                           (define (interface-vtable-id _) #'int-vtable-id)]))))]))]))

(begin-for-syntax
  (define-generics class)
  (struct class-expansion-data
    (cls-id [rep-id #:mutable] [new-found? #:mutable] interface-set))
  (define (empty-class-expansion-data cls)
    (class-expansion-data cls #f #f (make-bound-id-table))))

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
        [cls-vtables (format-id #f "~a-vtables" #'cls)]
        [cls-this (format-id #f "~a-this" #'cls)]
        [cls-new (format-id #f "~a-new" #'cls)]
        [cls-Current (format-id #f "~a-Current" #'cls)]
        [cls-alloc* (format-id #f "~a-alloc*" #'cls)]
        [cls-alloc (format-id #f "~a-alloc" #'cls)]
        [((int int-vtable cls-int-impl cls-int-impl-def) ...)
         (for/list ([(int int-internal) (in-dict interface-set)])
           (define cls-int-impl-id (format-id #f "~a-~a" #'cls int))
           (match-define (list int-vtable-id cls-int-impl-def)
             (int-internal cls-int-impl-id #'cls-this))
           (list int int-vtable-id cls-int-impl cls-int-impl-def))]
        [cls-new-def (new-found? #'cls-new)])
       (syntax/loc stx
         (begin
           (define (cls-alloc* the-rep)
             (object cls-vtables the-rep))
           (define-syntax (cls-alloc stx)
             (syntax-parse stx
               [(_ . args)
                (syntax/loc stx
                  (cls-alloc* (remix:#%app (remix:#%dot rep #:alloc) . args)))]))
           (remix:def (remix:#%brackets static-interface cls-Current)
                      (remix:#%brackets #:alloc cls-alloc))
           (splicing-syntax-parameterize
               ([Current (make-rename-transformer #'cls-Current)])
             cls-new-def
             cls-int-impl-def ...
             (remix:def (remix:#%brackets static-interface cls-this))
             (remix:def (remix:#%brackets static-interface cls)
                        (remix:#%brackets #:new cls-new)
                        (remix:#%brackets int cls-int-impl)
                        ...
                        #:extensions
                        #:methods gen:class
                        []))
           (define cls-vtables
             (make-immutable-hasheq
              (list (cons int-vtable cls-int-impl)
                    ...))))))]))

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
  (syntax-parse stx
    [(_ int . int-body)
     #:declare int (static interface? "interface")
     (define is (class-expansion-data-interface-set ced))
     (when (dict-has-key? is #'int)
       (raise-syntax-error
        'implementation
        (format "duplication definition of implementation for interface ~a" #'int)
        stx))
     (with-syntax ([int-vtable-id (interface-vtable-id (attribute int.value))]
                   [int-vtable (interface-vtable (attribute int.value))])
       (dict-set! is #'int
                  (λ (cls-impl-id cls-this-id)
                    (list
                     #'int-vtable-id
                     (with-syntax ([cls-impl cls-impl-id])
                       (syntax/loc stx
                         (remix:def (remix:#%brackets model int-vtable cls-impl)
                                    ;; XXX manipulate and bind this
                                    . int-body)))))))
     #'(void)]))

(remix:def (remix:#%brackets static-interface default-Current))
(define-rename-transformer-parameter Current
  (make-rename-transformer #'default-Current))

(remix:def (remix:#%brackets static-interface default-this))
(define-rename-transformer-parameter this
  (make-rename-transformer #'default-this))

(provide interface
         representation
         (rename-out [representation rep])
         new
         Current
         this
         implementation
         (rename-out [implementation impl])
         class)
