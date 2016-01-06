#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     remix/stx/singleton-struct0
                     (prefix-in remix: remix/stx0))
         racket/stxparam
         remix/theory0
         (prefix-in remix: remix/stx0))

(struct object (interface->implementation rep))

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
         (syntax/loc stx
           ;; XXX instead, make an int-vtable and then a separate int
           ;; def transformer that looks at objects.
           (remix:def (remix:#%brackets theory int)
                      ;; XXX add a property for interfaces
                      ;; XXX support defaults?
                      v ...))]))]))

(define-syntax-parameter representation
  (λ (stx)
    (raise-syntax-error 'representation "Illegal outside class" stx)))
(define-syntax-parameter new
  (λ (stx)
    (raise-syntax-error 'new "Illegal outside class" stx)))
(define-syntax-parameter this
  (λ (stx)
    (raise-syntax-error 'this "Illegal outside class" stx)))
(define-syntax-parameter implementation
  (λ (stx)
    (raise-syntax-error 'implementation "Illegal outside class" stx)))

(define-syntax class
  (singleton-struct
   #:property prop:procedure
   (λ (_ stx)
     (raise-syntax-error 'class "Illegal outside def" stx))
   #:methods remix:gen:def-transformer
   [(define (def-transform _ stx)
      ;; XXX ensure everything is expandable
      ;; XXX
      #'(void))]))

(provide interface
         representation
         (rename-out [representation rep])
         new
         this
         implementation
         (rename-out [implementation impl])
         class)
