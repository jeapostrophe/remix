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
         remix/static-interface0
         (prefix-in remix: remix/stx0))

(begin-for-syntax
  (define-generics layout
    (layout-planner-id layout)
    ;; xxx the accessors seem to not be around anyways, so instead,
    ;; this should just be a mapping produced by the planner.
    (layout-field->acc layout))
  (define-generics layout-planner
    (layout-planner-mutable? layout-planner))

  (define-syntax-class field
    #:attributes (name dt)
    #:literals (remix:#%brackets)
    (pattern name:id
             #:attr dt #f)
    (pattern (remix:#%brackets dt:id name:id)
             ;; XXX This can't be here because it disallows mutual
             ;; recursion... move the check somewhere else?
             
             ;; #:declare dt (static remix:def-transformer? "def transformer")
             )))

(define-syntax layout-immutable
  (singleton-struct
   #:methods gen:layout-planner
   [(define (layout-planner-mutable? lp) #f)]))

(define-syntax layout-mutable
  (singleton-struct
   #:methods gen:layout-planner
   [(define (layout-planner-mutable? lp) #t)]))

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
           (~optional (~and (~seq #:parent (~var parent (static layout? "layout")))
                            (~bind [parent-va (attribute parent.value)]))
                      #:defaults ([parent-va #f]))
           (~optional (~and (~seq #:rep (~var rep (static layout-planner?
                                                          "layout planner"))))
                      #:defaults ([rep #f]))
           ;; XXX make expandable position
           F:field ...)
         (define parent-v (attribute parent-va))
         (define this-rep-id (attribute rep))
         (define parent-rep-id (and parent-v (layout-planner-id parent-v)))
         (unless (or (not this-rep-id)
                     (not parent-rep-id)
                     (bound-identifier=? this-rep-id parent-rep-id))
           (raise-syntax-error
            'layout
            (format "Parent (~v) and child (~v) representation planner must match"
                    parent-rep-id
                    this-rep-id)
            stx))
         (define the-planner-id
           (or parent-rep-id
               this-rep-id
               #'layout-immutable))
         (define the-planner
           (syntax-local-value the-planner-id))
         (define parent-f->acc
           (or (and parent-v (layout-field->acc parent-v))
               (hasheq)))
         (define f->acc
           (for/fold ([base parent-f->acc])
                     ([the-f (in-list (syntax->datum #'(F.name ...)))]
                      [the-dt (in-list (attribute F.dt))]
                      [the-idx (in-naturals (hash-count parent-f->acc))])
             (when (hash-has-key? base the-f)
               (raise-syntax-error 'layout
                                   (format "duplicate field ~a in layout"
                                           the-f)
                                   stx
                                   the-f))
             (define the-name-f (format-id #f "~a-~a" #'name the-f))
             (hash-set base the-f (vector the-name-f the-dt the-idx))))
         (with-syntax* ([name-alloc (format-id #f "~a-alloc" #'name)]
                        [name-set (format-id #f "~a-set" #'name)]
                        [name-set! (format-id #f "~a-set!" #'name)]
                        [((all-f all-name-f all-f-si-rhs all-f-idx) ...)
                         (for/list ([(the-f v) (in-hash f->acc)])
                           (match-define (vector the-name-f the-dt the-f-idx) v)
                           (list the-f the-name-f
                                 (if the-dt
                                     (list the-name-f '#:is the-dt)
                                     (list the-name-f))
                                 the-f-idx))]
                        [stx-the-planner-id the-planner-id]
                        [stx-f->acc f->acc]
                        [(rep-constructor
                          rep-accessor rep-mutate
                          (mutation-interface ...))
                         ;; XXX This should work differently
                         (if (layout-planner-mutable? the-planner)
                             (list #'vector
                                   #'unsafe-vector*-ref
                                   #'unsafe-vector*-set!
                                   #'((remix:#%brackets #:set! name-set!)
                                      (remix:#%brackets #:! name-set!)))
                             (list #'vector-immutable
                                   #'unsafe-vector*-ref
                                   #'void
                                   #'()))])
           (syntax/loc stx
             (begin
               (begin-for-syntax
                 (define f->acc stx-f->acc)
                 (define available-fields
                   (sort (hash-keys f->acc)
                         string<=?
                         #:key symbol->string))
                 (define ordered-fields
                   (sort (hash-keys f->acc)
                         <=
                         #:key (λ (x)
                                 (vector-ref (hash-ref f->acc x) 2))))
                 (define-syntax-class name-arg
                   #:attributes (lhs rhs)
                   #:literals (remix:#%brackets)
                   (pattern (remix:#%brackets lhs:id rhs:expr)
                            #:do [(define lhs-v (syntax->datum #'lhs))]
                            #:fail-unless
                            (hash-has-key? f->acc lhs-v)
                            (format "invalid field given: ~a, valid fields are: ~a"
                                    lhs-v
                                    available-fields)))
                 (define-syntax-class name-args
                   #:attributes (f->rhs)
                   (pattern (a:name-arg (... ...))
                            #:do [(define first-dupe
                                    (check-duplicates
                                     (syntax->datum #'(a.lhs (... ...)))))]
                            #:fail-when first-dupe
                            (format "field occurs twice: ~a" first-dupe)
                            #:attr f->rhs
                            (for/hasheq ([l (syntax->list #'(a.lhs (... ...)))]
                                         [r (syntax->list #'(a.rhs (... ...)))])
                              (values (syntax->datum l) r)))))
               (define-syntax (name-alloc stx)
                 (syntax-parse stx
                   [(_ . args:name-args)
                    (with-syntax ([(f-val (... ...))
                                   (for/list ([this-f (in-list ordered-fields)])
                                     (hash-ref (attribute args.f->rhs)
                                               this-f
                                               (λ ()
                                                 (raise-syntax-error
                                                  'name-alloc
                                                  (format "missing initializer for ~a"
                                                          this-f)
                                                  stx))))])
                      (syntax/loc stx
                        (rep-constructor f-val (... ...))))]))
               (define-syntax (name-set stx)
                 (syntax-parse stx
                   [(_ base:expr . args:name-args)
                    (with-syntax* ([base-id (generate-temporary #'base)]
                                   [(f-val (... ...))
                                    (for/list ([this-f (in-list ordered-fields)])
                                      (define this-name-f
                                        (vector-ref
                                         (hash-ref f->acc this-f)
                                         0))
                                      (hash-ref (attribute args.f->rhs)
                                                this-f
                                                (λ ()
                                                  (quasisyntax/loc stx
                                                    (#,this-name-f base-id)))))])
                      (syntax/loc stx
                        (let ([base-id base])
                          (rep-constructor f-val (... ...)))))]))
               (define-syntax (name-set! stx)
                 (syntax-parse stx
                   [(_ base:expr . args:name-args)
                    (with-syntax* ([base-id (generate-temporary #'base)]
                                   [((f-val-id f-val f-idx) (... ...))
                                    (for/list ([(this-f this-f-val)
                                                (in-hash (attribute args.f->rhs))])
                                      (match-define
                                        (vector this-name-f _ this-idx)
                                        (hash-ref f->acc this-f))
                                      (list
                                       (generate-temporary this-f)
                                       this-f-val
                                       this-idx))])
                      (syntax/loc stx
                        (let ([f-val-id f-val]
                              (... ...))
                          (let ([base-id base])
                            (rep-mutate base-id f-idx f-val-id)
                            (... ...)
                            (void)))))]))
               ;; xxx add per-field mutators with a set! macro
               (begin-encourage-inline
                 (define (all-name-f v) (rep-accessor v all-f-idx))
                 ...)
               (remix:def
                (remix:#%brackets static-interface name)
                (remix:#%brackets #:alloc name-alloc)
                (remix:#%brackets #:set name-set)
                (remix:#%brackets #:= name-set)
                mutation-interface ...
                (remix:#%brackets all-f . all-f-si-rhs)
                ...
                #:extensions
                #:methods gen:layout
                [(define (layout-planner-id _)
                   #'stx-the-planner-id)
                 (define (layout-field->acc _)
                   f->acc)]))))]))]))

(provide (rename-out [phase0:layout layout])
         (for-syntax gen:layout
                     layout?
                     gen:layout-planner
                     layout-planner?
                     layout-planner-mutable?)
         layout-immutable
         layout-mutable)
