#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/generic
                     racket/syntax
                     syntax/parse)
         syntax/parse/define
         remix/stx/singleton-struct0
         racket/stxparam)

;; xxx add extensibility
;; xxx add case where x itself is a def transformer
(define-syntax (def stx)
  (syntax-parse stx
    [(_ x:id . body:expr)
     (syntax/loc stx
       (define x (remix-block . body)))]
    [(_ (x . args:expr) . body:expr)
     (syntax/loc stx
       (def x (remix-λ args . body)))]))

(define-syntax (def* stx)
  (raise-syntax-error 'def* "illegal outside of block" stx))

;; xxx add extensibility
(define-syntax (def*-internal stx)
  (syntax-parse stx
    [(_ (x:id . def-body:expr) bind-body:expr)
     (syntax/loc stx
       (let ([x (remix-block . def-body)])
         (remix-block . bind-body)))]
    [(_ ((x . args:expr) . def-body:expr) bind-body:expr)
     (syntax/loc stx
       (def*-internal (x (remix-λ args . def-body)) bind-body))]))

(define-syntax (remix-block stx)
  (syntax-parse stx
    #:literals (def*)
    [(_ (~and (~not (def* . _)) before:expr) ...
        (def* . def*-body:expr) . after:expr)
     (syntax/loc stx
       (let ()
         before ...
         (def*-internal def*-body after)))]
    [(_ . body:expr)
     (syntax/loc stx
       (let () . body))]))

(define-syntax (#%brackets stx)
  (syntax-parse stx
    [(_ . body:expr)
     (syntax/loc stx
       (remix-block . body))]))

(begin-for-syntax
  (define-generics binary-operator
    (binary-operator-precedence binary-operator))
  (define (operator-chars? s)
    (not
     (ormap (λ (c) (or (char-alphabetic? c)
                       (char-numeric? c)))
            (string->list s))))
  (define-syntax-class operator-sym
    (pattern op:identifier
             #:when (operator-chars? (symbol->string (syntax->datum #'op)))))
  (define PRECEDENCE-TABLE
    (hasheq '* 30 '/ 30
            '+ 40 '- 40
            '< 60 '<= 60
            '> 60 '>= 60
            '= 70 '≙ 70 '≙* 70))
  (define (shunting-yard:precendence op)
    (define v (syntax-local-value op (λ () #f)))
    (or (and v (binary-operator? v) (binary-operator-precedence v))
        (hash-ref PRECEDENCE-TABLE (syntax->datum op) 150)))

  (define (shunting-yard:consume-input input output operators)
    (match input
      ['()
       (shunting-yard:pop-operators output operators)]
      [(cons token input)
       (syntax-parse token
         #:literals (unquote)
         [(~or (unquote (~and op1:expr (~not _:operator-sym))) op1:operator-sym)
          (define-values (output-p operators-p)
            (shunting-yard:push-operator output operators #'op1))
          (shunting-yard:consume-input input output-p operators-p)]
         [(~or (unquote arg:operator-sym) arg:expr)
          (shunting-yard:consume-input input (cons #'arg output) operators)])]))
  (define (shunting-yard:push-operator output operators op1)
    (match operators
      ['()
       (values output (cons op1 operators))]
      [(cons op2 operators-p)
       (cond
         [(<= (shunting-yard:precendence op2) (shunting-yard:precendence op1))
          (shunting-yard:push-operator
           (shunting-yard:push-operator-to-output op2 output)
           operators-p op1)]
         [else
          (values output (cons op1 operators))])]))
  (define (shunting-yard:pop-operators output operators)
    (match operators
      ['()
       (match output
         [(list result)
          result]
         [_
          (error 'shunting-yard:pop-operators "Too much output: ~v" output)])]
      [(cons op operators)
       (shunting-yard:pop-operators
        (shunting-yard:push-operator-to-output op output)
        operators)]))
  (define (shunting-yard:push-operator-to-output op output)
    (syntax-parse output
      [(arg2:expr arg1:expr output:expr ...)
       (cons (quasisyntax/loc op
               (#,op arg1 arg2))
             (syntax->list
              #'(output ...)))])))
(define-syntax (#%braces stx)
  (syntax-parse stx
    [(_ input-tokens:expr ...)
     (shunting-yard:consume-input
      (syntax->list #'(input-tokens ...))
      empty
      empty)]))

(begin-for-syntax
  (define-generics dot-transformer
    (dot-transform dot-transformer stx)))
(define-syntax (#%dot stx)
  (syntax-parse stx
    #:literals (#%dot)
    [(_ x:expr ... (#%dot y:expr ...))
     (syntax/loc stx
       (#%dot x ... y ...))]
    [(_ dt . more:expr)
     #:declare dt (static dot-transformer? "dot transformer")
     (dot-transform (attribute dt.value) stx)]))

(begin-for-syntax
  (define-syntax-class remix-λ-raw-arg
    #:attributes (λ-arg λ-bind)
    #:literals (#%brackets)
    ;; xxx add a case where x is a def transformer
    (pattern x:id
             #:attr λ-arg (syntax x)
             #:attr λ-bind '())
    ;; xxx write a test for this
    (pattern (~and def-lhs:expr (#%brackets . _))
             #:with x (generate-temporary #'def-lhs)
             #:attr λ-arg #'x
             #:attr λ-bind (list #'(def def-lhs x))))
  (define-syntax-class remix-λ-maybe-def-arg
    #:attributes (λ-arg λ-bind)
    ;; xxx write a test for this
    (pattern x:remix-λ-raw-arg
             #:attr λ-arg #'x.λ-arg
             #:attr λ-bind (attribute x.λ-bind))
    ;; xxx write a test for this
    (pattern (x:remix-λ-raw-arg default:expr)
             #:attr λ-arg #'(x.λ-arg default)
             #:attr λ-bind (attribute x.λ-bind)))
  (define-splicing-syntax-class remix-λ-arg
    #:attributes ([λ-arg 1] λ-bind)
    ;; xxx write a test for this
    (pattern (~seq x:remix-λ-maybe-def-arg)
             #:attr [λ-arg 1] (list #'x.λ-arg)
             #:attr λ-bind (attribute x.λ-bind))
    ;; xxx write a test for this
    (pattern (~seq kw:keyword x:remix-λ-maybe-def-arg)
             #:attr [λ-arg 1] (list #'kw #'x.λ-arg)
             #:attr λ-bind (attribute x.λ-bind)))
  (define-syntax-class remix-λ-args
    #:attributes (λ-args
                  [λ-binds 1])
    ;; xxx write a test for this
    (pattern ()
             #:attr λ-args (syntax ())
             #:attr [λ-binds 1] '())
    ;; xxx write a test for this
    (pattern x:remix-λ-raw-arg
             #:attr λ-args (syntax x.λ-arg)
             #:attr [λ-binds 1] (list #'x.λ-bind))
    ;; xxx write a test for this
    (pattern (x:remix-λ-arg . xs:remix-λ-args)
             #:attr λ-args
             #'(x.λ-arg ... . xs.λ-args)
             #:attr [λ-binds 1]
             (append (attribute x.λ-bind)
                     (attribute xs.λ-binds)))))

(define-syntax/singleton-struct remix-λ
  #:property prop:procedure
  (λ (_ stx)
    (syntax-parse stx
      [(_ args:remix-λ-args . body:expr)
       (syntax/loc stx
         (λ args.λ-args (remix-block args.λ-binds ... (remix-block . body))))]))
  #:methods gen:dot-transformer
  [(define (dot-transform _ stx)
     (syntax-parse stx
       [(_#%dot _λ body:expr)
        (syntax/loc stx
          (remix-cut body))]
       ;; xxx test this
       [(_#%dot _λ bodies:expr ...)
        (syntax/loc stx
          (remix-cut (#%dot bodies ...)))]))])

(define-syntax-parameter remix-cut-$
  (λ (stx)
    (raise-syntax-error '$ "illegal outside cut" stx)))
(define-syntax (remix-cut stx)
  (syntax-parse stx
    [(_ body:expr)
     (syntax/loc stx
       (remix-λ (x)
                (syntax-parameterize ([remix-cut-$ (make-rename-transformer #'x)])
                  body)))]))

(define-syntax (remix-cond stx)
  (syntax-parse stx
    #:literals (#%brackets)
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets #:else . answer-body:expr))
     (syntax/loc stx
       (remix-block before ... . answer-body))]
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets question:expr . answer-body:expr)
        . more:expr)
     (syntax/loc stx
       (remix-block before ...
                    (if question
                        (remix-block . answer-body)
                        (remix-cond . more))))]))

(provide def def*
         (rename-out [def ≙]
                     [def* ≙*])
         (rename-out [remix-λ λ]
                     [remix-cond cond]
                     [remix-cut-$ $])
         #%brackets
         #%braces
         (for-syntax gen:binary-operator
                     binary-operator?
                     binary-operator-precedence)
         #%dot
         #%app
         #%datum
         quote
         unquote
         module
         module*
         module+
         provide)
