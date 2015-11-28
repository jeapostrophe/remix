#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/generic
                     racket/syntax
                     syntax/parse)
         syntax/quote
         syntax/parse/define
         remix/stx/singleton-struct0
         racket/stxparam)

(begin-for-syntax
  (define-generics def-transformer
    (def-transform def-transformer stx)))
(define-syntax (def stx)
  (syntax-parse stx
    #:literals (#%brackets)
    [(_ (#%brackets dt . _) . _)
     #:declare dt (static def-transformer? "def transformer")
     ;; xxx maybe this interface should be thicker, because right now
     ;; it can expand to anything at all. thicker would mean more
     ;; composable.
     (def-transform (attribute dt.value) stx)]
    ;; xxx test this
    [(_ dt . body:expr)
     #:declare dt (static def-transformer? "def transformer")
     (syntax/loc stx
       (def (#%brackets dt) . body))]
    [(_ x:id . body:expr)
     (syntax/loc stx
       (define x (remix-block . body)))]
    [(_ ((~and (~not #%brackets) x) . args:expr) . body:expr)
     (syntax/loc stx
       (def x (remix-λ args . body)))]))

(module remix-block racket/base
  (require (for-syntax racket/base
                       racket/generic
                       syntax/parse))
  (define-syntax (def* stx)
    (raise-syntax-error 'def* "illegal outside of block" stx))

  (begin-for-syntax
    (define-generics def*-transformer
      (def*-transform def*-transformer stx)))

  (define-syntax (def*-internal stx)
    (syntax-parse stx
      #:literals (#%brackets)
      ;; xxx test this
      [(_ (#%brackets dt . _) _)
       #:declare dt (static def*-transformer? "def* transformer")
       (def*-transform (attribute dt.value) stx)]
      ;; xxx test this
      [(_ (dt . def-body:expr) bind-body:expr)
       #:declare dt (static def*-transformer? "def* transformer")
       (syntax/loc stx
         (def*-internal ((#%brackets dt) . def-body) bind-body))]
      [(_ ((~and (~not #%brackets) x:id) . def-body:expr) bind-body:expr)
       (syntax/loc stx
         (let ([x (remix-block . def-body)])
           (remix-block . bind-body)))]
      [(_ (((~and (~not #%brackets) x) . args:expr) . def-body:expr) bind-body:expr)
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

  (define-syntax #%brackets
    (make-rename-transformer #'remix-block))

  (provide def*
           #%brackets
           (for-syntax def*-transformer?
                       gen:def*-transformer)
           remix-block))
(require (submod "." remix-block)
         (for-syntax (submod "." remix-block)))

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
            '= 70 '≙ 70 '≙* 70 '≡ 70))
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
    [(_ dt . _)
     #:declare dt (static dot-transformer? "dot transformer")
     (dot-transform (attribute dt.value) stx)]))

(begin-for-syntax
  (define-generics app-dot-transformer
    (app-dot-transform app-dot-transformer stx)))
(define-syntax (remix-#%app stx)
  (syntax-parse stx
    #:literals (#%dot)
    [(_ (~and dot-rator (#%dot x:expr ... (#%dot y:expr ...))) . body:expr)
     (syntax/loc stx
       (#%app (#%dot x ... y ...) . body))]
    [(_ (~and dot-rator (#%dot adt . _)) . body:expr)
     #:declare adt (static app-dot-transformer? "app-dot transformer")
     (app-dot-transform (attribute adt.value) stx)]
    [(_ . body:expr)
     (syntax/loc stx
       (#%app . body))]))

(define-syntax (#%rest stx)
  (raise-syntax-error '#%rest "Illegal outside of function arguments" stx))

(begin-for-syntax
  (define-syntax-class remix-λ-raw-arg
    #:attributes (λ-arg λ-bind)
    #:literals (#%brackets)
    ;; xxx test this
    (pattern dt
             #:declare dt (static def-transformer? "def transformer")
             #:with x (generate-temporary #'dt)
             #:attr λ-arg #'x
             #:attr λ-bind (list #'(def dt x)))
    ;; xxx test this
    (pattern dt
             #:declare dt (static def*-transformer? "def* transformer")
             #:with x (generate-temporary #'dt)
             #:attr λ-arg #'x
             #:attr λ-bind (list #'(def* dt x)))
    (pattern x:id
             #:attr λ-arg (syntax x)
             #:attr λ-bind '())
    (pattern (~and def-lhs:expr (#%brackets dt . _))
             #:declare dt (static def-transformer? "def transformer")
             #:with x (generate-temporary #'def-lhs)
             #:attr λ-arg #'x
             #:attr λ-bind (list #'(def def-lhs x)))
    ;; xxx write a test for this
    (pattern (~and def-lhs:expr (#%brackets dt . _))
             #:declare dt (static def*-transformer? "def* transformer")
             #:with x (generate-temporary #'def-lhs)
             #:attr λ-arg #'x
             #:attr λ-bind (list #'(def* def-lhs x))))
  (define-syntax-class remix-λ-maybe-def-arg
    #:attributes (λ-arg λ-bind)
    (pattern x:remix-λ-raw-arg
             #:attr λ-arg #'x.λ-arg
             #:attr λ-bind (attribute x.λ-bind))
    (pattern (x:remix-λ-raw-arg default:expr)
             #:attr λ-arg #'(x.λ-arg default)
             #:attr λ-bind (attribute x.λ-bind)))
  (define-splicing-syntax-class remix-λ-arg
    #:attributes ([λ-arg 1] λ-bind)
    (pattern (~seq x:remix-λ-maybe-def-arg)
             #:attr [λ-arg 1] (list #'x.λ-arg)
             #:attr λ-bind (attribute x.λ-bind))
    (pattern (~seq kw:keyword x:remix-λ-maybe-def-arg)
             #:attr [λ-arg 1] (list #'kw #'x.λ-arg)
             #:attr λ-bind (attribute x.λ-bind)))
  (define-syntax-class remix-λ-args
    #:attributes (λ-args
                  [λ-binds 1])
    #:literals (#%rest)
    (pattern ()
             #:attr λ-args (syntax ())
             #:attr [λ-binds 1] '())
    (pattern (~or x:remix-λ-raw-arg
                  (#%rest x:remix-λ-raw-arg))
             #:attr λ-args (syntax x.λ-arg)
             #:attr [λ-binds 1] (attribute x.λ-bind))
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

(define-syntax (impossible! stx)
  (syntax-parse stx
    [(_ fun msg loc)
     (quasisyntax/loc stx
       (raise-syntax-error fun msg
                           (quote-syntax/keep-srcloc #,#'loc)))]
    [_
     (quasisyntax/loc stx
       (raise-syntax-error '☠ "Unreachable code has been reached"
                           (quote-syntax/keep-srcloc #,stx)))]))

(define-syntax (remix-cond stx)
  (syntax-parse stx
    #:literals (#%brackets)
    [(_ . (~and (cond-arg ...)
                (_ ... (#%brackets (~not #:else) . _))))
     (quasisyntax/loc stx
       (remix-cond cond-arg ...
                   (#%brackets
                    #:else (impossible! 'cond
                                        "non-existent default case reached"
                                        #,stx))))]
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets #:else . answer-body:expr))
     (syntax/loc stx
       (remix-block before ... . answer-body))]
    [(_ (~and before:expr (~not (#%brackets . any:expr))) ...
        (#%brackets question:expr . answer-body:expr)
        . more:expr)
     (quasisyntax/loc stx
       (remix-block before ...
                    (if question
                        (remix-block . answer-body)
                        #,(syntax/loc #'more (remix-cond . more)))))]))

(provide def def*
         (for-syntax gen:def-transformer
                     gen:def*-transformer)
         (rename-out [def ≙] ;; \defs
                     [def :=]
                     [def* ≙*]
                     [def* :=*]
                     [def* nest])
         (rename-out [remix-λ λ] ;; \lambda
                     [remix-cond cond]
                     [remix-cut-$ $])
         impossible!
         (rename-out [impossible! ☠])
         #%rest
         (rename-out [remix-block block])
         #%brackets
         #%braces
         (for-syntax gen:binary-operator
                     binary-operator?
                     binary-operator-precedence)
         #%dot
         (for-syntax gen:dot-transformer)
         (rename-out [remix-#%app #%app])
         (for-syntax gen:app-dot-transformer)
         (rename-out [... …]) ;; \ldots
         #%datum
         quote
         unquote
         module
         module*
         module+
         for-syntax
         provide)

(define-syntax val
  (singleton-struct
   #:property prop:procedure
   (λ (stx)
     (raise-syntax-error 'val "Illegal outside def" stx))
   #:methods gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (#%brackets)
        [(_def (#%brackets _stx x:id) . body:expr)
         (syntax/loc stx
           (define x (remix-block . body)))]))]))

(define-syntax stx
  (singleton-struct
   #:property prop:procedure
   (λ (stx)
     (raise-syntax-error 'stx "Illegal outside def" stx))
   #:methods gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (#%brackets)
        [(_def (#%brackets _stx x:id) . body:expr)
         (syntax/loc stx
           (define-syntax x (remix-block . body)))]))]))

(define-syntax mac
  (singleton-struct
   #:property prop:procedure
   (λ (stx)
     (raise-syntax-error 'mac "Illegal outside def" stx))
   #:methods gen:def-transformer
   [(define (def-transform _ stx)
      (syntax-parse stx
        #:literals (#%brackets)
        [(_def (#%brackets _mac (x:id . pat:expr)) . body:expr)
         (syntax/loc stx
           (define-simple-macro (x . pat) . body))]))]))

(provide val
         stx
         mac)
