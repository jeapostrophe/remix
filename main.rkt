#lang racket/base

(module reader syntax/module-reader
  remix/main
  #:read at:read
  #:read-syntax at:read-syntax
  #:wrapper1
  (λ (t)
    (parameterize ([read-square-bracket-as-paren #f]
                   [read-curly-brace-as-paren #f]
                   [read-square-bracket-with-tag #t]
                   [read-curly-brace-with-tag #t]
                   [read-accept-dot #f]
                   [read-accept-infix-dot #f]
                   [read-cdot #t])
      (t)))
  #:info
  (λ (key defval proc)
    (define (fallback) (if proc (proc key defval) defval))
    (define (try-dynamic-require lib export)
      (with-handlers ([exn:missing-module?
                       (λ (x) (fallback))])
        (dynamic-require lib export)))
    (case key
      [(color-lexer)
       (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
      [(drracket:indentation)
       (try-dynamic-require 'scribble/private/indentation 'determine-spaces)]
      [(drracket:keystrokes)
       (try-dynamic-require 'scribble/private/indentation 'keystrokes)]
      [else (fallback)]))
  (require (prefix-in at: scribble/reader)))

(require (for-syntax racket/base
                     syntax/parse)
         remix/private/required-helper
         remix/default-in)

;; xxx remove need for local-require/local-require* by detecting context?

(define-syntax (remix-module-begin stx)
  (syntax-parse stx
    #:literals (remix-require*)
    [(_ (~and s (~not (remix-require* . _))) ...)
     (syntax/loc stx
       (#%module-begin s ...))]
    [(_ (~and pre (~not (remix-require* . _))) ...
        (remix-require* m)
        post ...)
     (syntax/loc stx
       (#%module-begin pre ... (do-remix-require* m post ...)))]))

(define-syntax (do-remix-require* stx)
  (syntax-parse stx
    [(_ m . body)
     (syntax/loc stx
       (begin (require (rename-in m [#%require*d internal-#%require*d]))
              (internal-#%require*d . body)))]))

(define-syntax (remix-require stx)
  (syntax-parse stx
    [(_ m)
     (syntax/loc stx
       (begin (require (rename-in (default-in m
                                    [#%required default-#%required])
                                  [#%required internal-#%required]))
              (internal-#%required m)))]
    [(_ m ...)
     (syntax/loc stx
       (begin (remix-require m) ...))]))

(define-syntax (remix-require* stx)
  (raise-syntax-error 'require* "illegal outside of top-level" stx))

(provide (rename-out
          [remix-module-begin #%module-begin]
          [remix-require require]
          [remix-require* require*]))
