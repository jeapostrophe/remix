#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         remix/required-helper
         racket/default-in)

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
