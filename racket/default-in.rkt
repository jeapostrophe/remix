#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     racket/match
                     racket/require-transform
                     syntax/modresolve))

(define-syntax default-in
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ m [id def] ...)
        (let ()
          (define-values (ims srcs) (expand-import #'m))
          (define ids (mutable-seteq))
          (for ([im (in-list ims)])
            (set-add! ids (import-src-sym im)))
          (define ims+
            (for/fold ([ims ims]) ([i*d (in-list (syntax->list #'([id def] ...)))])
              (match-define (list id-stx def) (syntax-e i*d))
              (match-define
                (list source-mod source-id nominal-source-mod nominal-source-id
                      source-phase import-phase nominal-export-phase)
                (or (identifier-binding def)
                    (raise-syntax-error 'default-in "default must be bound" stx def)))
              (define source-mod-path
                (with-handlers
                    ([exn:fail?
                      (λ (x)
                        ;; xxx this is stupid, but I don't know how not to do it
                        (raise-syntax-error 'default-in "default must be imported"
                                            stx def))])
                  (match (resolve-module-path-index source-mod)
                    ['#%kernel ''#%kernel]
                    [x x])))
              (define id (syntax->datum id-stx))
              (cond
                [(set-member? ids id)
                 ims]
                [else
                 (define id-im
                   (import id-stx source-id
                           source-mod-path
                           import-phase
                           0 nominal-export-phase def))
                 (cons id-im ims)])))
          (values ims+ srcs))]))))

(define-syntax (xxx-stupid-void stx)
  #'(void))

(provide default-in
         xxx-stupid-void)

(module+ test
  (define moogle displayln)
  (require (default-in racket/bool
             [snoozle displayln]

             ;; xxx Doesn't work
             #;
             [snozle moogle]

             [nozzle void]))
  (snoozle 42)
  (nozzle 1 2 3 4))
