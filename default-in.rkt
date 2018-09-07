#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     racket/list
                     racket/match
                     racket/require-transform
                     syntax/parse
                     syntax/modcollapse))

(define-syntax default-in
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       ;; xxx m should use :module-path, which should be part of syntax/parse
       [(_ m [in:id def:id] ...)
        (define ins (make-hasheq))
        (for ([i*d (in-list (syntax->list #'([in . def] ...)))])
          (match-define (cons in-stx _) (syntax-e i*d))
          (define in (syntax-e in-stx))
          (when (hash-has-key? ins in)
            (raise-syntax-error 'default-in "duplicate import" stx in-stx))
          (hash-set! ins in i*d))

        (define-values (ims srcs) (expand-import #'m))
        (for ([im (in-list ims)])
          (hash-remove! ins (import-src-sym im)))

        (define ims+
          (for/fold ([ims ims]) ([(in i*d) (in-hash ins)])
            (match-define (cons in-stx def) (syntax-e i*d))
            (match-define
              (list source-mod source-id nominal-source-mod nominal-source-id
                    source-phase import-phase nominal-export-phase)
              (or (identifier-binding def)
                  (raise-syntax-error 'default-in "default must be bound" stx def)))
            (define source-mod-path
              ;; xxx remove let/ec when c-m-p-i's contract is updated
              (let/ec esc
                (collapse-module-path-index
                 source-mod
                 (λ ()
                   (esc #f)))))
            (cond
              [source-mod-path
               (cons
                (import in-stx source-id
                        source-mod-path
                        import-phase
                        0 nominal-export-phase def)
                ims)]
              [else
               (syntax-local-lift-module-end-declaration
                (quasisyntax/loc stx
                  (define-syntax #,in-stx (make-rename-transformer #'#,def))))
               ims])))
        (values ims+ srcs)]))))

(provide default-in)

(module+ test
  (define moogle displayln)
  (require (default-in racket/bool
             [snoozle displayln]
             [snozle moogle]
             [nozzle void]))
  (snozle 1)
  (snoozle 42)
  (nozzle 1 2 3 4))
