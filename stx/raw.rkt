#lang racket/base
(require racket/contract/base
         racket/match)

(define (syntax-strings->input-port name first-ss)
  (define line 1)
  (define col 0)
  (define pos 1)
  (define current-idx #f)
  (define current-bs #f)
  (define next-ss first-ss)

  (define (consume-ss!)
    (match next-ss
      ['() (void)]
      [(cons ss more-ss)     
       (set! line (syntax-line ss))
       (set! col (syntax-column ss))
       (set! pos (syntax-position ss))
       (set! current-bs (string->bytes/utf-8 (syntax->datum ss)))
       (set! current-idx 0)
       (set! next-ss more-ss)]))

  (consume-ss!)

  (define (read-in bs)
    (cond
      [(not current-bs)
       (match next-ss
         ['() eof]
         [(cons ss more-ss)
          (consume-ss!)
          (read-in bs)])]
      [(< current-idx (bytes-length current-bs))
       (define how-many
         (min (bytes-length bs)
              (- (bytes-length current-bs)
                 current-idx)))
       (define end (+ current-idx how-many))
       (bytes-copy! bs 0 current-bs current-idx end)
       (set! current-idx end)
       (set! col (+ col how-many))
       (set! pos (+ pos how-many))
       (unless (< current-idx (bytes-length current-bs))
         (consume-ss!))
       how-many]
      [else
       (set! current-bs #f)
       (read-in bs)]))
  (define (get-location)
    (values line col pos))

  (parameterize ([port-count-lines-enabled #t])
    (make-input-port name read-in #f void #f #f
                     get-location void #f #f)))

(provide
 (contract-out
  [syntax-strings->input-port
   (-> any/c
       (listof syntax?)
       input-port?)]))
