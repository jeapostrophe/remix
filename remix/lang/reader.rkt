(module reader syntax/module-reader
  remix/core
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
                   [read-cdot #t]
                   [current-readtable
                    (make-readtable
                     #f #\; 'terminating-macro
                     (λ (ch port [src #f] [line #f] [col #f] [pos #f])
                       (define next-ch (peek-char port))
                       (cond
                         [(eq? #\; next-ch)
                          (if src
                              (read-syntax/recursive src port ch #f)
                              (read/recursive port ch #f))]
                         [else
                          (datum->syntax #f '#%semi (list src line col pos (add1 pos)))])))])
      (t)))
  (require (prefix-in at: scribble/reader)))
