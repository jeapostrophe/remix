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
                   [read-cdot #t])
      (t)))
  (require (prefix-in at: scribble/reader)))
