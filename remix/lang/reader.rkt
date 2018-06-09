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
