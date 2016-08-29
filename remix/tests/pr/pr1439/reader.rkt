#lang s-exp syntax/module-reader
"lang.rkt"
#:wrapper1 (lambda (thunk)
             (parameterize ([read-cdot #true])
               (thunk)))
