(module reader syntax/module-reader
  remix/core
  #:read remix:read
  #:read-syntax remix:read-syntax
  (require (prefix-in remix: remix/stx/read)))
