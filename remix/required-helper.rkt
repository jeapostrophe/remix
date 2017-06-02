#lang racket/base
(require (for-syntax racket/base))
(define-syntax (default-#%required stx)
  #'(void))
(provide default-#%required)
