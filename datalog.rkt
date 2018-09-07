#lang racket/base
(require datalog/runtime
         (prefix-in stx: datalog/stx)
         (for-syntax racket/base
                     remix/stx/raw0
                     datalog/private/compiler
                     datalog/parse
                     syntax/parse))

(define-syntax (datalog stx)
  (syntax-parse stx
    [(_ thy:expr s:str ...)
     (with-syntax
       ([(stmt ...)
         (compile-program
          (parse-program
           (syntax-strings->input-port
            (syntax-source stx)
            (syntax->list #'(s ...)))))])
       (syntax/loc stx
         (stx:datalog thy stmt ...)))]))

(provide make-theory
         datalog)
