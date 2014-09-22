#lang racket/base

(define (:read ip) (:read-syntax #f ip))

(define (:read-syntax name ip) 
  eof)

(provide
 (rename-out [:read read]
             [:read-syntax read-syntax]))
