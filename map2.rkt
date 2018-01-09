#lang racket

(provide map2)

(define (map2 proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
