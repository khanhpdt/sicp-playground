#lang racket

(provide append-lists append-item)

(define (append-item item l)
  (cons item l))

(define (append-lists list1 list2)
  (if (null? list1)
      list2
      (append-item (car list1) (append-lists (cdr list1) list2))))
  
