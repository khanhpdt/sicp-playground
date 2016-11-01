#lang planet neil/sicp

(define (append-list l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append-list (cdr l1) l2))))

(define (reverse l)
  (if (null? l)
      nil
      (append-list (reverse (cdr l)) (cons (car l) nil))))
