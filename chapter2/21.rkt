#lang planet neil/sicp

(define (map f l)
  (if (null? l)
      nil
      (cons (f (car l)) (map f (cdr l)))))

(define (square x)
  (* x x))

(define (square-list l)
  (map square l))

(define (square-list-2 l)
  (if (null? l)
      nil
      (cons (square (car l)) (square-list-2 (cdr l)))))
