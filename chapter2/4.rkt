#lang planet neil/sicp

(define (cons-2 x y)
  (lambda (m) (m x y)))
(define (car-2 z)
  (z (lambda (p q) p)))
(define (cdr-2 z)
  (z (lambda (p q) q)))
