#lang planet neil/sicp

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated-smooth f n)
  (lambda (x) (repeated (smooth f) n)))
