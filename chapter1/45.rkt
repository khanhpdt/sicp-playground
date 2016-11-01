#lang planet neil/sicp

(define (fixed-point-repeated-average-damp f n first-guess)
  (fixed-point (repeated (average-damp f) n) first-guess))

(define (n-root n x average-times)
  (fixed-point-repeated-average-damp (lambda (y) (/ x (expt y (- n 1)))) average-times 1.0))
