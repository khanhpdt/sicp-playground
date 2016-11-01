#lang planet neil/sicp

(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (square x))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac-iter n d k))
