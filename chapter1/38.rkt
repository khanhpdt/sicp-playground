#lang planet neil/sicp

; approximate the base of the natural algorithm
(define (approximate-e)
  (define (n i) 1.0)
  (define (d i)
    (cond ((= i 2) 2)
          ((divides? (+ i 1) 3) (* 2 (/ (+ i 1) 3)))
          (else 1)))
  (+ 2 (cont-frac-iter n d 100)))
