#lang planet neil/sicp

(define (span-zero? x)
  (and (<= (lower-bound x) 0) (>= (upper-bound x) 0)))

(define (zero-checked-div-interval x y)
  (if (span-zero? y)
      (error "Divides span-zero interval!")
      (div-interval x y)))
