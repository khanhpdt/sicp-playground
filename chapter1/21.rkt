#lang planet neil/sicp

(define (smallest-divisor n)
  (define (internal-smallest-divisor d)
    (cond ((> (square d) n) n) ; the divisor of n must be less than or equal to square-root of n
          ((divides? n d) d)
          (else (internal-smallest-divisor (+ d 1)))))
  (internal-smallest-divisor 2))

(define (divides? n d) (= (remainder n d) 0))
