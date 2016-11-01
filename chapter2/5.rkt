#lang planet neil/sicp

(define (cons-3 x y)
  (* (expt 2 x) (expt 3 y)))
;; Find x such that base base^x divides n
(define (extract-exponent n base)
  (if (= (remainder n base) 0)
      (+ (extract-exponent (/ n base) base) 1)
      0))
(define (car-3 z)
  (extract-exponent z 2))
(define (cdr-3 z)
  (extract-exponent z 3))
