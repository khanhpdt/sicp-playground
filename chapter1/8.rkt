#lang planet neil/sicp

(define (cube-root current-guess old-guess x)
  (define (close-enough? current-guess old-guess)
    (< (abs (- current-guess old-guess)) (/ current-guess 1000)))
  (define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (if (close-enough? current-guess old-guess)
      current-guess
      (cube-root (improve current-guess x) current-guess x)))
