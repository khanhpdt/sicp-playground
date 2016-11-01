#lang planet neil/sicp

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (fixed-point-excercise-1-35 f guess)
  (let ((next-guess (f guess)))
    (if (close-enough? next-guess guess)
        guess
        (fixed-point f next-guess))))
