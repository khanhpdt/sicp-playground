#lang planet neil/sicp

(define (percentage-mul-interval x y)
  (let ((p1 (percentage x))
        (p2 (percentage y)))
    (/ (+ p1 p2) (+ (* (/ p1 100) (/ p2 100)) 1))))
