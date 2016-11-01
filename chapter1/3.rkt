#lang planet neil/sicp

(define (sum-of-squares x y z)
  (cond ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
        ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
        ((and (>= x z) (>= y z)) (+ (* x x) (* y y)))))
