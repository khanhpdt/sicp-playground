#lang planet neil/sicp

(define (make-center-percent c p)
  (let ((cp (* c (/ p 100.0))))
    (make-interval (- c cp)
                   (+ c cp))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percentage x)
  (* (/ (width x) (center x)) 100.0))
