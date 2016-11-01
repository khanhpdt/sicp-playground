#lang planet neil/sicp

(define (cont-frac-iter n d k)
  (define (internal-impl counter result)
    (if (= counter 0)
        result
        (internal-impl (- counter 1) (/ (n counter) (+ (d counter) result)))))
  (internal-impl k 0))

(define (cont-frac-recur n d k)
  (define (internal-impl counter)
    (if (> counter k)
        0
        (/ (n counter) (+ (d counter) (internal-impl (+ counter 1))))))
  (internal-impl 1))

(define (golden-ratio)
  (fixed-point-average-damp (lambda (x) (+ 1 (/ 1 x))) 1.0))
