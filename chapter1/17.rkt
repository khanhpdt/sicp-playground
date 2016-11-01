#lang planet neil/sicp

(define (fast-mult-recursive a b)
  (cond ((= a 1) b)
        ((even? a) (double (fast-mult-recursive (halve a) b)))
        (else (+ b (fast-mult-recursive (- a 1) b)))))

(define (halve n) (/ n 2))

(define (double n) (* n 2))
