#lang planet neil/sicp

(define (fast-fib n)
  (define (internal-fast-fib a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (internal-fast-fib
            a
            b
            (+ (* p p) (* q q)) ; p'
            (+ (* 2 p q) (* q q)) ; q'
            (/ count 2)))
          (else
           (internal-fast-fib
            (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- count 1)))))
  (internal-fast-fib 1 0 0 1 n))
