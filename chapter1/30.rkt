#lang planet neil/sicp

(define (sum-iter a b eval next)
  (define (internal-sum-iter a accumulator)
    (if (> a b)
        accumulator
        (internal-sum-iter (next a) (+ accumulator (eval a)))))
  (internal-sum-iter a 0))
