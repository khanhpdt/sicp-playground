#lang planet neil/sicp

(define (accumulate combiner base-value a b eval next)
  (if (> a b)
      base-value
      (combiner (eval a) (accumulate combiner base-value (next a) b eval next))))

(define (accumulate-sum a b eval next)
  (accumulate sum-two 0 a b eval next))

(define (sum-two a b) (+ a b))

(define (accumulate-product a b eval next)
  (accumulate product-two 1 a b eval next))

(define (product-two a b) (* a b))

(define (accumulate-iter combiner base-value a b eval next)
  (define (internal-accumulate-iter a result)
    (if (> a b)
        result
        (internal-accumulate-iter (next a) (combiner result (eval a)))))
  (internal-accumulate-iter a base-value))

(define (accumulate-sum-iter a b eval next)
  (accumulate-iter sum-two 0 a b eval next))
