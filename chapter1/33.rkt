#lang planet neil/sicp

(define (filtered-accumulate combiner base-value a b eval next predicate?)
  (cond ((> a b) base-value)
        ((predicate? a) (combiner (eval a) (filtered-accumulate combiner base-value (next a) b eval next predicate?)))
        (else (filtered-accumulate combiner base-value (next a) b eval next predicate?))))

(define (sum-primes a b)
  (filtered-accumulate sum-two 0 a b identity increase prime?))

(define (sum-primes-relative-to n)
  (define (relative-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate product-two 1 1 n identity increase relative-prime?))

(define (gcd a b)
  (if (divides? a b) b (gcd b (remainder a b))))
