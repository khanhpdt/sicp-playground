#lang planet neil/sicp

(define (full-fermat-test? n)
  (define (try-it a n)
    (= (expmod a n n) a))
  (define (fermat-test-iter? a n)
    (cond ((>= a n) true)
          ((try-it a n) (fermat-test-iter? (+ a 1) n))
          (else false)))
  (fermat-test-iter? 2 n))
