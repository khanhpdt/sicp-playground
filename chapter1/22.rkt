#lang planet neil/sicp

; l: left bound inclusive, r: right bound inclusive
(define (search-for-primes l r)
  (define (internal-search-for-primes l r)
    (if (<= l r) (timed-prime-test l))
    (if (< l r) (internal-search-for-primes (+ l 2) r)))
  ; make sure that the first number tested is an odd number
  (internal-search-for-primes (if (even? l) (+ l 1) l) r))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n) (report-prime (- (runtime) start-time))))

(define (prime? n)
  (if (= n 1) false (= (smallest-divisor n) n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
