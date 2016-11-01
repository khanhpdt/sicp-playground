#lang planet neil/sicp

(define (miller-rabin-test? n)
  (define (miller-rabin-test-iter a n)
    (cond ((>= a n) true)
          ((try-it a n) (miller-rabin-test-iter (+ a 1) n))
          (else false)))
  (define (try-it a n)
    ; in case of non-trivial square, this will evaluates to false anyway
    (= (expmod a (- n 1) n) 1))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder-square (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (remainder-square s m)
    ; return 0 as the special signal
    (if (non-trivial-square? s m) 0 (remainder s m)))
  (define (non-trivial-square? s m)
    (and (not (= s 1))
         ; note that s is the square of the number under consideration. that's
         ; why we compare it to the square of (m - 1)
         (not (= s (square (- m 1))))
         (= (remainder s m) 1)))
  (miller-rabin-test-iter 2 n))
