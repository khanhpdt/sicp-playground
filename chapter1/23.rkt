#lang planet neil/sicp

(define (smart-smallest-divisor n)
  (define (next-divisor d) (if (= d 2) 3 (+ d 2)))
  (define (internal-smart-smallest-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          ; no need to check for multiples of 2 when we know that 2 is not the divisor
          (else (internal-smart-smallest-divisor (next-divisor test-divisor)))))
  (internal-smart-smallest-divisor 2))

(define (timed-smart-prime-test n)
  (newline)
  (display n)
  (start-smart-prime-test n (runtime)))

(define (start-smart-prime-test n start-time)
  (if (smart-prime? n) (report-prime (- (runtime) start-time))))

(define (smart-prime? n)
  (= (smart-smallest-divisor n) n))
