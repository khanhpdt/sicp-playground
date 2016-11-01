#lang planet neil/sicp

(define (no-more? l)
  (null? l))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

;; The order of coin-values does not affect the result of cc, because cc only cares about the value
;; of the coins, not their orders in the coin-values list.
