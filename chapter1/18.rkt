#lang planet neil/sicp

(define (fast-mult-iter a b)
  ; loop invariant: a*b + c
  (define (internal-fast-mult-iter a b c)
    (cond ((= a 1) (+ b c))
          ((even? a) (internal-fast-mult-iter (halve a) (double b) c))
          (else (internal-fast-mult-iter (- a 1) b (+ c b)))))
  (internal-fast-mult-iter a b 0))
