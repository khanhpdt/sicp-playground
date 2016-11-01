#lang planet neil/sicp

(define (fast-expt-iter b n)
  ; loop invariant: a*(b^n)
  (define (internal-fast-expt-iter b n a)
    (cond ((= n 0) a)
          ; this is what makes this exponentation procedure fast
          ((even? n) (internal-fast-expt-iter (* b b) (/ n 2) a))
          (else (internal-fast-expt-iter b (- n 1) (* a b)))))
  (internal-fast-expt-iter b n 1))

(define (even? n) (= (remainder n 2) 0))
