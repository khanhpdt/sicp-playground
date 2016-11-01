#lang planet neil/sicp

(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))))

(define (f-iterative n)
  ; fj = f(i - j), e.g., f2 = f(i - 2)
  (define (f-iter i f1 f2 f3)
    (cond ((= i n) (+ f1 (* 2 f2) (* 3 f3)))
          (else (f-iter (+ i 1) (+ f1 (* 2 f2) (* 3 f3)) f1 f2))))
  (cond ((< n 3) n)
        (else (f-iter 3 2 1 0))))
