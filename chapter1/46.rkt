#lang planet neil/sicp

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess) guess ((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt-iter-improve x)
  ((iterative-improve (good-enough-sqrt-guess? x) (improve-sqrt-guess x)) 1.0))

(define (fixed-point-iter-improve f first-guess)
  (define (good-enough? guess)
    (close-enough? guess (f guess)))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))
