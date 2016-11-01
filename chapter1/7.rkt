#lang planet neil/sicp

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define (improve-sqrt-guess x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (good-enough-sqrt-guess? x)
  (lambda (guess)
    (< (abs (- (square guess) x)) tolerance)))

(define (sqrt-iter current-guess old-guess x)
  (if ((good-enough-sqrt-guess? x) current-guess)
      current-guess
      (sqrt-iter ((improve-sqrt-guess x) current-guess) current-guess x)))
