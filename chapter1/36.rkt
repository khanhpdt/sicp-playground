#lang planet neil/sicp

; This solves f(x) = x
(define (fixed-point f first-guess)
  (define (internal-impl guess step-counter)
    (if (> step-counter 10000) (error "No converge."))
    (println (number->string step-counter) ": " (number->string guess))
    (let ((next-guess (f guess)))
      (if (close-enough? next-guess guess)
          guess
          (internal-impl next-guess (+ step-counter 1)))))
  (internal-impl first-guess 1))

(define (println . strings)
  (apply print strings)
  (newline))

(define (print . strings) (display (apply string-append strings)))

(define (average-damp f)
  (lambda (guess) (average guess (f guess))))

(define (fixed-point-average-damp f first-guess)
  (fixed-point (average-damp f) first-guess))
