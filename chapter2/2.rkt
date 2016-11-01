#lang planet neil/sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment seg)
  (define (avg a b) (/ (+ a b) 2))
  (let ((x-point (avg (x-point (start-segment seg)) (x-point (end-segment seg))))
        (y-point (avg (y-point (start-segment seg)) (y-point (end-segment seg)))))
    (make-point x-point y-point)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
