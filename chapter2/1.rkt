#lang planet neil/sicp

(define (make-rat n d)
  (cond ((= d 0) (error "Zero denominator."))
        ((= n 0) 0)
        ((and (> n 0) (> d 0)) (cons n d))
        ((and (> n 0) (< d 0)) (cons (- n) (- d)))
        ((and (< n 0) (> d 0)) (cons n d))
        ((and (< n 0) (< d 0)) (cons (- n) (- d)))))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))
