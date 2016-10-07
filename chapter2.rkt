#lang planet neil/sicp

;; Exercise 2.1
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

;; Exercise 2.2
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

;; Exercise 2.3

; rectangle representation by its left and right edges
(define (make-rect left-edge right-edge) (cons left-edge right-edge))
(define (left-edge rect) (car rect))
(define (right-edge rect) (cdr rect))

(define (get-width rect)
  (abs (- (y-point (start-segment (left-edge rect))) (y-point (end-segment (left-edge rect))))))
(define (get-length rect)
  (abs (- (x-point (start-segment (left-edge rect))) (x-point (start-segment (right-edge rect))))))

; alternative rectangle representations: representation by its upper and lower edges, or by its four points

(define (perimeter rect)
  (* 2 (+ (get-width rect) (get-length rect))))
(define (area rect)
  (* (get-width rect) (get-length rect)))

;; Exercise 2.4
(define (cons-2 x y)
  (lambda (m) (m x y)))
(define (car-2 z)
  (z (lambda (p q) p)))
(define (cdr-2 z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (cons-3 x y)
  (* (expt 2 x) (expt 3 y)))
;; Find x such that base base^x divides n
(define (extract-exponent n base)
  (if (= (remainder n base) 0)
      (+ (extract-exponent (/ n base) base) 1)
      0))
(define (car-3 z)
  (extract-exponent z 2))
(define (cdr-3 z)
  (extract-exponent z 3))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
; n here is a Church number
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero) expansion
; (add-1 zero)
; -> (lambda (f) (lambda (x) (f (((lambda (f2) (lambda (x2) x2)) f) x))))
; -> (lambda (f) (lambda (x) (f ( (lambda (x2) x2) x))))
; -> (lambda (f) (lambda (x) (f ( x ))))
(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one) expansion
; (add-1 one)
; -> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; -> (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; -> (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (church->int z)
  ((z (lambda (x) (+ x 1))) 0))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x))))) ; (n f) means applying f for n times


