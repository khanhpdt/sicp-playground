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

;; Exercise 2.7
(define (make-interval x y) (cons x y))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; Exercise 2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))
    
;; Exercise 2.9
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; Exercise 2.10
(define (span-zero? x)
  (and (<= (lower-bound x) 0) (>= (upper-bound x) 0)))

(define (zero-checked-div-interval x y)
  (if (span-zero? y)
      (error "Divides span-zero interval!")
      (div-interval x y)))

;; Exercise 2.11
(define (mul-interval-2 x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (< ux 0) (< uy 0)) (make-interval (* ux uy)
                                                  (* lx ly)))
          ((and (< ux 0) (>= uy 0) (< ly 0)) (make-interval (* lx uy)
                                                            (* lx ly)))
          ((and (< ux 0) (>= uy 0) (>= ly 0)) (make-interval (* lx uy)
                                                             (* ux ly)))
          ((and (>= ux 0) (< lx 0) (< uy 0)) (make-interval (* ux ly)
                                                            (* lx ly)))
          ((and (>= ux 0) (< lx 0) (>= uy 0) (< ly 0)) (make-interval (min (* lx uy) (* ux ly))
                                                                      (max (* lx ly) (* ux uy))))
          ((and (>= ux 0) (< lx 0) (>= uy 0) (>= ly 0)) (make-interval (* lx uy)
                                                                       (* ux uy)))
          ((and (>= ux 0) (>= lx 0) (< uy 0)) (make-interval (* ux ly)
                                                             (* lx uy)))
          ((and (>= ux 0) (>= lx 0) (>= uy 0) (< ly 0)) (make-interval (* ux ly)
                                                                       (* ux uy)))
          ((and (>= ux 0) (>= lx 0) (>= uy 0) (>= ly 0)) (make-interval (* lx ly)
                                                                        (* ux uy))))))

;; Exercise 2.12
(define (make-center-percent c p)
  (let ((cp (* c (/ p 100.0))))
    (make-interval (- c cp)
                   (+ c cp))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percentage x)
  (* (/ (width x) (center x)) 100.0))

;; Exercise 2.13
(define (percentage-mul-interval x y)
  (let ((p1 (percentage x))
        (p2 (percentage y)))
    (/ (+ p1 p2) (+ (* (/ p1 100) (/ p2 100)) 1))))

;; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;; Try these:
;; (div-interval (make-center-width 100 0.1) (make-center-width 100 50))
;; -> (mcons 0.666 2.002)
;; (div-interval (make-center-width 100 10) (make-center-width 100 50))
;; -> (mcons 0.6000000000000001 2.2)
;; (div-interval (make-center-width 100 20) (make-center-width 100 50))
;; -> (mcons 0.5333333333333333 2.4)
;; We can see that as the width of the dividend increases, the result becomes less accurate.

;; Exercise 2.15
;; par2 is better than par1 because all of the dividends in par2 has width of 0, whereas the dividend in
;; par1 can have a big width.

;; Exercise 2.17
(define (last-pair l)
  (cond ((null? l) nil)
        ((null? (cdr l)) (cons (car l) nil))
        (else (last-pair (cdr l)))))

;; Exercise 2.18
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (reverse l) 
  (if (null? l)
      nil
      (append (reverse (cdr l)) (cons (car l) nil))))

;; Exercise 2.19
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

;; Exercise 2.20
(define (same-parity . numbers)
  (define (same-parity-with n others)
    (cond ((null? others) nil)
          ((= (remainder n 2) (remainder (car others) 2)) (cons (car others)
                                                                (same-parity-with n (cdr others))))
          (else (same-parity-with n (cdr others)))))
  (same-parity-with (car numbers) (cdr numbers)))
        
