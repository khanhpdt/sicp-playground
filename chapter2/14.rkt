#lang planet neil/sicp

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
