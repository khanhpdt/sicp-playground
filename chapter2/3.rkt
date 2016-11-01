#lang planet neil/sicp

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
