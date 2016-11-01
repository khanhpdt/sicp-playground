#lang planet neil/sicp

(define (exercise-2-25)
  (define (car-cdr l)
    (car (cdr l)))
  (let ((l1 (list 1 3 (list 5 7) 9))
        (l2 (list (list 7)))
        (l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))
    (newline)
    (display (car (cdr (car (cdr (cdr l1))))))
    (newline)
    (display (car (car l2)))
    (newline)
    (display (car-cdr (car-cdr (car-cdr (car-cdr (car-cdr (car-cdr l3)))))))))
