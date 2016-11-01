#lang planet neil/sicp

(define (square-list-3 l)
  (define (iter l answer)
    (if (null? l)
        answer
        (iter (cdr l)
              (cons (square (car l)) answer))))
  (iter l nil))
;; This algorithm square-list-3 produces a list in reverse order
;; because (cons (square (car l)) answer) appends the tail of the
;; given list to the head of the result.

(define (square-list-4 l)
  (define (iter l answer)
    (if (null? l)
        answer
        (iter (cdr l)
              (cons answer (square (car l))))))
  (iter l nil))
;; This algorithm square-list-4 doesn't work because (cons answer (square (car l))
;; creates a pair whose first item is a pair and second item is a number, and
;; this format is not a list format. For example, (square-list-4 (list 1 2 3))
;; produces (mcons (mcons (mcons '() 1) 4) 9).
