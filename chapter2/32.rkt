#lang racket

(require "../list-utils.rkt")

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append-lists rest (map (lambda (rest-sublist)
                                  (append-item (car s) rest-sublist))
                                rest)))))
;; This algorithm works because subsets of, e.g., a set of (a,b,c) is composed of the
;; subsets of set (a,b) and the subsets created by appending c into those subsets of the set (a,b).

(module+ test
  (require rackunit)
  (define s (list 1 2 3))
  (check-equal? (subsets s) (list null (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))))
