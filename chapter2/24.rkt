#lang planet neil/sicp

(define exercise-2-24 (list 1 (list 2 (list 3 4))))
;; -> (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))
;; It seems that "list" always transforms its second argument to a pair by
;; appending nil to the argument.
