#lang racket

(require "../math-utils.rkt")

(define (square-tree-directly tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-directly (car tree))
                    (square-tree-directly (cdr tree))))))

(define (square-tree-with-map tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (square subtree)
             (square-tree-with-map subtree)))
        tree))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (f subtree)
             (tree-map f subtree)))
       tree))

(define (square-tree-2 tree)
  (tree-map square tree))

(module+ test
  (require rackunit)
  (define l (list (list 1 2) (list 3 (list 4 5)) 6 7))
  (define expected (list (list 1 4) (list 9 (list 16 25)) 36 49))
  (check-equal? (square-tree-directly l) expected)
  (check-equal? (square-tree-with-map l) expected)
  (check-equal? (square-tree-2 l) expected))
