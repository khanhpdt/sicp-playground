#lang racket

(require "../math-utils.rkt")

(define (square-tree-directly tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-directly (car tree))
                    (square-tree-directly (cdr tree))))))

(define (square-tree-with-map tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (square sub-tree)
             (square-tree-with-map sub-tree)))
        tree))
