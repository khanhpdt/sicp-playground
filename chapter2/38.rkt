#lang racket

(require "../accumulate.rkt")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; (fold-left / 1 (list 1 2 3)) ;; 1/6
;; (accumulate / 1 (list 1 2 3)) ;; 3/2
;; (fold-left list null (list 1 2 3)) ;; '(((() 1) 2) 3)
;; (accumulate list null (list 1 2 3)) ;; '(1 (2 (3 ())))

;; fold-right and fold-left will produce the same result if op
;; satisfies: (= (op x1 x2) (op x2 x1))