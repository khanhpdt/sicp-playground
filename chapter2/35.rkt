#lang racket

(require "../accumulate.rkt")

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (accumulate + 0 (map count-leaves t)))))
