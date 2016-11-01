#lang planet neil/sicp

; v: vertical position, h: horizontal position. these positions start from 1.
(define (pascal-triangle v h)
  (cond ((or (= v 1) (= v 2)) 1)
        ((or (= h 1) (>= h v)) 1)
        (else (+ (pascal-triangle (- v 1) (- h 1)) (pascal-triangle (- v 1) h)))))
