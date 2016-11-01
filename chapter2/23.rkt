#lang planet neil/sicp

(define (for-each proc l)
  (if (null? l)
      true ;; dummy result because the result of for-each will not be used
      ((lambda ()
        (proc (car l))
        (for-each proc (cdr l))))))
