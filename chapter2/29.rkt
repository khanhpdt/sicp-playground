#lang planet neil/sicp

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (check-equals value expected)
  (if (= value expected)
      (display "OK")
      (error (string-append (number->string value)
                            " not equals "
                            (number->string expected)))))

(define (total-weight-test)
  (let ((m (make-mobile (make-branch 1 1)
                        (make-branch 2 (make-mobile (make-branch 1 1)
                                                    (make-branch 1 1))))))
    (check-equals (total-weight m) 3)))

(define (balanced-mobile? mobile)
  (define (branch-equals? left right)
    (= (* (branch-length left) (total-weight (branch-structure left)))
       (* (branch-length right) (total-weight (branch-structure right)))))
  (if (not (pair? mobile))
      true
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (branch-equals? left right)
             (balanced-mobile? (branch-structure left))
             (balanced-mobile? (branch-structure right))))))

(define (check-true value)
  (if value
      (display "OK")
      (error ("Not a true value."))))

(define (balanced-mobile-test-1)
  (let ((m (make-mobile (make-branch 4 1)
                        (make-branch 2 (make-mobile (make-branch 1 1)
                                                    (make-branch 1 1))))))
    (check-true (balanced-mobile? m))))

(define (check-false value)
  (if (not value)
      (display "OK")
      (error ("Not a false value."))))

(define (balanced-mobile-test-2)
  (let ((m (make-mobile (make-branch 4 (make-mobile (make-branch 1 2)
                                                    (make-branch 1 3)))
                        (make-branch 2 (make-mobile (make-branch 1 6)
                                                    (make-branch 1 6))))))
    (check-false (balanced-mobile? m))))

;; Exercise 2.29.d
;; We only need to modify the selectors: left-branch, right-branch, branch-length, and branch-structure.
