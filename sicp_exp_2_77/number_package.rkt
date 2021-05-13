#lang racket

(require
 "common.rkt"
 "op_table.rkt"
 )

(provide tag-number install-number-package)

(define tag 'number)

(define (tag-number x) (attach-tag tag x))
(define (untag x)
  (if (eq? (get-tag x) 'number)
      (get-content x)
      (error "input is not number")))

(define (install-number-package)
  (op-put! 'add (list tag tag)
           (lambda (x y) (tag-number (+ (untag x) (untag y)))))

  (op-put! 'sub (list tag tag)
           (lambda (x y) (tag-number (- (untag x) (untag y)))))

  (op-put! 'mul (list tag tag)
           (lambda (x y) (tag-number (* (untag x) (untag y)))))

  (op-put! 'div (list tag tag)
           (lambda (x y) (tag-number (/ (untag x) (untag y)))))

  (display "number package installed\n")
  )
