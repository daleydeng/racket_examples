#lang racket

(require
 "common.rkt"
 "op_table.rkt"
 )

(provide tag-number install-number-package)

(define tag 'number)
(define (tag-number x) (attach-tag tag x))

(define (install-number-package)
  (op-put! 'add (list tag tag) (lambda (x y) (tag-number (+ x y))))
  (op-put! 'sub (list tag tag) (lambda (x y) (tag-number (- x y))))
  (op-put! 'mul (list tag tag) (lambda (x y) (tag-number (* x y))))
  (op-put! 'div (list tag tag) (lambda (x y) (tag-number (/ x y))))
  (display "number package installed\n")
  )
