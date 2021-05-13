#lang racket

(require
 racket/include
 racket/trace
 rackunit

 "common.rkt"
 "op_table.rkt"
 "number_package.rkt"
 "rational_package.rkt"
)

(install-number-package)
(install-rational-package)

(check-equal? (op-get 'a 'b) #f)
(op-put! 'a 'b 1)
(check-equal? (op-get 'a 'b) 1)
(op-del! 'a 'b)
(check-equal? (op-get 'a 'b) #f)

(define t (attach-tag 'a 1))
(check-equal? (get-tag t) 'a)
(check-equal? (get-contents t) 1)

(check-equal? (apply-generic 'add (tag-number 1) (tag-number 2))
              (tag-number 3))

(check-equal? (apply-generic 'add (tag-rational (make-rat 1 2))
                             (tag-rational (make-rat 3 4)))
              (tag-rational (make-rat 5 4)))
