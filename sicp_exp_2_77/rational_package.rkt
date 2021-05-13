#lang racket

(require
 "common.rkt"
 "op_table.rkt"
 )

(provide
 make-rat
 tag-rational
 install-rational-package
 )

(define tag 'rational)
(define (tag-rational x) (attach-tag tag x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (install-rational-package)
  (op-put! 'add (list tag tag)
           (lambda (x y) (tag-rational (add-rat x y))))
  (op-put! 'sub (list tag tag)
           (lambda (x y) (tag-rational (sub-rat x y))))
  (op-put! 'mul (list tag tag)
           (lambda (x y) (tag-rational (mul-rat x y))))
  (op-put! 'div (list tag tag)
           (lambda (x y) (tag-rational (div-rat x y))))
  (display "rational package has been installed\n"))
