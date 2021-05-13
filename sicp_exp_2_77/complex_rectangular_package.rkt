#lang racket

(require
 "common.rkt"
 "op_table.rkt"
 )

(provide
 tag-rectangular
 make-mag-aug
 make-real-imag
 install-complex-rectangular-package
 )

(define tag 'rectangular)

(define (tag-rectangular x) (attach-tag tag x))

(define (make-real-imag x y) (cons x y))

(define (make-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

(define (angle z)
    (atan (imag-part z) (real-part z)))

(define (install-complex-rectangular-package)
  (op-put! 'real-part (list tag) real-part)
  (op-put! 'imag-part (list tag) imag-part)
  (op-put! 'magnitude (list tag) magnitude)
  (op-put! 'angle (list tag) angle)
  (display "complex rectangular package has been installed\n"))
