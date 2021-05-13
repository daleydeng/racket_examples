#lang racket

(require
 "common.rkt"
 "op_table.rkt"
 )

(provide
 tag-polar
 make-mag-aug
 make-real-imag
 install-complex-polar-package
 )

(define tag 'polar)

(define (tag-polar x) (attach-tag tag x))

(define (make-mag-ang r a)
  (cons r a))

(define (make-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (install-complex-polar-package)
  (op-put! 'real-part (list tag) real-part)
  (op-put! 'imag-part (list tag) imag-part)
  (op-put! 'magnitude (list tag) magnitude)
  (op-put! 'angle (list tag) angle)
  (display "complex polar package has been installed\n"))
