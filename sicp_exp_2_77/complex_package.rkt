#lang racket

(require
  "common.rkt"
  "op_table.rkt"
  "complex_polar_package.rkt"
  "complex_rectangular_package.rkt"
  )

(provide
 tag-complex
 )

(define tag 'complex)

(define (tag-complex x) (attach-tag tag x))

(define (add-complex z1 z2)
  (tag-rectangular (+ (real-part z1) (real-part z2))
                   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (tag-rectangular (- (real-part z1) (real-part z2))
                   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (tag-polar (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (tag-polar (/ (magnitude z1) (magnitude z2))
             (- (angle z1) (angle z2))))

(define (install-complex-package)


  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (display "complex package has been installed\n"))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
  (display "'make-from-real-imag 'complex\n"))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
