#lang racket

(require racket/trace)

(define (=number? x num)
    (and (number? x) (= x num)))

(define (variable? x) (symbol? x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else '(+ a1 a2))))

(define (deriv-sum operands var)
  (make-sum (deriv (car operands) var)
            (deriv (cadr operands) var)))

(define (sum-expr? expr)
  (eq? (car expr) '+))

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((variable? expr) (if (eq? expr var) 1 0))
          ((sum-expr? expr) (deriv-sum (cdr expr) var))
          (else (raise "unsupported expr" expr))))

(trace deriv)
(display (deriv '(+ x 3) 'x))
