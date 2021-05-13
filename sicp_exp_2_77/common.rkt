#lang racket

(require
 "op_table.rkt"
 )

(provide attach-tag get-tag get-content apply-generic)

(define (attach-tag tag contents) (cons tag contents))

(define (get-tag data)
  (if (pair? data)
      (car data)
      (error "BAD TAG FORMAT: TAG")))

(define (get-content data)
  (if (pair? data)
      (cdr data)
      (error "BAD TAG FORMAT: CONTENTS")))

(define (apply-generic op . args)
  (let* ([tags (map get-tag args)]
         [proc (op-get op tags)]
         )
    (if proc
        (apply proc args)
        (error "apply-generic::No methods for these types" (list op tags))
        )))
