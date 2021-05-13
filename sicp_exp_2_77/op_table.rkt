#lang racket

(provide op-get op-put! op-del!)

(define (make-op-table)
  (let ([ht (make-hash)])
    (define (lookup k1 k2)
      (hash-ref ht (cons k1 k2) #f))

    (define (insert! k1 k2 value)
      (hash-set! ht (cons k1 k2) value))

    (define (delete! k1 k2)
      (hash-remove! ht (cons k1 k2)))

    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [(eq? m 'delete-proc!) delete!]
            [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define op-tbl (make-op-table))
(define op-get (op-tbl 'lookup-proc))
(define op-put! (op-tbl 'insert-proc!))
(define op-del! (op-tbl 'delete-proc!))
