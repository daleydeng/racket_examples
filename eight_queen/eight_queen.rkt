#lang racket

(require racket/trace)

(define (make-coord x y)
  (cons x y))

(define (coord-x coord)
  (car coord))

(define (coord-y coord)
  (cdr coord))

(define (make-cand-coords n size)
  (for/list ([x (in-range size)]) (make-coord x n)))

(define (!= a b) (not (= a b)))
(define (ne a b) (not (equal? a b)))

(define (filter-cands-by-one cand_coords queen)
  (define (no-conflict? c)
    (and (!= (coord-x c) (coord-x queen))
         (!= (coord-y c) (coord-y queen))
         (!= (- (coord-x c) (coord-x queen))
             (- (coord-y c) (coord-y queen)))
         (!= (- (coord-x queen) (coord-x c))
             (- (coord-y c) (coord-y queen)))))
  (if (empty? cand_coords) '()
      (filter no-conflict? cand_coords)))

(define (filter-cands-by-list cand_coords queens)
  (if (empty? queens)
      cand_coords
      (filter-cands-by-list
       (filter-cands-by-one cand_coords (car queens))
       (cdr queens))))

(define (get-avail-coords queens size)
  (let* ([n (length queens)]
         [cand_coords (make-cand-coords n size)])
    (filter-cands-by-list cand_coords queens)
    ))

(define (expand-queens queens size)
  (if (= (length queens) size)
      queens
      (let* ([avail_coords (get-avail-coords queens size)])
        (if (empty? avail_coords)
            '()
            (let ([result (for/list ([c avail_coords]) (expand-queens (cons c queens) size))])
              (filter (lambda (x) (ne x '())) result))))))

(define (flatten-sol lst)
  (define (sol? v) (number? (caar v)))
  (cond [(empty? lst) '()]
        [(number? lst) lst]
        [(sol? lst) `(lst)]
        [(sol? (car lst)) (cons (car lst) (flatten-sol (cdr lst)))]
        [else (append (flatten-sol (car lst)) (flatten-sol (cdr lst)))]
        ))

(define (solve-queen size)
  (flatten-sol (for/list ([x (in-range size)])
                 (expand-queens (list (make-coord x 0)) size))))

(let ([result (solve-queen 8)])
  (length result))

; some unit test
; (expand-queens '((0 . 0)) 8)
;; (define (solve-queen size)
;;   (filter (lambda (x) (list? x)) (expand-queens '((0 . 0)) 8)))

; (solve-queen 8)
;; (filter-cands-by-one (make-cand-coords 3) `(0 . 0))
;; (filter-cands-by-list (make-cand-coords 3) `((0 . 0) (2 . 0)))
;; (get-avail-coords '((0 . 0)) 8)

;; (trace filter-coords)
;; (filter-coords (make-cand-coords 3) (make-coord 0 0))
