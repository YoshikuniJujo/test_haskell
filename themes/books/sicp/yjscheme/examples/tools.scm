(define true #t)
(define false #f)
(define nil ())
(define pi 3.141592653589793)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (average x y) (/ (+ x y) 2))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
